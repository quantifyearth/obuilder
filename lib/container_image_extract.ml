let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let cache fs =
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let root = Eio.Path.(fs / Xdg.cache_dir xdg / "image") in
  let cache = Container_image.Cache.v root in
  Container_image.Cache.init cache;
  cache

let fetch ~net ~domain_mgr ?platform cache image no_progress =
  let client =
    Cohttp_eio.Client.make
      ~https:(Some (https ~authenticator:null_auth))
      net
  in
  let show_progress = not no_progress in
  Container_image.fetch ~show_progress ~client ~cache ~domain_mgr ?platform image

module Make (E : sig
    val fs : Eio.Fs.dir_ty Eio.Path.t
    val net : [ `Generic ] Eio.Net.ty Eio.Net.t 
    val domain_mgr : Eio.Domain_manager.ty Eio.Domain_manager.t

    val progress : bool
end) = struct

  let get_environment_variables_from_manifest ~cache v i =
    let module Ci = Container_image in
    let open Container_image_spec in
    let of_oci_manifest (docker_manifest : Manifest.OCI.t) =
      let config = Manifest.OCI.config docker_manifest in
      let config = 
        Ci.Cache.Blob.get_string cache (Descriptor.digest config)
        |> Config.of_string ~media_type:Media_type.(Docker Docker.Image_config)
      in
      match config with
      | Ok config -> Config.env config, Config.user config
      | Error (`Msg m) -> failwith m
    in
    let of_docker_manifest (docker_manifest : Manifest.Docker.t) =
      let config = Manifest.Docker.config docker_manifest in
      let config_str = 
        Ci.Cache.Blob.get_string cache (Descriptor.digest config)
      in
      let config =
        config_str |> Config.of_string ~media_type:Media_type.(Docker Docker.Image_config)
      in
      match config with
      | Ok config -> Config.env config, Config.user config
      | Error (`Msg m) -> failwith (m ^ " : " ^ config_str)
    in
    match (v : Manifest.t) with
  | `Docker_manifest_list _ -> (
      match Container_image.Util.guess_manifest v with
      | None -> failwith "No known manifest for this architecture"
      | Some descriptor ->
      let digest = Descriptor.digest descriptor in
        let manifest = 
          let img = Ci.Image.v ~digest (Ci.Image.repository i) in
          Ci.Cache.Manifest.get cache img
          |> function `Docker_manifest m -> m | _ -> assert false
        in
        of_docker_manifest manifest
    )
    | `Docker_manifest m -> of_docker_manifest m
    | `OCI_manifest oci -> of_oci_manifest oci
    | `OCI_index _ ->
      (* TODO: Which manifest? *)
      match Container_image.Util.guess_manifest v with
      | None -> failwith "No known manifest for this architecture"
      | Some descriptor ->
      let digest = Descriptor.digest descriptor in
      let manifest = 
        let img = Ci.Image.v ~digest (Ci.Image.repository i) in
        Ci.Cache.Manifest.get cache img
        |> function `OCI_manifest m -> m | _ -> assert false
      in
      of_oci_manifest manifest

  let fetch_with_eio ~log:_ ~rootfs base =
    let module Ci = Container_image in
    let open Container_image_spec in
    Logs.info (fun f -> f "Fetching %s" base);
    let cache = cache E.fs in
    match Container_image.Image.of_string base with
    | Error (`Msg m) -> failwith m
    | Ok img -> 
      fetch ~net:E.net ~domain_mgr:E.domain_mgr cache img (not E.progress);
      let root = Eio.Path.(E.fs / rootfs) in
      let manifest = Container_image.Cache.Manifest.get cache img in
      match Container_image.Util.guess_manifest manifest with
      | None -> failwith "No known manifest"
      | Some descriptor ->
      let digest = Descriptor.digest descriptor in
      let single_image = Ci.Image.with_digest digest img in 
      Ci.checkout ~only_rootfs:true ~cache ~root single_image;
      get_environment_variables_from_manifest ~cache manifest img

    let fetch ~log ~rootfs base =
      Lwt_eio.run_eio @@ fun () -> fetch_with_eio ~log ~rootfs base
end

let make_fetcher ?(progress=true) ~fs ~net domain_mgr =
  let module T = struct
    let fs = (fs :> Eio.Fs.dir_ty Eio.Path.t)
    let net = (net :> [`Generic] Eio.Net.ty Eio.Net.t)
    let domain_mgr = (domain_mgr :> Eio.Domain_manager.ty Eio.Domain_manager.t)
    let progress = progress
  end in
  let module Fetcher = Make (T) in
  (module Fetcher : S.FETCHER)