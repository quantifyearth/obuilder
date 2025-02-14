(* This store will work with any file system which supports reflinks. *)
open Lwt.Infix

type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;
}

type t = {
  path : string;
  caches : (string, cache) Hashtbl.t;
  mutable next : int;
}

let ( / ) = Filename.concat

module Xfs = struct
  let create dir = Lwt.return @@ Os.ensure_dir dir

  let delete dir =
    Os.sudo [ "rm"; "-r"; dir ]

  let cp ~src ~dst =
    Os.sudo [ "cp"; "-pRduT"; "--reflink=always"; src; dst ]

  let rename ~src ~dst =
    Os.sudo [ "mv"; src; dst ]
end

module Path = struct
  let state_dirname = "state"
  let cache_dirname = "cache"
  let cache_tmp_dirname = "cache-tmp"

  let result_dirname = "result"
  let result_tmp_dirname = "result-tmp"

  let dirs root =
    List.map ((/) root)
      [ state_dirname; cache_dirname; cache_tmp_dirname; result_dirname; result_tmp_dirname ]

  let result t id = t.path / result_dirname / id
  let cache t id = t.path / cache_dirname / id

  let cache_tmp t n id = t.path / cache_tmp_dirname / Printf.sprintf "%i-%s" n id

  let result_tmp t id = t.path / result_tmp_dirname / id
end

let root t = t.path

let df t = Lwt.return (Os.free_space_percent t.path)

let create ~path =
  Xfs.create path >>= fun () ->
  Lwt_list.iter_s Xfs.create (Path.dirs path) >|= fun () ->
  { path; caches = Hashtbl.create 10; next = 0 }

let build t ?base ~id ~meta:_ fn =
  Log.debug (fun f -> f "xfs: build %S" id);
  let result = Path.result t id in
  let result_tmp = Path.result_tmp t id in
  let base = Option.map (Path.result t) base in
  begin match base with
  | None -> Xfs.create result_tmp
  | Some src -> Xfs.cp ~src ~dst:result_tmp
  end
  >>= fun () ->
  Lwt.try_bind
    (fun () -> fn result_tmp)
    (fun r ->
      begin match r with
      | Ok () -> Xfs.rename ~src:result_tmp ~dst:result
      | Error _ -> Xfs.delete result_tmp
      end >>= fun () ->
      Lwt.return r
    )
    (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      Xfs.delete result_tmp >>= fun () ->
      Lwt.fail ex
    )

let delete t id =
  let path = Path.result t id in
  match Os.check_dir path with
  | `Present -> Xfs.delete path
  | `Missing -> Lwt.return_unit

let result t id =
  let dir = Path.result t id in
  match Os.check_dir dir with
  | `Present -> Lwt.return_some dir
  | `Missing -> Lwt.return_none

let log_file t id =
  result t id >|= function
  | Some dir -> dir / "log"
  | None -> (Path.result_tmp t id) / "log"

let failed t id =
  result t id >|= function
  | Some dir -> dir / "failed"
  | None -> (Path.result_tmp t id) / "failed"

let state_dir t = t.path / Path.state_dirname

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let tmp = Path.cache_tmp t t.next name in
  t.next <- t.next + 1;
  let snapshot = Path.cache t name in
  (* Create cache if it doesn't already exist. *)
  begin match Os.check_dir snapshot with
    | `Missing -> Xfs.create snapshot >>= fun () ->
      let { Obuilder_spec.uid; gid } = match user with
        | `Unix user -> user
        | `Windows _ -> assert false (* xfs not supported on Windows *)
      in
      Os.sudo [ "chown"; Printf.sprintf "%d:%d" uid gid; snapshot ]
    | `Present -> Lwt.return_unit
  end >>= fun () ->
  (* Create writeable clone. *)
  let gen = cache.gen in
  Xfs.cp ~src:snapshot ~dst:tmp >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    begin
      if cache.gen = gen then (
        (* The cache hasn't changed since we cloned it. Update it. *)
        (* todo: check if it has actually changed. *)
        cache.gen <- cache.gen + 1;
        Xfs.delete snapshot >>= fun () ->
        Xfs.rename ~src:tmp ~dst:snapshot
      ) else 
        Xfs.delete tmp
    end
  in
  Lwt.return (tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Path.cache t name in
  if Sys.file_exists snapshot then (
    Xfs.delete snapshot >>= fun () ->
    Lwt_result.return ()
  ) else Lwt_result.return ()

let complete_deletes _t = Lwt.return_unit

let get_meta _t _id _key = failwith "Meta data not supported"
