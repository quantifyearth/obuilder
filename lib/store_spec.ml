(** Configuration information to set up a store. *)

open Lwt.Infix
open Sexplib.Conv

type t = [
  | `Btrfs of string  (* Path *)
  | `Zfs of (string option * string * bool)    (* Path with pool at end, optional sub-directory path and whether the pool should be in the file paths *)
  | `Rsync of (string * Rsync_store.mode)  (* Path for the root of the store *)
  | `Xfs of string    (* Path *)
  | `Docker of string (* Path *)
][@@deriving sexp]

let is_absolute path = not (Filename.is_relative path)

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", pool) -> (
    (* zfs:<prefix/pool>:<|subdir|> *)
    match Astring.String.cut pool ~sep:":" with
    | Some (pool, subdir) -> Ok (`Zfs (Some subdir, pool))
    | None -> Ok (`Zfs (None, pool))
  )
  | Some ("btrfs", path) when is_absolute path -> Ok (`Btrfs path)
  | Some ("rsync", path) when is_absolute path -> Ok (`Rsync path)
  | Some ("xfs", path) when is_absolute path -> Ok (`Xfs path)
  | Some ("docker", path) -> Ok (`Docker path)
  | _ -> Error (`Msg "Store must start with zfs:, btrfs:/, rsync:/ or xfs:/")

let pp f = function
  | `Zfs (None, path) -> Fmt.pf f "zfs:%s" path
  | `Zfs (Some subdir, path) -> Fmt.pf f "zfs:%s:%s" path subdir
  | `Btrfs path -> Fmt.pf f "btrfs:%s" path
  | `Rsync path -> Fmt.pf f "rsync:%s" path
  | `Xfs path -> Fmt.pf f "xfs:%s" path
  | `Docker path -> Fmt.pf f "docker:%s" path

type store = Store : (module S.STORE with type t = 'a) * 'a -> store

let to_store = function
  | `Btrfs path ->
    `Native, Btrfs_store.create path >|= fun store ->
    Store ((module Btrfs_store), store)
  | `Zfs (subdir, path, path_without_pool) ->
    `Native, Zfs_store.create ~path_with_pool:(not path_without_pool) ?subdir ~path >|= fun store ->
    Store ((module Zfs_store), store)
  | `Rsync (path, rsync_mode) ->
    `Native, Rsync_store.create ~path ~mode:rsync_mode () >|= fun store ->
    Store ((module Rsync_store), store)
  | `Xfs path ->
    `Native, Xfs_store.create ~path >|= fun store ->
    Store ((module Xfs_store), store)
  | `Docker path ->
    `Docker, Docker_store.create path >|= fun store ->
    Store ((module Docker_store), store)

open Cmdliner

let store_t = Arg.conv (of_string, pp)

let store ?docs names =
  Arg.opt Arg.(some store_t) None @@
  Arg.info
    ~doc:"$(docv) must be one of $(b,btrfs:/path), $(b,rsync:/path), $(b,xfs:/path), $(b,zfs:pool) or $(b,docker:path) for the OBuilder cache."
    ~docv:"STORE"
    ?docs
    names

let rsync_mode_opt =
  let options =
    [("copy", Rsync_store.Copy);
     ("hardlink", Rsync_store.Hardlink);
     ("hardlink_unsafe", Rsync_store.Hardlink_unsafe)]
  in
  Arg.opt Arg.(some (enum options)) None @@
    Arg.info
      ~doc:(Printf.sprintf "Optimize for speed or low disk usage. $(docv) must be %s."
              (Arg.doc_alts_enum options))
      ~docv:"RSYNC_MODE"
      ~docs:"RSYNC STORE"
      ["rsync-mode"]

let zfs_path_without_pool =
  Arg.flag @@
  Arg.info ~doc:"Whether the ZFS file paths should include the pool name at the start or not"
  ~docv:"ZFS_PATH_WITHOUT_POOL"
  [ "zfs-path-without-pool" ]

let rsync_mode =
  Arg.value @@ rsync_mode_opt

(** Transform a [store] and [rsync-mode] into a validated combination.

    For example an rsync store must supply an rsync-mode.
 *)
let of_t store rsync_mode zfs_path_without_pool =
  match store, rsync_mode with
  | Some (`Rsync path), Some rsync_mode -> `Rsync (path, rsync_mode)
  | Some (`Rsync _path), None -> failwith "Store rsync:/ must supply an rsync-mode"
  | Some (`Btrfs path), None -> (`Btrfs path)
  | Some (`Zfs (path, subdir)), None -> (`Zfs (path, subdir, zfs_path_without_pool))
  | Some (`Xfs path), None -> (`Xfs path)
  | Some (`Docker path), None -> (`Docker path)
  | _, _ -> failwith "Store type required must be one of btrfs:/path, rsync:/path, xfs:/path, zfs:pool or docker:path for the OBuilder cache."

(** Parse cli arguments for t *)
let v =
  Term.(const of_t
        $ Arg.value @@ store ["store"]
        $ Arg.value @@ rsync_mode_opt
        $ Arg.value @@ zfs_path_without_pool)

(** Parse cli arguments for t and initialise a [store]. *)
let cmdliner =
  Term.(const to_store $ v)
