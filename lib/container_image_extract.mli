(** Fetching of base images as .tar.gz archives *)

module Make (_ : sig
    val fs : Eio.Fs.dir_ty Eio.Path.t
    val net : [ `Generic ] Eio.Net.ty Eio.Net.t 
    val domain_mgr : Eio.Domain_manager.ty Eio.Domain_manager.t
    val progress : bool
end) : sig
  include S.FETCHER
end
