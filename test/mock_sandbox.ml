type t = {
  expect :
    (cancelled:unit Lwt.t ->
     ?stdin:Obuilder.Os.unix_fd ->
     log:Obuilder.Build_log.t ->
     Obuilder.Config.t ->
     string ->
     (unit, [`Msg of string | `Cancelled]) Lwt_result.t) Queue.t;
}

let expect t x = Queue.add x t.expect

let run ~cancelled ?stdin ~log t (config:Obuilder.Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) (match config.argv with `Run args -> args | `Terminal -> assert false)
  | Some fn ->
    Lwt.catch
      (fun () -> fn ~cancelled ?stdin ~log config dir)
      (function
        | Failure ex -> Lwt_result.fail (`Msg ex)
        | ex -> Lwt_result.fail (`Msg (Printexc.to_string ex))
      )

let shell 
  ~cancelled:_
  ?stdin:_
  ?unix_sock:_
  _t _conf _s = assert false

let create () = { expect = Queue.create () }

let finished () = Lwt.return ()
