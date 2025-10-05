structure Server =
struct
  fun run (instrm, outstrm) =
    let
      val log = TextIO.openOut "/home/castlehoney/repos/smlsp/log.txt"
      fun loop () =
        (Incoming.decode instrm; loop ())
        handle Incoming.EndOfStream => ()
    in
      ( case Incoming.decode instrm of
          {body = (id, Request.Initialize _), ...} =>
            Response.encode outstrm (id, Response.Result Response.initialize)
        | _ => raise Fail "hi"
      ; loop ()
      )
      handle Incoming.EndOfStream => ()
    end
end
