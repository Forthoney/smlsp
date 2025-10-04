structure Server =
struct
  fun run (instrm, outstrm) =
    let
      val log = TextIO.openOut "/home/castlehoney/repos/smlsp/log.txt"
      fun loop () =
        let val _ = Request.decode instrm
        in loop ()
        end

      val _ = 
        case Request.decode instrm of
          {body = (id, Request.Initialize _), ...} =>
            Response.encode outstrm (id, Response.Result Response.initialize)
        | _ => raise Fail "hi"
      val _ = TextIO.output (log, "sent reply\n")
    in
      loop ()
    end
end
