structure Server =
struct
  fun run strm =
    let
      val log = TextIO.openOut "/home/sj4963/repos/smlsp/log.txt"
      fun loop () =
        let
          val _ = Request.decode strm
        in
          loop ()
        end
    in
      case Request.decode strm of
        (_, (_, Request.Initialize _)) =>
        (JSONPrinter.print (TextIO.stdOut, Response.initialize);
        loop ())
      | _ => raise Fail "hi"
    end
end
