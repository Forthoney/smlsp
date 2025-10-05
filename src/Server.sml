structure Server =
struct
  structure In = Incoming
  structure Res = Response

  exception BadExit

  fun safeReceive (instrm, outstrm) =
    let
      fun sendErr e =
        (Res.encode outstrm (NONE, Res.Error e); safeReceive (instrm, outstrm))
    in
      In.decode instrm
      handle
        In.Parse => sendErr {code = Res.ParseError, message = ""}
      | In.Field s => sendErr {code = Res.InvalidParams, message = s}
      | In.Method m => sendErr {code = Res.MethodNotFound, message = m}
      | In.EndOfStream => raise In.EndOfStream
      | other => sendErr {code = Res.InternalError, message = exnMessage other}
    end

  fun run (strm as (instrm, outstrm)) =
    let
      val send = Res.encode outstrm
      val recv = #body o safeReceive

      fun initialize () =
        case recv strm of
          In.Request (id, In.Initialize _) =>
            send (SOME id, Res.Result Res.initialize)
        | In.Notification In.Exit => (Logger.warn "received exit in initialize mode"; raise BadExit)
        | In.Notification _ => initialize ()
        | In.Request (id, _) =>
            ( send
                ( SOME id
                , Res.Error {code = Res.ServerNotInitialized, message = ""}
                )
            ; initialize ()
            )

      fun shutdown () =
        case recv strm of
          In.Notification In.Exit => Logger.debug "received exit in shutdown mode"
        | In.Notification _ => shutdown ()
        | In.Request (id, _) =>
            ( send
                ( SOME id
                , Res.Error {code = Res.InvalidRequest, message = "in shutdown"}
                )
            ; shutdown ()
            )

      fun loop () =
        case recv strm of
          In.Request (id, In.Initialize _) =>
            ( send (SOME id, Res.Error
                { code = Res.InvalidRequest
                , message = "initialized request already received"
                })
            ; loop ()
            )
        | In.Request (id, In.Shutdown) => send (SOME id, Res.Nothing)
        | In.Notification In.Exit =>
          (Logger.warn "received exit in loop mode"; raise BadExit)
        | In.Notification _ => loop ()
    in
      ( initialize ()
      ; Logger.info "initialized"
      ; loop ()
      ; Logger.info "shutting down"
      ; shutdown ()
      )
    end
end
