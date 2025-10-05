structure Response =
struct
  datatype err_code =
    ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerNotInitialized
  | UnknownErrorCode
  | RequestFailed
  | ServerCancelled
  | ContentModified
  | RequestCancelled

  val errToInt =
    fn ParseError => ~32700
     | InvalidRequest => ~32600
     | MethodNotFound => ~32601
     | InvalidParams => ~32602
     | InternalError => ~32603
     | ServerNotInitialized => ~32002
     | UnknownErrorCode => ~32001
     | RequestFailed => ~32803
     | ServerCancelled => ~32802
     | ContentModified => ~32801
     | RequestCancelled => ~32800

  datatype result =
    Initialize of
      { capabilities: string list
      , serverInfo: {name: string, version: string option}
      }

  datatype outcome =
    Result of result
  | Error of {code: err_code, message: string}
  (* For rare cases where there is nothing to report besides an acknowledgement.
     One such example is a response to a shutdown request that does not encounter any issues *)
  | Nothing

  val initialize =
    Initialize
      {capabilities = [], serverInfo = {name = "smlsp", version = NONE}}

  fun serializeResult (Initialize {capabilities, serverInfo = {name, version}}) =
    let
      val version =
        case version of
          SOME v => [("version", JSON.STRING v)]
        | NONE => []
    in
      JSON.OBJECT
        [ ("capabilities", JSON.OBJECT [])
        , ("serverInfo", JSON.OBJECT (("name", JSON.STRING name) :: version))
        ]
    end

  val rec jsonSize =
    fn JSON.OBJECT cs =>
      List.foldl
        (fn ((name, v), acc) => acc + String.size name + 2 + 1 + jsonSize v) 2
        cs + Int.max (length cs - 1, 0)
     | JSON.ARRAY cs =>
      List.foldl (fn (c, acc) => jsonSize c + acc + 1) 2 cs
      + Int.max (length cs - 1, 0)
     | JSON.NULL => 4
     | JSON.BOOL true => 4
     | JSON.BOOL false => 5
     | JSON.INT i => String.size (LargeInt.toString i)
     | JSON.FLOAT f => String.size (Real.fmt (StringCvt.GEN (SOME 17)) f)
     | JSON.STRING s => String.size s + 2

  fun encode strm (id, outcome) =
    let
      val body =
        case outcome of
          Result res => ("result", serializeResult res)
        | Error {code, message} =>
            ( "error"
            , JSON.OBJECT
                [ ("code", JSON.INT (Int.toLarge (errToInt code)))
                , ("message", JSON.STRING message)
                ]
            )
        | Nothing => ("result", JSON.NULL)
      val payload = JSON.OBJECT
        [ ("jsonrpc", JSON.STRING "2.0")
        , ("id", Option.getOpt (Option.map (JSON.INT o Int.toLarge) id, JSON.NULL))
        , body
        ]
    in
      ( TextIO.output
          ( strm
          , "Content-Length: " ^ Int.toString (jsonSize payload) ^ "\r\n\r\n"
          )
      ; JSONPrinter.print (strm, payload)
      ; TextIO.flushOut strm
      )
    end
end
