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
      { capabilities: (string * JSON.value) list
      , serverInfo: {name: string, version: string option}
      }

  datatype outcome =
    Result of result
  | Error of {code: err_code, message: string}
  (* For rare cases where there is nothing to report besides an acknowledgement.
     One such example is a response to a shutdown request that does not encounter any issues *)
  | Nothing

  val initialize = Initialize
    { capabilities = [("textDocumentSync", JSON.INT 1)]
    , serverInfo = {name = "smlsp", version = NONE}
    }

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

  (* Just copying value from 
     https://learn.microsoft.com/en-us/dotnet/api/system.text.json.jsonserializeroptions.defaultbuffersize?view=net-9.0
     Probably could do better but it's fine
  *)
  val buf = CharBuffer.new 16384 
  val printer = JSONBufferPrinter.new buf
  fun stringify v = 
    ( CharBuffer.reset buf
    ; JSONBufferPrinter.value (printer, v)
    ; (CharBuffer.length buf, CharBuffer.contents buf)
    )

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
        , ( "id"
          , Option.getOpt (Option.map (JSON.INT o Int.toLarge) id, JSON.NULL)
          )
        , body
        ]
      val (length, s) = stringify payload
      val output = fn s => TextIO.output (strm, s)
    in
      ( output "Content-Length: "
      ; output (Int.toString length)
      ; output "\r\n\r\n"
      ; output s
      ; TextIO.flushOut strm
      )
    end
end
