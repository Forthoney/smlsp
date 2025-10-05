structure Incoming =
struct
  exception EndOfStream
  exception Parse
  exception Method of string
  exception Field of string

  fun |> (x, f) = f x
  infix |>
  structure JD = JSONDecode

  datatype request =
    Initialize of
      { processId: int option
      , clientInfo: {name: string, version: string option} option
      , locale: string option
      }
  | Shutdown

  datatype notification = Initialized | Exit

  datatype message = Request of int * request | Notification of notification

  fun decodeHeader strm =
    let
      fun headerField s (length, ty) =
        let
          open Substring
          val s = (trimr 2 s)
          val (name, value) = position ": " s
          val sepLength = String.size ": "
          val value = triml sepLength value
        in
          case (string name, string value) of
            (_, "") => raise Parse
          | ("Content-Type", value) => (length, SOME value)
          | ("Content-Length", value) =>
              (case Int.fromString value of
                 SOME i => (SOME i, ty)
               (* this is a failure to parse the value of the Content-Length flag, so it is a parse error *)
               | NONE => raise Parse)
          | (unknown, _) => raise Field unknown
        end

      fun loop headers =
        case TextIO.inputLine strm of
          NONE => raise EndOfStream
        | SOME "\r\n" => headers
        | SOME s =>
            let
              val s = Substring.full s
            in
              if Substring.isSuffix "\r\n" s then loop (headerField s headers)
              else raise Parse
            end
    in
      case loop (NONE, NONE) of
        (SOME length, ty) =>
        { contentLength = length
        , contentType = Option.getOpt
            (ty, "application/vscode-jsonrpc; charset=utf-8")
        }
      | (NONE, _) => raise Field "Content-Length"
    end

  structure Request =
  struct
    fun mk f =
      JD.succeed (fn id => fn params => Request (id, JD.decode f params))
      |> JD.reqField "id" JD.int
      |> JD.reqField "params" JD.raw

    val initialize =
      let
        fun pack name version = {name = name, version = version}
        fun wrap pid loc info =
          Initialize {processId = pid, locale = loc, clientInfo = info}

        val clientInfo =
          JD.succeed pack
          |> JD.reqField "name" JD.string
          |> JD.optField "version" JD.string
      in
        JD.succeed wrap
        |> JD.reqField "processId" (JD.nullable JD.int)
        |> JD.optField "locale" JD.string
        |> JD.optField "clientInfo" (JD.map (JD.decode clientInfo) JD.raw)
      end
  end

  val route =
    fn "initialize" => Request.mk Request.initialize
     | "initialized" => JD.succeed (Notification Initialized)
     | "shutdown" =>
        JD.succeed (fn id => Request (id, Shutdown)) |> JD.reqField "id" JD.int
     | "exit" => JD.succeed (Notification Exit)
     | m => raise Method m

  fun decode strm =
    let
      val header as {contentLength, ...} = decodeHeader strm
      val body = TextIO.inputN (strm, contentLength)
      val bodyDecoder =
        JD.field "jsonrpc" JD.string
        |>
        JD.andThen
          (fn "2.0" => JD.field "method" JD.string |> JD.andThen route
            | _ => raise Field "jsonrpc")
    in
      {header = header, body = JD.decodeString bodyDecoder body}
      handle
        JD.JSONError (JD.FieldNotFound f, _) => raise Field f
      | JD.JSONError _ => raise Field ""
    end
end
