structure Request =
struct
  exception Parse
  exception EndOfStream
  exception Field of string

  fun |> (x, f) = f x
  infix |>
  structure JD = JSONDecode

  datatype method =
    Initialize of
      { processId: int option
      , clientInfo: {name: string, version: string option} option
      , locale: string option
      }

  fun validate jsonrpc id method =
    let
      val _ = if jsonrpc = "2.0" then () else raise Field "jsonrpc"
      val specialize =
        case method of
          "initialize" =>
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
        | "initialized" => raise Fail "initialized"
        | _ => raise Fail ("unsupported method " ^ method)
    in
      specialize |> JD.map (fn param => (id, param)) |> JD.decode
    end

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
        (NONE, _) => raise Field "Content-Length"
      | (SOME length, ty) =>
          { contentLength = length
          , contentType = Option.getOpt
              (ty, "application/vscode-jsonrpc; charset=utf-8")
          }
    end

  fun decode strm =
    let
      val header as {contentLength, ...} = decodeHeader strm
      val body = TextIO.inputN (strm, contentLength)
      val bodyDecoder =
        JD.succeed validate
        |> JD.reqField "jsonrpc" JD.string
        |> JD.reqField "id" JD.int |> JD.reqField "method" JD.string
        |> JD.reqField "params" JD.raw
    in
      {header = header, body = JD.decodeString bodyDecoder body}
      handle
        JD.JSONError (JD.FieldNotFound f, _) => raise Field f
      | JD.JSONError _ => raise Field ""
    end
end
