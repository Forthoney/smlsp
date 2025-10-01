structure Request =
struct
  exception Parse
  exception Field of string
  type body = {method: string, jsonrpc: int * int}

  val bodyDecoder =
    let
      fun validate "2.0" method = {method = method, jsonrpc = (2, 0)}
        | validate jsonrpc _ = raise Field "jsonrpc"
      fun |> (x, f) = f x
      infix |>
      open JSONDecode
    in
      succeed validate
      |> reqField "jsonrpc" string
      |> reqField "method" string
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
          NONE => raise Parse
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
    in
      (header, JSONDecode.decodeString bodyDecoder body)
      handle JSONDecode.JSONError (JSONDecode.FieldNotFound f, _) => raise Field f 
           | JSONDecode.JSONError _ => raise Field ""
    end
end
