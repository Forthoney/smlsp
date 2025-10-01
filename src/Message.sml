structure Message =
struct
  exception Decode

  type header = {contentLength: int, contentType: string}
  type t = {header: header, method: string}

  fun encode s =
    "Content-Length: " ^ Int.toString (String.size s) ^ "\r\n\r\n" ^ s

  fun decodeHeader strm : header =
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
            (_, "") => raise Decode
          | ("Content-Type", value) => (length, SOME value)
          | ("Content-Length", value) =>
              case Int.fromString value of
                SOME i => (SOME i, ty)
              | NONE => raise Decode
        end

      fun loop headers =
        case TextIO.inputLine strm of
          NONE => raise Decode
        | SOME "\r\n" => headers
        | SOME s =>
            let
              val s = Substring.full s
            in
              if Substring.isSuffix "\r\n" s then loop (headerField s headers)
              else raise Fail "Non CRLF newline not permitted in header"
            end
    in
      case loop (NONE, NONE) of
        (NONE, _) => raise Decode
      | (SOME length, ty) =>
          { contentLength = length
          , contentType = Option.getOpt
              (ty, "application/vscode-jsonrpc; charset=utf-8")
          }
    end

  fun decode strm =
    let
      val header as {contentLength, contentType} = decodeHeader strm
      val body = TextIO.inputN (strm, contentLength)

      val bodyDecoder = 
        let
          fun |> (x, f) = f x
          infix |>
        in
          JSONDecode.succeed (fn method => {header = header, method = method})
          |> JSONDecode.reqField "method" JSONDecode.string
        end
    in
      JSONDecode.decodeString bodyDecoder body
    end
end
