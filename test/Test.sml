fun runTest name f =
  if not (f ()) then
    raise Fail name
  else
    ()

(* val _ = runTest "basic encode" *)
  (* (fn () => Message.encode "hi" = "Content-Length: 2\r\n\r\nhi") *)

val _ = runTest "basic decode"
  (fn () =>
    let
      val req = Request.decode (TextIO.openString "Content-Length: 34\r\n\r\n{\"jsonrpc\": \"2.0\", \"method\": \"hi\"}")
    in
      case req of
        ({contentLength = 34, contentType = "application/vscode-jsonrpc; charset=utf-8"}, {method = "hi", jsonrpc = (2, 0)}) => true
      | _ => false
    end)

val _ = runTest "decode invalid jsonrpc version"
  (fn () =>
    (Request.decode (TextIO.openString "Content-Length: 34\r\n\r\n{\"jsonrpc\": \"2.2\", \"method\": \"hi\"}"); false)
    handle Request.Field "jsonrpc" => true)

val _ = runTest "decode invalid jsonrpc version"
  (fn () =>
    (Request.decode (TextIO.openString "Content-Length: 34\r\nfoobar: 10\r\n\r\n{\"jsonrpc\": \"2.2\", \"method\": \"hi\"}"); false)
    handle Request.Field "foobar" => true)
