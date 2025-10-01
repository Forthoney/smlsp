fun runTest name f =
  if not (f ()) then
    raise Fail name
  else
    ()

val _ = runTest "basic encode"
  (fn () => Message.encode "hi" = "Content-Length: 2\r\n\r\nhi")

val _ = runTest "basic decode"
  (fn () =>
    let
      val req = Message.decode (TextIO.openString "Content-Length: 14\r\n\r\n{method: \"hi\"}")
    in
      case req of
        {header = {contentLength = 14, contentType = "application/vscode-jsonrpc; charset=utf-8"}, method = "hi"} => true
      | _ => false
    end)
