structure Response: MESSAGE_STRUCT =
struct
  datatype err_code =
    ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerNotinitialized
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
     | ServerNotinitialized => ~32002
     | UnknownErrorCode => ~32001
     | RequestFailed => ~32803
     | ServerCancelled => ~32802
     | ContentModified => ~32801
     | RequestCancelled => ~32800

  datatype result = Ok of string | Err of {code: err_code, message: string}
end
