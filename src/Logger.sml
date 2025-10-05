structure Logger = DynamicLoggerFn(
  struct
    val out = TextIO.openOut "smlsp-log.txt"
    val dateFmt = "%Y-%m-%dT%H:%M:%S"
    val postProcess = fn s => s
  end
)

val _ = Logger.setLevel Level.DEBUG
