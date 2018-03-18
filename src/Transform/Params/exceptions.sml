structure Exceptions =
  struct
    exception OtherError of string
    exception SystemCommand of string
    exception InternalError
    exception ParseError
    exception SpecificationError of Errors.Position * string
    exception CompileError of string
    exception RuntimeError of string
    exception NoSuchKey
    exception NoSuchTable of string
    exception Arguments of int
  end