sig
  type Char = word
  type Data = Char list
  type Vector = Char Vector.vector

  val nullData : Data
  val nullVector : Vector

  val Vector2String : Vector -> string
  val String2Vector : string -> Vector
  val UnicodeVectorlt : Vector * Vector -> bool
  val concatVectors : Vector * Vector -> Vector

  val Data2String : Data -> string
  val Data2Vector : Data -> Vector
  val String2Data : string -> Data
  val Vector2Data : Vector -> Data
  val Vector2StringShort : Vector -> string

  val Char2Uni : Char -> string
  val Char2String : Char -> string
  val Char2char : Char -> char
  val char2Char : char -> Char
  val compareChar : Char * Char -> order
end