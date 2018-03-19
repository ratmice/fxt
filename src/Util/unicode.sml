signature UNICODE =
  sig
    structure Chars :
      sig
	type word = word
	val wordSize : int
	val toLargeWord : word -> Word32.word
	val toLargeWordX : word -> Word32.word
	val fromLargeWord : Word32.word -> word
	val toLargeInt : word -> LargeInt.int
	val toLargeIntX : word -> LargeInt.int
	val fromLargeInt : LargeInt.int -> word
	val toInt : word -> int
	val toIntX : word -> int
	val fromInt : int -> word
	val orb : word * word -> word
	val xorb : word * word -> word
	val andb : word * word -> word
	val notb : word -> word
	val << : word * word -> word
	val >> : word * word -> word
	val ~>> : word * word -> word
	val + : word * word -> word
	val - : word * word -> word
	val * : word * word -> word
	val div : word * word -> word
	val mod : word * word -> word
	val compare : word * word -> order
	val > : word * word -> bool
	val >= : word * word -> bool
	val < : word * word -> bool
	val <= : word * word -> bool
	val min : word * word -> word
	val max : word * word -> word
	val scan : StringCvt.radix
	  -> (char,'a) StringCvt.reader -> (word,'a) StringCvt.reader
	val fromString : string -> word option
	val fmt : StringCvt.radix -> word -> string
	val toString : word -> string
      end
    type Char = word
    type Data = Char list
    type Vector = Char Vector.vector
    val Char2String : Char -> string
    val Char2Uni : Char -> string
    val Char2char : Char -> char
    val Data2String : Data -> string
    val Data2Vector : Data -> Vector
    val Latin2String : Data -> string
    val String2Data : string -> Data
    val String2Vector : string -> Vector
    val UnicodeVectorlt : Vector * Vector -> bool
    val Vector2Data : Vector -> Data
    val Vector2String : Vector -> string
    val Vector2StringShort : Vector -> string
    val char2Char : char -> Char
    val compareChar : Char * Char -> order
    val compareData : Data * Data -> order
    val compareVector : Vector * Vector -> order
    val concatVectors : Vector * Vector -> Vector
    val hashChar : Char -> word
    val hashData : Data -> word
    val hashVector : Vector -> word
    val nullData : Data
    val nullVector : Vector
    val quoteChar : Char -> Char -> string
    val quoteData : Char -> Data -> string
    val quoteUni : Char -> string -> string
    val quoteVector : Char -> Vector -> string
  end


structure Unicode : UNICODE =
  struct
    open UniChar

    val Vector2StringShort = Vector2String      

    val Vector2String = Util.myVector2String

    fun concatVectors (v1,v2) = 
      Data2Vector (List.concat [Vector2Data v1,Vector2Data v2])

    fun UnicodeVectorlt (v1:Vector,v2:Vector) =
      let
	val l1 = Vector.length v1
	val l2 = Vector.length v2
	fun doit i =
	  if (i < l1) then
	    if (i < l2) then
	      if Vector.sub (v1,i) < Vector.sub (v2,i) then
		true
	      else
		if Vector.sub (v1,i) > Vector.sub (v2,i) then
		  false
		else
		  doit (i+1)
	    else false
	  else if l1=l2 then false
	       else true
      in
	doit 0
      end
  end
