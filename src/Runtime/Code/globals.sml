structure Globals : GLOBALS =
  struct
    type 'a Stack = ('a list) ref
    fun isEmpty g = g = ref []
    fun push x v = x := (v::(!x))
    fun pop x = hd (!x) before x := tl (!x)
    fun get x = hd (!x)
    fun set x v = x := (v::(tl (!x)))

    val int2Forest = (fn i => vector [Fdm.text (UniChar.String2Vector (Int.toString (get i)))])
    val bool2Forest = (fn i => vector [Fdm.text (UniChar.String2Vector (Bool.toString (get i)))])
    val real2Forest = (fn i => vector [Fdm.text (UniChar.String2Vector (Real.toString (get i)))])
    val string2Forest = (fn i => vector [Fdm.text (UniChar.String2Vector (String.toString (get i)))])
    val Tree2Forest = (fn t => vector [get t])
    val Forest2Forest = (fn f => get f)
  end

