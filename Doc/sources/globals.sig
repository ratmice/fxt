sig
  type 'a Stack
  val isEmpty : 'a Stack -> bool
  val push : 'a Stack -> 'a -> unit
  val pop : 'a Stack -> 'a
  val get : 'a Stack -> 'a
  val set : 'a Stack -> 'a -> unit

  val int2Forest : int Stack -> Fdm.Forest
  val bool2Forest : bool Stack -> Fdm.Forest
  val real2Forest : real Stack -> Fdm.Forest
  val string2Forest : string Stack -> Fdm.Forest
  val Tree2Forest : Fdm.Tree Stack -> Fdm.Forest
  val Forest2Forest : Fdm.Forest Stack -> Fdm.Forest
end
