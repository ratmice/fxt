sig
  type doc

  val makeDoc : string -> doc list
  val docSons : doc -> doc list
  val iSon : doc -> int -> doc
  val docTree : doc -> Fdm.Tree

  val matchedPattern : doc -> int

  val docMap : (doc -> Fdm.Forest ) -> doc list -> Fdm.Forest
  val docMapFilter : (doc -> Fdm.Forest) -> (Fdm.Tree -> bool) -> doc list -> Fdm.Forest 

  val selectForest : UniChar.Vector -> doc -> doc list
end