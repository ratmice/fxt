signature DOC =
  sig
    type Doc 

    val matchFile  : MatchData.Passes -> string -> Doc list
    val matchFdm : MatchData.Passes -> Fdm.Forest -> Doc list
    val docSons : Doc -> Doc list
    val docTree : Doc -> Fdm.Tree

    val matchedPattern : Doc -> int

    val docMap : (Doc -> Fdm.Forest ) -> Doc list -> Fdm.Forest
    val docMapFilter : (Doc -> Fdm.Forest) -> (Fdm.Tree -> bool) -> Doc list -> Fdm.Forest

    (*val dynamicSelect : UniChar.Vector -> Doc -> Doc list*)
    (* the dynamic select is probably only sensitive to provide if
     only unary queries are  available *)
  end


