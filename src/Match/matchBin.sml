structure MatchBin =
  struct
    fun printSecondariesMap sec =
      IntListMap.appi
      (fn (secOrd,sec) =>
       let
	 val _ = print ("[%"^(Int.toString secOrd)^": ")
	 val _ =
	   IntSet.app 
	   (fn s => print ((Int.toString s)^";")) sec
	 val _ = print ("]")
       in
	 ()
       end) sec
      
    fun printPatNumMap patNumMap = 
      IntListMap.appi
      (fn (pat,sec) =>
       let
	 val _ = print ("("^(Int.toString pat)^",{")
	 val _ = printSecondariesMap sec
	 val _ = print "});"
       in
	 ()
       end) patNumMap

    fun printDict dict =
      IntListMap.appi
      (fn (y,(prim,sec)) =>
       let
	 val _ = print ("("^(Int.toString y)^",")
	 val _ = print "["
	 val _ = IntSet.app (fn s => print ((Int.toString s)^";")) prim
	 val _ = print "]"
	 val _ = printSecondariesMap sec
	 val _ = print ")"
       in
	 ()
       end) dict
       

    fun printMultiMatchTable matchTable =
      Array.appi 
      (fn (i,(t,patNumMap,sons)) =>
       let
	 val _ = print ((Int.toString i)^"-->{")
	 val _ = printPatNumMap patNumMap
	 val _ = print "},"
	 val _ = print (UtilString.List2String Int.toString sons)
	 val _ = print "\n"
       in
	 ()
       end) (matchTable,0,NONE)

    fun multiMatchMapUnion matchTable (matchMap1,matchMap2) secUnion sub update pat_of_y =
      IntListMap.unionWithi
	(fn (y,(t1,secondaries1),(t2,secondaries2)) =>
	 let
	   val _ = 
	     IntSet.app
	     (fn j =>
	      let 
		val patNumMap = sub (matchTable,j)
		val i = pat_of_y y
		val patNumMap = 
		  case IntListMap.find (patNumMap,i) of
		    NONE => IntListMap.insert (patNumMap,i,secondaries2)
		  | SOME sec => IntListMap.insert (patNumMap,i,secUnion (sec,secondaries2))
		val _ = update (matchTable,j,patNumMap)
	      in
		()
	      end) t1
	   val _ = 
	     IntSet.app
	     (fn j =>
	      let 
		val patNumMap = sub (matchTable,j)
		val i = pat_of_y y
		val patNumMap = 
		  case IntListMap.find (patNumMap,i) of
		    NONE => IntListMap.insert (patNumMap,i,secondaries1)
		  | SOME sec => IntListMap.insert (patNumMap,i,secUnion (sec,secondaries1))
		val _ = update (matchTable,j,patNumMap)
	      in
		()
	      end) t2
	 in
	   (IntSet.union (t1,t2),secUnion (secondaries1,secondaries2))
	 end) (matchMap1,matchMap2)

    fun multiAddPrimaryMatch targets matchTable matchMap j patNum =
    (* patNum is the ordinal number of the matched match pattern *)
      let
	(* the current match information for the current node *)
	val patNumMap = Array.sub (matchTable,j)

	val (matchMap,patNumMap) =
	  List.foldl
	  (fn (y,(dict,patNumMap)) =>
	   let
	     (* recompute the list of primaries and secondaries
	     carried by y-s *)
	     val (primaries,carriedSecondaries) = 
	       case (IntListMap.find (dict,y))of
		 SOME (matches,matches') =>
		   (IntSet.add (matches,j),matches')
	       | NONE => 
		   (IntSet.singleton j,IntListMap.empty)
	     val matchInfo = (primaries,carriedSecondaries)

	     (* update the entry in the match table: each carried
	     secondary is a secondary for the patNum primary
	     represented by the current element*)
	     val patNumMap =
	       case IntListMap.find (patNumMap,patNum) of
		 SOME patSec => 
		   IntListMap.insert(patNumMap,patNum,
				     IntListMap.unionWith IntSet.union (patSec,carriedSecondaries))
	       | NONE => 
		   IntListMap.insert(patNumMap,patNum,carriedSecondaries)
	   in
	     (IntListMap.insert (dict,y,matchInfo),patNumMap)
	   end) (matchMap,patNumMap) targets

	val _ = Array.update (matchTable,j,patNumMap)

      in
	matchMap
      end


    fun multiAddSecondaryMatches targets matchTable matchMap j patNum =
      (* targets is a list of secondary order of secondary targets
      found, together with the positive ys which have made possible
      the match *)
      Vector.foldli
      (* for each secondary order found *)
      (fn (secOrder,ys,matchMap) =>
       (* for each positive y *)
       List.foldl 
       (fn (y,matchMap) =>
	let
	  (* find the primaries and secondaries asscociated with y;
	  update the secs *)
	  val (prim,sec) = 
	    case (IntListMap.find (matchMap,y)) of
	      SOME (matches,matches') => 
		(matches,
		 case IntListMap.find (matches',secOrder) of
		   SOME secondaries => IntListMap.insert (matches',secOrder,IntSet.add (secondaries,j))
		 | NONE => IntListMap.insert (matches',secOrder,IntSet.singleton j))
	    | NONE => 
		(IntSet.empty,
		 IntListMap.insert (IntListMap.empty,secOrder,IntSet.singleton j))
		
          (* the secondary found form a match pair with each primary, so update the match table *)
	  val _ = IntSet.app
	    (fn i =>
	     let
	       val patNumMap = Array.sub (matchTable,i)
	       val patNumMap = 
		 case IntListMap.find (patNumMap,patNum) of 
		  SOME sec' => IntListMap.insert 
		    (patNumMap,patNum,
		     case IntListMap.find (sec',secOrder) of
		       SOME secondaries => 
			 IntListMap.insert (sec',secOrder,IntSet.add (secondaries,j))
		     | NONE =>
			 IntListMap.insert (sec',secOrder,IntSet.singleton j))
		| NONE => IntListMap.insert 
		    (patNumMap,patNum,
		     IntListMap.insert (IntListMap.empty,secOrder,IntSet.singleton j))
	     in
	       Array.update (matchTable,i,patNumMap)
	     end) prim
	in
	  IntListMap.insert (matchMap,y,(prim,sec))
	end) matchMap ys) matchMap 
      (targets,0,NONE)
  end