structure MatchSimpleOnePass = 
  struct
    open DocDtd Errors IntLists Fdm MatchUtil GrepOptions 
      MatchOptions MatchOutput

    fun match pre (noNodes,f) dtd =
      let
	val (q0,down,downRegExpNames,up,
	     upRegExpNames,side,sideMatchInfo,text,
	     mightMatch,mightMatchAfterTag,mightMatchAfterAtts,
	     matches1,elDictLookup,deltaA,transOnePass,debug) =
	  Transitions.getTransitions pre

	fun matches q =
	  Vector.foldli 
	  (fn (patNum,(m,yTargets,yTargets'),nums) =>
	   if m then IntLists.addIntList (patNum,nums)
	   else nums) 
	  IntLists.emptyIntList 
	  (matches1 q)
	   
	val matchTable = Array.array (noNodes,(Fdm.emptyTree,IntLists.emptyIntList,nil))

	(*--------------------------------------------------------------*)
        (* j = current node number                                      *)
	(* q = current state                                            *)
	(* f = current forest                                           *)
	(*--------------------------------------------------------------*)
	fun deltaF (j,q,f) =
	  let 
	    val len = Vector.length f
	    fun doit (j,q,i,siblings) = 
	      if i>=len 
		then (j,q,(rev siblings))
	      else 
		let
		  val t = Vector.sub (f,i)
		  val (j1,p,sons) = deltaT (j+1,q,t)
		  val q1 = side (q,p)
		  val match = matches q1
		  val _ = Array.update (matchTable,j,(t,match,sons))
		in 
		  doit (j1,q1,i+1,j::siblings)
		end
	  in 
	    doit (j,q,0,nil)
	  end
	and deltaT (j,q,t) =
	  case t of 
	    TEXT txt => (j,text (q,txt),nil)
	  | PI (target,cont) => 
	      let 
		val q0 = down (piIdx,q)
		val qa0 = down (attrsIdx,q0)
		val qa1 = deltaA (qa0,Vector.fromList[(targetIdxE,target)])
		val pa = up (attrsIdx,qa1)
		val q1 = side (q0,pa)
		val qc0 = down (contentIdx,q1)
		val (j,qc1,sons) = deltaF (j,qc0,cont)
		val pc = up (contentIdx,qc1)
		val q2 = side (q1,pc)
	      in 
		(j,up(piIdx,q2),sons)
	      end
	  | ELEM (a,atts,cont) => 
	      let
		val (y0s_regExpRules,regExpRules) = elDictLookup (dtd,a)
		val q0 = downRegExpNames (a,q,y0s_regExpRules)
		val qa0 = down (attrsIdx,q0)
		val qa1 = deltaA (qa0,atts)
		val pa = up (attrsIdx,qa1)
		val q1 = side (q0,pa)
		val qc0 = down (contentIdx,q1)
		val (j,qc1,sons) = deltaF (j,qc0,cont)
		val pc = up (contentIdx,qc1)
		val q2 = side (q1,pc)
	      in 
		(j,upRegExpNames(a,q2,regExpRules),sons)
	      end
	and deltaA (q,atts) =
	  let val len = Vector.length atts
	    fun doit (q,i) = 
	      if i>=len then q
	      else let val (a,txt) = Vector.sub(atts,i) 
		       val qa0 = down (a,q)
		       val pa = text (qa0,txt)
		       val qa1 = side (qa0,pa)
		       val p = up (a,qa1)
		       val q1 = side (q,p)
		   in doit (q1,i+1)
		   end
	  in doit(q,0)
	  end

	val (j,q,siblings) = deltaF (0,q0,f)

	val matchTable = 
	  Array.tabulate
	  (Array.length matchTable,
	   (fn i =>
	    let
	      val (t,match,sons) = Array.sub (matchTable,i)
	      val patNumMap = 
		List.foldl 
		(fn (m,patNumMap) => IntListMap.insert (patNumMap,m,IntListMap.empty))
		IntListMap.empty match 
	    in
	      (t,patNumMap,sons)
	    end))


	(*---------------------------------------------------*)
	(* possibly do some debugging output                 *)
	(*---------------------------------------------------*)
	val _ = debug ()
      in 
	(siblings,matchTable)
      end
  end
