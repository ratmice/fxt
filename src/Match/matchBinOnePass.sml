structure MatchBinOnePass = 
  struct
    open DocDtd Errors IntLists Fdm MatchUtil GrepOptions 
      MatchOptions MatchOutput
      Transitions

    fun match pre (noNodes,f) dtd =
      let
	val (yVec,sigVec,rhsVec,tpVec,otherRules,otherY0s,q0,
	     doMatch,
	     ys_of_y,regExpNameRules,couldMatch,couldMatchAfterTag,
	     couldMatchAfterAtt,postForm) = 
	  pre

	val (q0,down,downRegExpNames,up,
	     upRegExpNames,side,sideMatchInfo,text,
	     mightMatch,mightMatchAfterTag,mightMatchAfterAtts,
	     matches,elDictLookup,deltaA,transOnePass,debug) =
	  getTransitions pre

	fun pat_of_y y = let val (_,_,_,_,num) = Vector.sub(yVec,y) in num end

	(* The match table contains an entry for each node in the
	input. The first element is the node itself. The second is a
	map with an entry for each of the primary patterns
	matched. Each of these entries is a map with an entry for each
	ordinal number of corresponding secondaries matched. For each
	such ordinal number, the set of indexes corresponding to
	secondary matches is remembered. The third element is the
	 list of indices of sons. *)
	val matchTable = Array.array (noNodes,IntListMap.empty)
	val treeTable = Array.array (noNodes,(Fdm.emptyTree,nil))

	val secUnion = IntListMap.unionWith IntSet.union
	val multiSideMatchInfo = sideMatchInfo (matchMapTransition secUnion)

	(*--------------------------------------------------------------*)
        (* j = current node number                                      *)
	(* q = current state                                            *)
	(* f = current forest                                           *)
	(*--------------------------------------------------------------*)
	fun deltaF (j,q,f) =
	  let 
	    val len = Vector.length f
	    fun doit (j,dict,q,i,siblings) = 
	      if i>=len 
		then (j,dict,q,(rev siblings))
	      else 
		let
		  val t = Vector.sub (f,i)
		  val (j1,dictT,p,sons) = deltaT (j+1,q,t)
		  val _ = Array.update (treeTable,j,(t,sons))
		  val (dict1,q1) = multiSideMatchInfo (dict,q,p)

		  (* dict are approximately the information associated
		  with a forest state. This is a map with an entry for
		  every y in the state. For every y the set of indices
		  of the carried primaries, and a map containing
		  secondaries is remembered. The carried secondaries
		  are a map containing entries of the form (ordinal
		  number of secondaries,set of indices of secondaries)*)

		  val dictT1 = matchMapTransition secUnion (transOnePass q1) dictT 
		  val dict = (* perform the Cartesian product *)
		    MatchBin.multiMatchMapUnion matchTable (dict1,dictT1) 
		    secUnion Array.sub Array.update pat_of_y
		  val targets = matches q1
		  val dict = 
		    Vector.foldli 
		    (fn (patNum,(m,yTargets,yTargets'),dict) =>
		     let
		       val dict =  
			 if m then
			   MatchBin.multiAddPrimaryMatch yTargets matchTable dict j patNum
			 else dict
		       val dict = 
			 MatchBin.multiAddSecondaryMatches yTargets' matchTable dict j patNum
		     in
		       dict
		     end) dict (targets,0,NONE)
		in 
		  doit (j1,dict,q1,i+1,j::siblings)
		end
	  in 
	    doit (j,IntListMap.empty,q,0,nil)
	  end
	and deltaT (j,q,t) =
	  case t of 
	    TEXT txt => (j,IntListMap.empty,text (q,txt),nil)
	  | PI (target,cont) => 
	      let 
		val q0 = down (piIdx,q)
		val qa0 = down (attrsIdx,q0)
		val qa1 = deltaA (qa0,Vector.fromList[(targetIdxE,target)])
		val pa = up (attrsIdx,qa1)
		val q1 = side (q0,pa)
		val qc0 = down (contentIdx,q1)
		val (j,dict,qc1,sons) = deltaF (j,qc0,cont)
		val pc = up (contentIdx,qc1)
		val q2 = side (q1,pc)
		val dict = matchMapTransition secUnion (transOnePass q2) dict
	      in 
		(j,dict,up(piIdx,q2),sons)
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
		val (j,dict,qc1,sons) = deltaF (j,qc0,cont)
		val pc = up (contentIdx,qc1)
		val q2 = side (q1,pc)
		val dict = matchMapTransition secUnion (transOnePass q2) dict
	      in 
		(j,dict,upRegExpNames (a,q2,regExpRules),sons)
	      end

	val (j,dict,q,siblings) = deltaF (0,q0,f)

	val matchTable = 
	  Array.tabulate
	  (noNodes,
	   (fn i =>
	    let
	      val (tree,sons) = Array.sub (treeTable,i)
	      val patNumMap = Array.sub (matchTable,i)
	    in
	      (tree,patNumMap,sons)
	    end))


	(*val _ = MatchBin.printMultiMatchTable matchTable*)

	(*---------------------------------------------------*)
	(* possibly do some debugging output                 *)
	(*---------------------------------------------------*)
	val _ = debug ()
      in 
	(siblings,matchTable)
      end
  end