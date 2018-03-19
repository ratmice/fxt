structure MatchBinTwoPasses = 
  struct
    open DocDtd Errors IntLists MatchData MatchUtil GrepOptions MatchOptions
      
    val O_TS_MATCH = ref 5
    val up2Levels_ys_of_y = Match2ArgBlg.up2Levels_ys_of_y

    fun match pre (noNodes,f) dtd = 
      let
	val (sigVec,yVec,rhsVec,tpVec,
	     otherRules,otherY0s,qr0,ql0info,
	     doMatch,incomingXVec,regExpNameRules,ys_of_y,formulas) = 
	  pre : PreArgBlg.PreArgBlg 
	val (pat_of_y,getIntersection,getPstate,Fs_for_rule,
	     xs_of_p,yFs_for_p,trans_for_y,yFs_for_q,
	     getQindex,getQstate,_,_,runArg,(doText,doPi,doElem,doForest,doAttrs),
	     debugArg,down,side,sideMatchInfo,debugBlg) = 
	  ArgBlg.argBlg 
	  (sigVec,yVec,rhsVec,tpVec,
	   otherRules,otherY0s,qr0,ql0info,
	   incomingXVec,regExpNameRules,ys_of_y)
	  dtd

	fun deltaT (q,t) =
	  case t of 
	    Fdm.TEXT txt => doText q txt
	  | Fdm.PI (target,cont) => doPi q (target,cont) (doForest Vector.sub deltaT) doAttrs
	  | Fdm.ELEM (a,atts,cont) => doElem q (a,atts,cont) (doForest Vector.sub deltaT) doAttrs
	val runArg = doForest Vector.sub deltaT

	val matchTable = Array.array (noNodes,IntListMap.empty)
	val treeTable = Array.array (noNodes,(Fdm.emptyTree,nil))

	local 
	  (*--------------------------------------------------------------*)
	  (* matches q qr =                                               *) 
	  (*      q cap qr cap { y | (y1,x,y)in delta, x in Xo } <> 0     *)
	  (*--------------------------------------------------------------*)
	  val matchDict = IntDict.makeDict 
	    ("match",!O_TS_MATCH,Vector.fromList nil)

	  fun matches qq =
	    case IntDict.hasIndex(matchDict,qq) of 
	      SOME i => IntDict.getByIndex(matchDict,i)
	    | NONE => 
		let 
		  val ys = getQstate qq
		  val b =  
		    Vector.mapi
		    (fn (i,carry) => 
		     Transitions.getMatchInfo (ys,carry,Vector.sub(formulas,i)))
		    doMatch
		in 
		  b before IntDict.setByKey(matchDict,qq,b)
		end

	  val secUnion = IntListMap.unionWith IntSet.union

	  (*--------------------------------------------------------------*)
	  (* q = current state                                            *)
	  (* f = current forest                                           *)
	  (* t = current tree                                             *)
	  (* r = already accumulated matches                              *)
	  (* s = whether a subtree matches                                *)
	  (* l = label for a subtree                                      *)
	  (*--------------------------------------------------------------*)
	  fun deltaF (j,q,ls,f) =
	    let
	      val len = Vector.length f
	      fun doit (j,dict,q,i,siblings) = 
		if i >= len then (j,dict,rev siblings)
		else 
		  let
		    val t = Vector.sub(f,i)
		    val l as LABEL(qr,p,_,_,_,_) = Vector.sub(ls,i)
		    val qq = getIntersection (q,qr)
		    val (j1,dictT,sons) = deltaT (j+1,qq,l,t)
		    val _ = Array.update (treeTable,j,(t,sons))
		    val targets = matches qq
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
		       end) dict targets
		    val dict =
		      MatchBin.multiMatchMapUnion matchTable (dict,dictT) secUnion Array.sub Array.update pat_of_y
		    val i = i+1
		    val ys_of_qq = getQstate qq
		    val siblings = j::siblings
		  in
		    if i >= len then
		      (j1,(IntListMap.filteri
			  (fn (y,_) => IntLists.inIntList (y,ys_of_qq))
			  dict),rev siblings)
		    else
		      let 			  
			val (dict,q1) = sideMatchInfo secUnion (dict,qq)
		      in
			doit (j1,dict,q1,i,siblings)
		      end
		  end
	    in 
	      doit (j,IntListMap.empty,q,0,nil)
	    end
	  and deltaT (j,qq,LABEL(qr,p,pa,qr1,pc,ls),t) =
	    let
	      fun matchConjunction dict ks =		
		List.app
		(fn k =>
		 let
		   val (_,_,_,yss) = Fs_for_rule k
		 in
		   if List.length yss <= 1 then ()
		   else (*conjunction*)
		     (* when more content models have to be verified simultaneously *)
		     let
		       val pat = pat_of_y (hd (hd yss))
		       val (allSecondaries,pss) =
			 List.foldl 
			 (fn (ys,(s,pss)) =>
			  let
			    val (s,p) = 
			      List.foldl 
			      (fn (y,(s1,p1)) =>
			       case IntListMap.find (dict,y) of
				 SOME (p2,s2) => (secUnion (s1,s2),IntSet.union (p1,p2))
			       | NONE => (s1,p1)) (s,IntSet.empty) ys
			  in
			    (s,(p,s)::pss)
			  end)
			 (IntListMap.empty,nil) yss
		     in
		       List.app
		       (fn (primaries,currentSecondaries) =>
			let
			  val restSecondaries = 
			    IntListMap.foldli
			    (fn (secOrder,currentSecs,restSecondaries) =>
			     let
			       val allSecs =
				 Option.valOf (IntListMap.find (restSecondaries,secOrder))
			       val restSecs = IntSet.difference (allSecs,currentSecs)
			     in
			       if IntSet.isEmpty restSecs then
				 #1 (IntListMap.remove (restSecondaries,secOrder))
			       else
				 IntListMap.insert (restSecondaries,secOrder,restSecs)
			     end) allSecondaries currentSecondaries (*!!efficiency*)
			in
			  IntSet.app
			  (fn i =>
			   let 
			     val patNumMap = Array.sub (matchTable,i)
			     val patNumMap =
			       case IntListMap.find (patNumMap,pat) of
				 SOME ts => IntListMap.insert(patNumMap,pat,secUnion (ts,restSecondaries))
			       | NONE => IntListMap.insert(patNumMap,pat,restSecondaries)
			   in
			     Array.update (matchTable,i,patNumMap)
			   end) primaries
			end) pss
		     end
		 end) ks
	    in
	      case t of 
		Fdm.PI(_,cont) => 
		  let 
		    val q0 = down (piIdx,qq,p)
		    val q1 = side (q0,pa)
		    val qq1 = getIntersection (q1,qr1)
		    val qc0 = down(contentIdx,qq1,pc)
		    val (j,dict,sons) = deltaF (j,qc0,ls,cont)
		    val ks = getPstate pc
		    val _ = matchConjunction dict ks
		    val dict = 
		      Transitions.matchMapTransition secUnion 
		      (up2Levels_ys_of_y (ys_of_y,getQstate qq1,getQstate qq)) dict
		  in 
		    (j,dict,sons)
		  end
	      | Fdm.ELEM(a,atts,cont) => 
		  let 
		    val q0 = down (a,qq,p)
		    val q1 = side (q0,pa)
		    val qq1 = getIntersection (q1,qr1)
		    val qc0 = down(contentIdx,qq1,pc)
		    val (j,dict,sons) = deltaF (j,qc0,ls,cont)
		    val ks = getPstate pc
		    val _ = matchConjunction dict ks
		    val dict = 
		      Transitions.matchMapTransition secUnion 
		      (up2Levels_ys_of_y (ys_of_y,getQstate qq1,getQstate qq)) dict
		  in 
		    (j,dict,sons)
		  end
	      | _ => (j,IntListMap.empty,nil)
	    end
	in 
	  val runBlg = deltaF
	end
            
        val (ls,qr) = runArg (getQindex qr0,f)
	val _ = debugArg()
	val yrs = getQstate qr
	val ql0 = foldl 
	  (fn ((posFs,negF,posF),ys) 
	   => if (capIntLists(yrs,negF)<>nil orelse 
		  List.exists (fn posF => capIntLists(yrs,posF)=nil) posFs) then ys
	      else cupIntLists(ys,posF))
	  nil ql0info 
	val (j,dict,siblings) = runBlg (0,getQindex ql0,ls,f)

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

	val _ = debugBlg()
	
      in 
	(siblings,matchTable)
      end

  end
