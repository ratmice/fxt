structure MatchSimpleTwoPasses = 
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
	     debugArg,down,side,_,debugBlg) = 
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

	val matchTable = Array.array (noNodes,(Fdm.emptyTree,IntLists.emptyIntList,nil))

	local 
	  (*--------------------------------------------------------------*)
	  (* matches q qr =                                               *) 
	  (*      q cap qr cap { y | (y1,x,y)in delta, x in Xo } <> 0     *)
	  (*--------------------------------------------------------------*)
	  val matchDict = IntDict.makeDict 
	    ("match",!O_TS_MATCH,nil)

	  fun matches qq =
	    case IntDict.hasIndex(matchDict,qq) of 
	      SOME i => IntDict.getByIndex(matchDict,i)
	    | NONE => 
		let
		  val ys = getQstate qq
		  val match = 
		    Vector.foldli
		    (fn (i,(formula,_),nums) => 
		     let
		       val isPrimaryMatch = 
			 Formula.eval (fn ys1 => capIntLists(ys,ys1)<>nil) formula
		     in
		       if isPrimaryMatch then addIntList(i,nums) (* the first is 0 *)
		       else nums
		     end) emptyIntList
		    (formulas,0,NONE)
		in 
		  match before IntDict.setByKey (matchDict,qq,match)
		end


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
	      fun doit (j,q,i,siblings) = 
		if i >= len then (j,rev siblings)
		else 
		  let
		    val t = Vector.sub(f,i)
		    val l as LABEL(qr,p,_,_,_,_) = Vector.sub(ls,i)
		    val qq = getIntersection (q,qr)
		    val (j1,sons) = deltaT (j+1,qq,l,t)
		    val match = matches qq
		    val _ = Array.update (matchTable,j,(t,match,sons))
		    val i = i+1
		    val siblings = j::siblings
		  in
		    if i >= len then
		      (j1,rev siblings)
		    else
		      let 			  
			val q1 = side (qq,p)
		      in
			doit (j1,q1,i,siblings)
		      end
		  end
	    in 
	      doit (j,q,0,nil)
	    end
	  and deltaT (j,qq,LABEL(qr,p,pa,qr1,pc,ls),t) =
	    case t of 
	      Fdm.PI(_,cont) => 
		let 
		  val q0 = down (piIdx,qq,p)
		  val q1 = side (q0,pa)
		  val qq1 = getIntersection (q1,qr1)
		  val qc0 = down(contentIdx,qq1,pc)
		  val (j,sons) = deltaF (j,qc0,ls,cont)
		in 
		  (j,sons)
		end
	    | Fdm.ELEM(a,atts,cont) => 
		let 
		  val q0 = down (a,qq,p)
		  val q1 = side (q0,pa)
		  val qq1 = getIntersection (q1,qr1)
		  val qc0 = down(contentIdx,qq1,pc)
		  val (j,sons) = deltaF (j,qc0,ls,cont)
		in 
		  (j,sons)
		end
	    | _ => (j,nil)
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
	val (j,siblings) = runBlg (0,getQindex ql0,ls,f)

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

	val _ = debugBlg()
	
      in 
	(siblings,matchTable)
      end

  end