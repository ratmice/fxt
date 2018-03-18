signature MultDocHooks =
   sig
      type AppData
      exception DocFail
      val docStart : DocDtd.Dtd -> AppData
   end

structure MultDocHooks0 =
   struct
      open DocDtd GrepError Encoding Errors HookData IgnoreHooks MatchData UniChar

      val THIS_MODULE = "DocHooks"

      exception DocFail

      type AppFinal = int * string * Fdm.Forest (* (numberOfNodes,encoding,tree) *)
      type AppData = 
	int * (* number of nodes *)
	Dtd * 
	string * (* encoding *)
	Fdm.Tree list * Vector list 
	* (int * AttSpc vector * Fdm.Tree list) list
	 
      fun docStart dtd = (0,dtd,"UTF-8",nil,nil,nil)

      fun hookXml((c,dtd,_,content,vecs,stack),(_,enc,opt)) =
         let val encName = case opt
                             of SOME(_,SOME encName,_) => encName
                              | _ => encodingName enc
         in (c,dtd,encName,content,vecs,stack)
         end


      fun takeData (c,content,vecs) = 
	 let 
            val vecs = rev vecs
            val vec = Vector.concat vecs
	 in 
            if Vector.length vec=0 then (c,content )
            else (c+1,Fdm.TEXT vec::content)
	 end

      fun hookStartTag ((c,dtd,enc,content,vecs,stack),(_,elem,atts,_,mt))  =
	 let 
	    val (c,content1) = takeData (c,content,vecs)
	    val attspecs = List.mapPartial 
	      (fn (i,ap,_) => case ap
	       of AP_DEFAULT(_,av,_) => SOME(DocAtt2Elem dtd i,av)
	     | AP_PRESENT(_,av,_) => SOME(DocAtt2Elem dtd i,av)
	     | _ => NONE) atts
	    val attvec = Vector.fromList attspecs
	 in if mt then (c+1,dtd,enc,Fdm.ELEM(elem,attvec,Fdm.emptyForest)::content1,nil,stack)
	    else (c+1,dtd,enc,nil,nil,(elem,attvec,content1)::stack)
	 end

      fun hookEndTag ((c,dtd,enc,content,vecs,stack),_) =
	 case stack 
	   of nil => raise InternalError (THIS_MODULE,"hookEndTag","empty stack")
	    | (elem,attspecs,content1)::stack1 =>
	      let 
		 val (c,content2) = takeData (c,content,vecs)	
		 val tree = Fdm.ELEM(elem,attspecs,Vector.fromList(rev content2))
	      in (c,dtd,enc,tree::content1,nil,stack1)
	      end

      fun hookProcInst ((c,dtd,enc,content,vecs,stack),(_,target,tpos,text)) =
	 let 
	    val (c,content1) = takeData (c,content,vecs)
	    val tree = Fdm.PI(Data2Vector target,Vector.fromList[Fdm.TEXT text])
	 in (c+2,dtd,enc,tree::content,nil,stack) (* the number of nodes must be increased by 2 *)
	 end

      fun addData ((c,dtd,enc,content,vecs,stack),vec) =
	 (c,dtd,enc,content,vec::vecs,stack)
      
      fun hookData (a,(_,vec,_)) = addData(a,vec)
      fun hookCData (a,(_,vec)) = addData(a,vec)
      fun hookCharRef (a,(_,c,_)) = addData(a,Vector.fromList [c])

      fun hookFinish (c,dtd,enc,content,vecs,stack) = 
	let
	  val (c,content1) = takeData (c,content,vecs)
	in
	  case stack of 
	    nil => (c,enc,Vector.fromList (rev content1))
	  | _ => raise InternalError (THIS_MODULE,"hookFinish","non-empty stack")
	end

      fun hookWarning (a,warn) = a before printWarning "" warn
      fun hookError (a,err) = 
         let 
            val _ = printError "" err
            val _ = case err 
                      of (_,ERR_NO_SUCH_FILE _) => raise DocFail
                       | _ => ()
         in a
         end
   end
      
structure MultDocHooks = MultDocHooks0 : MultDocHooks