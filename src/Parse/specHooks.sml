structure SpecHooks =
   struct
      open FxtOptions Encoding Errors HookData IgnoreHooks MatchData UniChar

      val THIS_MODULE = "DocHooks"

      exception DocFail

      type AppFinal = bool * string * Content
      type AppData = Dtd.Dtd * bool * string * (Position * Tree) list * (Position * Vector) list 
         * (Position * int * AttSpc vector * (Position * Tree) list) list
	 
      fun printError (pos,err) = 
         if !O_SILENT then () else TextIO.output
            (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
             (Position2String pos^" Parse Error: "::errorMessage err))
      fun printWarning (pos,warn) = 
         if !O_SILENT then () else TextIO.output
            (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
             (Position2String pos^" Parse Warning: "::warningMessage warn))

      fun docStart dtd = (dtd,false,"UTF-8",nil,nil,nil)

      fun hookXml((dtd,err,_,content,vecs,stack),(_,enc,opt)) =
         let val encName = case opt
                             of SOME(_,SOME encName,_) => encName
                              | _ => encodingName enc
         in (dtd,err,encName,content,vecs,stack)
         end

      fun posOf (pos,_) = pos

      fun takeData (content,vecs) = 
	 let 
            val vecs = rev vecs
            val vec = Vector.concat (map #2 vecs)
	 in 
            if Vector.length vec=0 then content 
            else (posOf(hd vecs),T_TEXT vec)::content
	 end

      fun hookStartTag ((dtd,err,enc,content,vecs,stack),((pos,_),elem,atts,_,mt))  =
	 let 
	    val content1 = takeData (content,vecs)
	    val attspecs = List.mapPartial 
	       (fn (i,ap,_) => case ap
				 of AP_DEFAULT(_,av,_) => SOME(i,av)
				  | AP_PRESENT(_,av,_) => SOME(i,av)
				  | _ => NONE) atts
               val attvec = Vector.fromList attspecs
	 in if mt then (dtd,err,enc,(pos,T_ELEM(elem,attvec,emptyContent))::content1,nil,stack)
	    else (dtd,err,enc,nil,nil,(pos,elem,attvec,content1)::stack)
	 end

      fun hookEndTag ((dtd,err,enc,content,vecs,stack),_) =
	 case stack 
	   of nil => raise InternalError (THIS_MODULE,"hookEndTag","empty stack")
	    | (pos,elem,attspecs,content1)::stack1 =>
	      let 
		 val content2 = takeData (content,vecs)	
		 val tree = T_ELEM(elem,attspecs,Vector.fromList(rev content2))
	      in (dtd,err,enc,(pos,tree)::content1,nil,stack1)
	      end

      fun hookProcInst ((dtd,err,enc,content,vecs,stack),((pos,_),target,tpos,text)) =
	 let 
	    val content1 = takeData (content,vecs)
	    val tree = T_PI(Data2Vector target,Vector.fromList[(tpos,T_TEXT text)])
	 in (dtd,err,enc,(pos,tree)::content,nil,stack)
	 end

      fun addData ((dtd,err,enc,content,vecs,stack),pos,vec) =
	 (dtd,err,enc,content,(pos,vec)::vecs,stack)
      
      fun hookData (a,((pos,_),vec,_)) = addData(a,pos,vec)
      fun hookCData (a,((pos,_),vec)) = addData(a,pos,vec)
      fun hookCharRef (a,((pos,_),c,_)) = addData(a,pos,Vector.fromList [c])

      fun hookFinish (dtd,err,enc,content,vecs,stack) = 
	 case stack
	   of nil => (err,enc,Vector.fromList (rev (takeData (content,vecs))))
	    | _ => raise InternalError (THIS_MODULE,"hookFinish","non-empty stack")

      fun hookWarning (a,warn) = a before printWarning warn
      fun hookError ((dtd,_,enc,content,vecs,stack),error) = 
         (dtd,true,enc,content,vecs,stack) before printError error 
   end
