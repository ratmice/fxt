structure WriteActions = 
  struct
    open SpecDtd MatchDataTreeDom

    exception INLINE_XML of string

    val indentInc = "  "
    fun makeIndent n s =
      if n>0 then makeIndent (n-1) (indentInc^s)
      else s
    val textConstructor = "text"
    val myVector2String = Util.myVector2String
    val writeInt = Int.toString
    val writeVector = WriteUtil.writeVector    
    val writeList = WriteUtil.writeList
    val writeString = WriteUtil.writeString o myVector2String

    val writeUniCharVector = WriteUtil.writeUniCharVector

    val writePair = WriteUtil.writePair

    fun writeConcatVectors indent v =
	   indent^
	   (case (Vector.length v) of
	      0 => "(vector[])"
	    | 1 => "("^(Vector.sub(v,0))^")"
	    | _ =>  "(concatForests (\n"^
		(writeVector (makeIndent 1 indent) true (fn x => x)  v)^
		"))")



    fun writeXslAction dtd functionName docArgument secCount ss =
      let

	val dynamicSelects = ref (nil: (ErrorData.Position * UniChar.Vector) list)
	val nextSecOrder = ref 0

	fun writeTree indent isInNormalElement (pos,t) = 
	  let
	    fun writeAttribute a =
	      let
		val attributeConstructor = "makeAttribute"
		fun attributeArguments2String a = 

		  (
		   "("^(writeUniCharVector (getAttributeName dtd a))^
		   ") ("^
		   (writeUniCharVector (getAttributeValue a)) 
		   ^")"
		   )
	      in
		attributeConstructor^" "^(attributeArguments2String a)
	      end

	    fun getVal (pos,t) attIdx = getAttribute dtd attIdx (pos,t)

	    fun writeElement indent elIdx (pos,t) =
	      let
		val elementName = getElementType dtd t
		val elementConstructor1 = Names.elementConstructor
		val elementConstructor = Names.elementConstructor2
		val attrs = getAttributes t
		val sons = sons t

		val aa = Names.accumulatedAttributes
		val ps = Names.prelucratedSons

		fun writeFilterForest forest =
		  let
		    val testAttribute = getAttribute dtd filterIdx (pos,t)
		    val filterFunction =  
		      "("^
		      (myVector2String testAttribute)^
		      ")"
		  in
		   "(filter "^filterFunction^" "^forest^")"
		  end

		fun writeSortForest forest =
		  let
		    val fName = myVector2String (getAttribute dtd sortIdx (pos,t))
		    val ascending = 
		      if (hasAttribute orderIdx t) then
			let
			  val direction = myVector2String (getAttribute dtd orderIdx (pos,t))
			in
			  case direction of "descending" => false | _ => true
			end
		      else
			true
		    val sortFunction = 
		      if (hasAttribute ltIdx t) then
			let
			  val lt = myVector2String (getAttribute dtd ltIdx (pos,t))
			in
			  "(fn (t1,t2) => "^
			  (if ascending then "not "
			   else "")^
			     "(("^lt^") ((("^fName^") t1),(("^fName^") t2))))"
			end
		      else
			"(fn (t1,t2) => (("^fName^") t1)"^
			(if ascending then ">="
			 else "<=")^
			 "(("^fName^") t2))"
		  in
		    "(sort "^sortFunction^" "^forest^")"
		  end

		fun writeFinalizeForest forest =
		  let
		    val finalizeFunction = myVector2String (getAttribute dtd finalizeIdx (pos,t))
		  in
		    "("^finalizeFunction^" "^forest^")"
		  end

		fun writeFilterSortFinalizeForest forest =
		  let
		    val forest = 
		      if (hasAttribute filterIdx t) then
			writeFilterForest forest
		      else forest
		    val forest = 
		      if (hasAttribute sortIdx t) then
			writeSortForest forest
		      else forest
		    val forest = 
		      if (hasAttribute finalizeIdx t) then
			writeFinalizeForest forest
		      else forest
		  in
		    forest
		  end

		fun writeSwitch indent =
		  let
		    fun doit ((pos,t),(acc,default)) =
		      case t of
			MatchData.T_ELEM (elIdx,_,_) =>
			  (if elIdx = caseIdx then
			     let 
			       val test = myVector2String (getVal (pos,t) testIdx)
			       val prelucratedSons = 
				 Vector.map (writeTree (indent^indentInc) false) (MatchDataTreeDom.sons t)
			     in
			       (((test,prelucratedSons)::acc),default)
			     end
			   else if elIdx = defaultIdx then
			     let 
			       val prelucratedSons = 
				 Vector.map (writeTree (indent^indentInc) false) (MatchDataTreeDom.sons t)
			     in
			       case default of
				 NONE => (acc,(SOME prelucratedSons))
			       | _ => 
				   raise Exceptions.SpecificationError (pos,"default case already specified")
			     end
				else (acc,default))
		      | _ => (acc,default)
		    val (cases,default) = Vector.foldr doit ([],NONE) sons
		  in
		    case default of 
		      SOME defaultForest =>
			let
			  fun doit l indent =
			    case l of
			      [(test,prelucratedSons)] =>
				indent^"(if "^test^" then\n"^
				indent^"  "^(writeConcatVectors indent prelucratedSons)^"\n"^
				indent^" else\n"^
				indent^"  "^(writeConcatVectors indent defaultForest)^
				")\n"
			      | ((test,prelucratedSons)::r) =>
				indent^"(if "^test^" then\n"^
				indent^"  "^(writeConcatVectors indent prelucratedSons)^"\n"^
				indent^" else\n"^
				indent^"   ("^(doit r (indent^indentInc))^"))"^"\n"
			      | [] => raise 
				Exceptions.SpecificationError (pos,"at least one case test expected")
			in
			  doit cases indent
			end
		    | NONE => raise Exceptions.SpecificationError (pos,"default case expected")
		  end

		fun writeIf indent =
		  let 
		    val test = myVector2String (getVal (pos,t) testIdx)
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons
		  in
		    indent^"(if "^test^" then\n"^
		    (writeConcatVectors (makeIndent 2 indent) prelucratedSons)^"\n"^
		    indent^" else  (vector[]))\n"
		  end

		fun writeDocMap docs =
		  if (hasAttribute testIdx t) then
		    let
		      val testAttribute = myVector2String (getAttribute dtd testIdx (pos,t))
		      val filterFunction = "("^testAttribute^")"
		    in
		      "(Doc.docMapFilter "^functionName^" "^filterFunction^" "^docs^")"
		    end
		  else
		    "(Doc.docMap "^functionName^" "^docs^")"

		fun writeApply indent = 
		  let
		    val forest = 
		      if (hasAttribute selectIdx t) then
			let
			  val selectAttribute = (getAttribute dtd selectIdx (pos,t))
			  val secOrd = UniChar.Vector2String selectAttribute
			in
			  case Int.fromString secOrd of
			    SOME _ =>
			      "(Doc.secondaries "^docArgument^" \""^secOrd^"\")"
			  | NONE => 
			      if (!FxtOptions.O_REMOVE_SELECTS) then
				let
				  val secOrd = Int.toString (!nextSecOrder)
				  val a = "(Doc.secondaries "^docArgument^" \""^secOrd^"\")"
				  val _ = nextSecOrder := !nextSecOrder + 1
				  val _ = dynamicSelects := (pos,selectAttribute)::(!dynamicSelects)
				in
				  a
				end
			      else
				"(Doc.dynamicSelect "^(writeUniCharVector selectAttribute)^
				" "^docArgument^")"
			end
		      else
			if (hasAttribute selectExpIdx t) then
			  let
			    val selectAttribute = (getAttribute dtd selectExpIdx (pos,t))
			  in
			    "(Doc.dynamicSelect ("^(myVector2String selectAttribute)^") "^
			    docArgument^")"
			  end
			else
			  "(Doc.docSons "^docArgument^")"
		    val forest = writeDocMap forest
		    val forest = writeFilterSortFinalizeForest forest
		  in
		    indent^forest^"\n"
		  end

		fun writeCopyContent indent =
		  let
		    val forest = 
		      if (hasAttribute selectIdx t) then
			let
			  val selectAttribute = (getAttribute dtd selectIdx (pos,t))
			  val secOrd = UniChar.Vector2String selectAttribute
			in
			  case Int.fromString secOrd of
			    SOME _ => "(Vector.fromList (List.map Doc.docTree (Doc.secondaries "^ docArgument^" \""^secOrd^"\")))"
			  | NONE => 
			      if (!FxtOptions.O_REMOVE_SELECTS) then
				let
				  val secOrd = Int.toString (!nextSecOrder)
				  val a = "(Vector.fromList (List.map Doc.docTree (Doc.secondaries "^ 
				    docArgument^" \""^secOrd^"\")))"
				  val _ = nextSecOrder := !nextSecOrder + 1
				  val _ = dynamicSelects := (pos,selectAttribute)::(!dynamicSelects)
				in
				  a
				end
			      else
				"(Vector.fromList (List.map Doc.docTree (Doc.dynamicSelect "^
				(writeUniCharVector selectAttribute)^" "^docArgument^")))"

			end
		      else "(sons current)"
		    val forest = writeFilterSortFinalizeForest forest
		  in
		    indent^"(if (isElement current) then\n"^
		    indent^"   "^forest^"\n"^
		    indent^" else if (isProcessingInstruction current)\n"^
		    indent^"      then vector[text (#2 (getProcessingInstruction current))]\n"^
		    indent^"      else (vector[current]))\n"
		  end

		fun writeCurrent2 indent =
		  let
		    val secOrd =
		      if (hasAttribute selectIdx t) then 
			UniChar.Vector2String (getAttribute dtd selectIdx (pos,t))
		      else "1"
		    val forest = "(Vector.fromList (List.map Doc.docTree (Doc.secondaries "^
		      docArgument^" \""^secOrd^"\")))"
		    val forest = writeFilterSortFinalizeForest forest
		  in
		    indent^forest^"\n"
		  end

		fun writeCopyType indent = 
		  let
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons 
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		  in
		    indent^"let\n"^
		    indent^"  val "^aa^" = ref (#[] : Attribute vector)\n"^
		    indent^"  val "^ps^" = "^sons^"\n"^
		    indent^"in\n"^
		    indent^"  if (isElement current) then\n"^
		    indent^"    vector["^elementConstructor^" (getElementType current) (!"^aa^") "^ps^"]\n"^
		    indent^"  else if (isProcessingInstruction current)\n"^
		    indent^"       then vector[processingInstruction (#1 (getProcessingInstruction current)) (Vector.foldl (fn (t,d) => concatVectors(d,getText t)) nullVector (concatForests "^ps^"))]\n"^
		    indent^"      else (vector[current])\n"^
		    indent^"end\n"
		  end

		fun writeCopyTag indent =
		  let
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false ) sons
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		  in
		    indent^"(let\n"^
		    indent^"   val "^aa^" = ref (getAttributes current)\n"^
		    indent^"   val "^ps^" = "^sons^"\n"^
		    indent^" in\n"^
		    indent^"   vector["^elementConstructor^" (getElementType current) (!"^aa^") "^ps^"]\n"^
		    indent^" end)\n"
		  end

		fun writeCopyTagApply indent =
		  indent^"if (isElement current) then\n"^ 
		  indent^"  (vector["^
		  elementConstructor1^
		  " (getElementType current) (getAttributes current) "^
		  (writeApply "")^
		  "])\n"^
		  indent^"else (vector[current])\n"

		fun writeSml indent =
		  let
		    val code =  myVector2String (getVal (pos,t) codeIdx)
		  in
		    indent^"(("^code^");(vector[]))"
		  end

		fun writeTag indent =
		  let
		    val name =  myVector2String (getVal (pos,t) fxtNameIdx)
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		  in
		    indent^"(let\n"^
		    indent^"   val "^aa^" = ref (deleteAttribute ("^(WriteUtil.writeVector "" false writeAttribute attrs)^") (String2Vector \"fxt:name\"))\n"^ 
		    indent^"   val "^ps^" = "^sons^"\n"^
		    indent^"  in\n"^
		    indent^"   vector["^elementConstructor^" ("^name^") "^"(!"^aa^") "^ps^"]\n"^
		    indent^" end)\n"
		  end

		fun writeGeneratedPi indent =
		  let
		    val processor = 
		      if (hasAttribute processorIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) processorIdx)
		      else
			myVector2String (getVal (pos,t) processorExpIdx)
		    val data = 
		      if (hasAttribute dataIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) dataIdx)
		      else
			myVector2String (getVal (pos,t) dataExpIdx)
		  in
		    indent^
		    "vector[processingInstruction ("^
		    processor^
		    ") ("^
		    data^
		    ")]"
		  end

		fun writeOutputText indent =
		  let
		    val code =  myVector2String (getVal (pos,t) codeIdx)
		  in
		    indent^"(vector["^textConstructor^" ("^code^")])"
		  end

		fun writeOutputCurrentText indent =
		  indent^"(vector["^textConstructor^" (getTextContent current)])"

		fun writeUnichars v indent =
		    indent^"(vector["^textConstructor^" ("^(writeUniCharVector v)^")])"

		fun writeCopyAttributes indent =
		  indent^"(if (isElement current) then\n"^
		  indent^"   let\n"^
		  indent^"     val _= "^aa^" := addAttributes (!"^aa^") (getAttributes "^
		  Names.current^")\n"^
		  indent^"   in\n"^
		  indent^"     vector[]\n"^
		  indent^"   end\n"^
		  indent^" else vector[])\n"

		fun writeCurrentAttributeValue indent =
		  let
		    val attributeName =  
		      if (hasAttribute nameIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) nameIdx)
		      else
			myVector2String (getVal (pos,t) nameExpIdx)
		  in
		    indent^"(vector["^textConstructor^" (getAttribute ("^attributeName^") current)])"
		  end
		  
		fun writeAddAttribute indent =
		  let
		    val name = 
		      if (hasAttribute nameIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) nameIdx)
		      else
			myVector2String (getVal (pos,t) nameExpIdx)
		    val value = 
		      if (hasAttribute valIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) valIdx)
		      else
			myVector2String (getVal (pos,t) valExpIdx)
		  in
		    indent^"(if (isElement current) then\n"^
		    indent^"  (let\n"^
		    indent^"    val _= "^aa^" := addAttribute (!"^aa^") ("^name^
		    ") ("^(value)^")\n"^
		    indent^"   in\n"^
		    indent^"     vector[]\n"^
		    indent^"   end)\n"^
		    indent^" else vector[])\n"
		  end

		fun writeDeleteAttribute indent =
		  let
		    val name = 
		      if (hasAttribute nameIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) nameIdx)
		      else
			myVector2String (getVal (pos,t) nameExpIdx)
		  in
		    indent^"(if (isElement current) then\n"^
		    indent^"  let\n"^
		    indent^"    val _= "^aa^" := deleteAttribute (!"^aa^") ("^
		    name^")\n"^
		    indent^"  in\n"^
		    indent^"    vector[]\n"^
		    indent^"  end\n"^
		    indent^" else vector[])\n"
		  end

		fun writeReplaceAttribute indent =
		  let
		    val name = 
		      if (hasAttribute nameIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) nameIdx)
		      else
			myVector2String (getVal (pos,t) nameExpIdx)
		    val value = 
		      if (hasAttribute valIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) valIdx)
		      else
			myVector2String (getVal (pos,t) valExpIdx)
		  in
		    indent^"(if (isElement current) then\n"^
		    indent^"  let\n"^
		    indent^"    val _= "^aa^" := replaceAttribute (!"^aa^") ("^name^
		    ") ("^(value)^")\n"^
		    indent^"  in\n"^
		    indent^"    vector[]\n"^
		    indent^"  end\n"^
		    indent^" else vector[])\n"
		  end

		fun writeCopyTagAddAttribute indent =
		  let
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		    val name = 
		      if (hasAttribute nameIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) nameIdx)
		      else
			myVector2String (getVal (pos,t) nameExpIdx)
		    val value = 
		      if (hasAttribute valIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) valIdx)
		      else
			myVector2String (getVal (pos,t) valExpIdx)
		  in
		    indent^"(let\n"^
		    indent^"   val "^aa^" = ref (addAttribute (getAttributes current) ("^
		    name^") ("^
		    (value)^"))\n"^
		    indent^"   val "^ps^" = "^sons^"\n"^
		    indent^" in\n"^
		    indent^"   vector["^elementConstructor^" (getElementType current) (!"^aa^") "^ps^"]\n"^
		    indent^"  end)\n"
		  end

		fun writeCopyTagDeleteAttribute indent =
		  let
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		    val name = 
		      if (hasAttribute nameIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) nameIdx)
		      else
			myVector2String (getVal (pos,t) nameExpIdx)
		  in
		    indent^"(let\n"^
		    indent^"   val "^aa^" = ref (deleteAttribute (getAttributes current) ("^
		    name^"))\n"^
		    indent^"   val "^ps^" = "^sons^"\n"^
		    indent^" in\n"^
		    indent^"   vector["^elementConstructor^" (getElementType current) (!"^aa^") "^ps^"]\n"^
		    indent^" end)\n"
		  end

		fun writeCopyTagReplaceAttribute indent =
		  let
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		    val name = 
		      if (hasAttribute nameIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) nameIdx)
		      else
			myVector2String (getVal (pos,t) nameExpIdx)
		    val value = 
		      if (hasAttribute valIdx t) then
			WriteUtil.writeUniCharVector (getVal (pos,t) valIdx)
		      else
			myVector2String (getVal (pos,t) valExpIdx)
		  in
		    indent^"(let\n"^
		    indent^"   val "^aa^" = ref (replaceAttribute (getAttributes current) ("^
		    name^") ("^
		    (value)^"))\n"^
		    indent^"   val "^ps^" = "^sons^"\n"^
		    indent^" in\n"^
		    indent^"   vector["^elementConstructor^" (getElementType current) (!"^aa^") "^ps^"]\n"^
		    indent^" end)\n"
		  end

		fun writeCurrent indent =
		  indent^"(vector[current])"

		fun writePush indent =
		  let
		    val name = Names.globalsStructure^"."^
		      (myVector2String (getVal (pos,t) nameIdx))
		    val value = myVector2String (getVal (pos,t) valIdx)
		  in
		    indent^"(let\n"^
		    indent^"   val _ = Globals.push ("^name^") ("^value^")\n"^
		    indent^" in\n"^
		    indent^"   vector[]\n"^
		    indent^" end)\n"
		  end

		fun writePushForest indent =
		  let
		    val name = Names.globalsStructure^"."^
		      (myVector2String (getVal (pos,t) nameIdx))
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons 
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		  in
		    indent^"(let\n"^
		    indent^"   val _ = Globals.push ("^name^") (concatForests ("^sons^"))\n"^
		    indent^" in\n"^
		    indent^"   vector []\n"^
		    indent^" end)\n"
		  end

		fun set indent name value = 
		  indent^"(let\n"^
		  indent^"   val _ = Globals.set ("^name^") ("^value^")\n"^
		  indent^" in\n"^
		  indent^"   vector[]\n"^
		  indent^" end)\n"

		fun writeSet indent =
		  let
		    val name = Names.globalsStructure^"."^
		      (myVector2String (getVal (pos,t) nameIdx))
		    val value = myVector2String (getVal (pos,t) valIdx)
		  in
		    set indent name value
		  end

		fun writeInc indent =
		  let
		    val name = Names.globalsStructure^"."^
		      (myVector2String (getVal (pos,t) nameIdx))

		  in
		    set indent name ("(Globals.get "^name^")+1")
		  end


		fun writeSetForest indent =
		  let
		    val name = Names.globalsStructure^"."^
		      (myVector2String (getVal (pos,t) nameIdx))
		    val prelucratedSons = Vector.map (writeTree (indent^indentInc) false) sons 
		    val sons = (writeVector indent true (fn x => x)  prelucratedSons)
		    val sons = writeFilterSortFinalizeForest sons
		  in
		    indent^"(let\n"^
		    indent^"   val _ = Globals.set ("^name^") (concatForests ("^sons^"))\n"^
		    indent^" in\n"^
		    indent^"   vector []\n"^
		    indent^" end)\n"
		  end

		fun writePop indent =
		  let
		    val name = Names.globalsStructure^"."^
		      (myVector2String (getVal (pos,t) nameIdx))
		  in
		    indent^"(let\n"^
		    indent^"   val _ = Globals.pop ("^name^")\n"^
		    indent^" in\n"^
		    indent^"   vector[]\n"^
		    indent^" end)\n"
		  end

		fun writeGet indent =
		  let
		    val name = 
		      (myVector2String (getVal (pos,t) nameIdx))
		    val (_, info) = (GlobalsTable.getEntry name) handle e => raise Exceptions.SpecificationError (pos,"Variable "^name^" was not declared")
		    val toForest =
		      (case info of 
			 GlobalsTable.INT => "Globals.int2Forest"
		       | GlobalsTable.BOOL => "Globals.bool2Forest"
		       | GlobalsTable.REAL => "Globals.real2Forest"
		       | GlobalsTable.STRING => "Globals.string2Forest"
		       | GlobalsTable.TREE => "Globals.Tree2Forest"
		       | GlobalsTable.FOREST => "Globals.Forest2Forest"
		       | GlobalsTable.USER s => s)
		    val forest = "("^toForest^" "^Names.globalsStructure^"."^name^")"
		    val forest = writeFilterSortFinalizeForest forest
		  in
		    indent^forest^"\n"
		  end

		fun writeCopyKey indent =
		  let
		    val tableName = getVal (pos,t) nameIdx
		    val key = 
		      if (hasAttribute keyAttIdx t) then
			"getAttribute "^
			(WriteUtil.writeUniCharVector 
			 (getVal (pos,t) keyAttIdx))^
			" current"
		      else
			myVector2String (getVal (pos,t) keyExpAttIdx)
		  in
		    indent^"(case\n"^
		    indent^" (TreeTables.findKey "^(writeUniCharVector tableName)^" ("^key^")) of\n"^
                    indent^"   SOME docs => Vector.fromList (List.map Doc.docTree docs)\n"^
		    indent^"  |NONE => vector[])\n"
		  end

		fun writeGetTableItems indent =
		  let
		    val tableName = getVal (pos,t) nameIdx
		    val key = 
		      if (hasAttribute keyAttIdx t) then
			"getAttribute "^
			(WriteUtil.writeUniCharVector 
			 (getVal (pos,t) keyAttIdx))^
			" current"
		      else
			myVector2String (getVal (pos,t) keyExpAttIdx)
		  in
		    indent^"(case\n"^
		    indent^" (StringTables.findKey "^(writeUniCharVector tableName)^" ("^key^")) of\n"^
                    indent^"   SOME items => Vector.fromList (List.map (fn s=>(text s)) items)\n"^
		    indent^"  |NONE => vector[])\n"
		  end

		fun writeApplyKey indent = 
		  let
		    val tableName = getVal (pos,t) nameIdx
		    val key = 
		      if (hasAttribute keyAttIdx t) then
			(WriteUtil.writeUniCharVector 
			 (getVal (pos,t) keyAttIdx))
		      else
			myVector2String (getVal (pos,t) keyExpAttIdx)
		    val forest = 
		      indent^"(case\n"^
		      indent^" (TreeTables.findKey "^(writeUniCharVector tableName)^" ("^key^")) of\n"^
		      indent^"   SOME docs => docs\n"^
		      indent^"  |NONE => [])\n"
		    val forest = writeDocMap forest
		    val forest = writeFilterSortFinalizeForest forest
		  in
		    indent^forest^"\n"
		  end

		fun writeDefault indent =
		  let
		    val prelucratedSons = Vector.map (writeTree (makeIndent 2 indent) true) sons 
		  in
		    indent^"(let\n"^
		    indent^"   val "^aa^" = ref (("^(WriteUtil.writeVector ""  false writeAttribute attrs)^") : Attribute vector)\n"^
		    indent^"   val "^ps^" = "^(writeVector "" true (fn x => x)  prelucratedSons)^"\n"^
		    indent^" in\n"^
		    indent^"   vector["^elementConstructor^" "^
		    (writeUniCharVector elementName)^" "^
		    "(!"^aa^") "^ps^"]\n"^
		    indent^" end)\n"
		  end

		val fxtElements = 
		  [(pushIdx,writePush),
		   (popIdx,writePop),
		   (getIdx,writeGet),
		   (currentIdx,writeCurrent),
		   (ifIdx,writeIf),
		   (switchIdx,writeSwitch),
		   (copyTagIdx,writeCopyTag),
		   (copyTagDeleteAttributeIdx,writeCopyTagDeleteAttribute),
		   (copyTagAddAttributeIdx,writeCopyTagAddAttribute),
		   (copyTagReplaceAttributeIdx,writeCopyTagReplaceAttribute),
		   (applyIdx,writeApply),
		   (copyTagApplyIdx,writeCopyTagApply),
		   (smlIdx,writeSml),
		   (tagIdx,writeTag),
		   (piIdx,writeGeneratedPi),
		   (textIdx,writeOutputText),
		   (currentTextIdx,writeOutputCurrentText),
		   (attributeIdx,writeCurrentAttributeValue),
		   (setIdx,writeSet),
                   (incIdx,writeInc),
		   (setForestIdx,writeSetForest),
                   (pushForestIdx,writePushForest),
                   (copyContentIdx,writeCopyContent),
		   (copyAttributesIdx,writeCopyAttributes),
		   (addAttributeIdx,writeAddAttribute),
		   (deleteAttributeIdx,writeDeleteAttribute),
		   (replaceAttributeIdx,writeReplaceAttribute),
		   (htIdx,writeUnichars #[0wx0009]),
		   (crIdx,writeUnichars #[0wx000D]),
		   (spIdx,writeUnichars #[0wx0020]),
		   (copyTypeIdx,writeCopyType),
		   (copyKeyIdx,writeCopyKey),
		   (applyKeyIdx,writeApplyKey),
		   (getTableItemsIdx,writeGetTableItems),
		   (current2Idx,writeCurrent2)]
		fun doit l =
		  case l of
		    (fxtElementIdx,writeFunction)::r =>
		      if elIdx = fxtElementIdx then writeFunction indent
		      else doit r
		  | _ => writeDefault indent
	      in
		(
		 (WriteUtil.writeComment indent ("Generated from <"^(myVector2String (getElement dtd elIdx))^"> in "^(Errors.Position2String pos)))^"\n"^
		 (doit fxtElements)^
		 (WriteUtil.writeComment indent ("</"^(myVector2String (getElement dtd elIdx))^">"))
		   )
	      end
	    
	    fun writeProcessingInstruction indent (pos,t) =
	      let
		val processingInstructionConstructor = 
		  "processingInstruction"
	        val write = writeUniCharVector
		val processingInstructionArguments2String =
		  writePair "" false write write
	      in
		indent^
		"(vector["^processingInstructionConstructor^" "
		^(processingInstructionArguments2String 
		  (getProcessingInstruction t))^"])"
	      end
	    fun writeText indent (pos,t) =
	      let
		val text = getText t
	      in
		indent^
		(if (not isInNormalElement) andalso (Util.isSpace text) then
		   "(vector[])"
		 else
		   "(vector["^textConstructor^" "^
		   (writeUniCharVector text)^
		   "])")
	      end
	  in
	    case t of 
	      MatchData.T_ELEM (elIdx,_,_) => writeElement indent elIdx (pos,t)
	    | MatchData.T_PI _ => writeProcessingInstruction indent (pos,t)
	    | _ => writeText indent (pos,t)
	  end

	val _ = dynamicSelects := (nil: (ErrorData.Position * UniChar.Vector) list)
	val _ = nextSecOrder :=
	  (case secCount of 
	     SOME c => c+1
	   | _ => 1)

	val action = writeConcatVectors "" (Vector.map (writeTree (makeIndent 2 "") false) ss)
	val dynSelects = !dynamicSelects
      in
	(rev dynSelects,action)
      end
  end