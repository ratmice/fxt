structure ParseSpec = 
  struct
    open SpecDtd GramData

    structure SpecParser =
      Parse(structure Dtd = Dtd
	    structure Hooks = SpecHooks
	    structure Resolve = ResolveNull
	    structure ParserOptions = FxtOptions)

    val Vector2String = Util.myVector2String

    fun readSpec uriOption = 
      let
	fun parseHeader dtd f =
	  let
	    fun doit1 ((pos,t),(stringTables,tables,preActs,toInclude,toOpen,globals,arguments)) =
	      case t of
		MatchData.T_ELEM (i,_,_) =>
		  let 
		    fun getAttribute i = MatchDataTreeDom.getAttribute dtd i (pos,t)
		    fun hasAttribute i = MatchDataTreeDom.hasAttribute i t
		  in
		    if i = globalIdx then
		      let
			val name =Vector2String (getAttribute nameIdx)
			val typ = Vector2String (getAttribute typeIdx)
			val toForest = 
			  if hasAttribute toForestIdx then GlobalsTable.USER (Vector2String (getAttribute toForestIdx))
			  else
			    case typ of
			      "int" => GlobalsTable.INT
			    | "bool"  => GlobalsTable.BOOL
			    | "real" => GlobalsTable.REAL
			    | "string" => GlobalsTable.STRING
			    | "Tree" => GlobalsTable.TREE
			    | "Forest" => GlobalsTable.FOREST
			    | _ => GlobalsTable.USER "(fn x => emptyForest)"
			val _ = GlobalsTable.addEntry name (typ,toForest)
		      in
			(stringTables,tables,preActs,toInclude,toOpen,(name,(typ,toForest))::globals,arguments)
		      end
		    else 
		      if i = argIdx then
			let
			  val name = Vector2String (getAttribute nameIdx)
			  val toForest =
			    if hasAttribute toForestIdx then GlobalsTable.USER (Vector2String (getAttribute toForestIdx))
			    else GlobalsTable.STRING
			  val entry = ("string",toForest)
			  val _ = GlobalsTable.addEntry name entry
			in
			  (stringTables,tables,preActs,toInclude,toOpen,(name,entry)::globals,(name)::arguments)
			end
		      else 
			if i = openIdx then
			  let 
			    val structs = Vector2String (getAttribute structureIdx)
			  in
			    if hasAttribute fileIdx then 
			      (stringTables,tables,preActs,Vector2String (getAttribute fileIdx)::toInclude,toOpen^" "^structs,globals,arguments)
			    else
			      (stringTables,tables,preActs,toInclude,toOpen^" "^structs,globals,arguments)

			  end
			else
			  if i = pushIdx then
			    let
			      val name = Names.globalsStructure^"."^
				(Vector2String (getAttribute nameIdx))
			      val value = Vector2String (getAttribute valIdx)
			      val preAct = "Globals.push ("^name^") ("^value^")"
			    in
			      (stringTables,tables,preAct::preActs,toInclude,toOpen,globals,arguments)
			    end
			  else
			    if i = popIdx then
			      let
				val name = Vector2String (getAttribute nameIdx)
				val preAct = "Globals.pop ("^name^")"
			      in
				(stringTables,tables,preAct::preActs,toInclude,toOpen,globals,arguments)
			      end
			    else
			      if i = setIdx then
				let
				  val name = Vector2String (getAttribute nameIdx)
				  val value = Vector2String (getAttribute valIdx)
				  val preAct = "Globals.set ("^name^") ("^value^")"
				in
				  (stringTables,tables,preAct::preActs,toInclude,toOpen,globals,arguments)
				end
			      else
				if i = smlIdx then
				  let
				    val code =  Vector2String (getAttribute codeIdx)
				    val preAct = "("^code^");"
				  in
				    (stringTables,tables,preAct::preActs,toInclude,toOpen,globals,arguments)
				  end
				else
				  if i = keyIdx then
				    let
				      val name = (getAttribute nameIdx)
				      val select = (getAttribute selectIdx)
				      val toString = 
					if (hasAttribute keyAttIdx) then
					  "(Fdm.getAttribute "^
					  (WriteUtil.writeUniCharVector
					   (getAttribute keyAttIdx))^
					  ")"
					else
					  Vector2String (getAttribute keyExpAttIdx)
				    in
				      (stringTables,(name,(pos,select),toString)::tables,preActs,toInclude,toOpen,globals,arguments)
				    end		
				  else 
				    if i = tableIdx then
				      let
					val name = (getAttribute nameIdx)
					val select = (getAttribute selectIdx)
					val toString = 
					  if (hasAttribute keyAttIdx) then
					    "(Fdm.getAttribute "^
					    (WriteUtil.writeUniCharVector
					     (getAttribute keyAttIdx))^
					    ")"
					  else
					    Vector2String (getAttribute keyExpAttIdx)
					val toItem = 
					  if hasAttribute itemAttIdx then
					    Vector2String (getAttribute itemAttIdx)
					  else
					    "(fn x:Fdm.Tree=>x)"
				      in
					((name,(pos,select),toString,toItem)::stringTables,tables,preActs,toInclude,toOpen,globals,arguments)
				      end
				    else
				      (stringTables,tables,preActs,toInclude,toOpen,globals,arguments)
		  end
	      | _ =>  (stringTables,tables,preActs,toInclude,toOpen,globals,arguments)
	  in
	    let
	      val name = Names.inputFile
	      val entry = ("string",GlobalsTable.STRING)
	      val _ =  GlobalsTable.addEntry  name entry
	    in
	      Vector.foldl doit1 ([],[],[],[],Names.toOpen,[(name,entry)],[]) f
	    end
	  end
	  

	fun parseTFSpec uriOption =
	  let
	    val dtd = SpecDtd.initDtdTables ()
	    val (hadErr,_,v) = SpecParser.parseDocument 
	      uriOption (SOME dtd) (SpecHooks.docStart dtd)
	  in
	    if hadErr then
	      raise Exceptions.ParseError
	    else
	      let
		fun doit i =
		  if i >= Vector.length v then
		    raise Exceptions.InternalError
		  else
		    let
		      val (pos,t) = Vector.sub(v,i)
		    in
		      case t of
			MatchData.T_ELEM (elIdx,atts,content) =>
			  if elIdx = specIdx then
			    let
			      fun readUntilPattern f =
				let
				  fun doit i a =
				    if (i >= Vector.length f) then (NONE,Vector.fromList (rev a),vector[])
				    else
				      let
					val (pos,t) = Vector.sub(f,i)
				      in
					case t of
					  MatchData.T_ELEM (elIdx,_,_) =>
					    if elIdx = patIdx then
					      case t of 
						MatchData.T_ELEM (_,_,#[(pos,MatchData.T_TEXT pat)]) =>
						  (SOME (pos,pat),Vector.fromList (rev a),VectorSlice.vector (VectorSlice.slice(f,i+1,NONE)))
					      | _  =>  raise 
						  Exceptions.SpecificationError (pos,"fxt:pat element must have only text content")
					    else doit (i+1) ((pos,t)::a)
					      | _ => doit (i+1) ((pos,t)::a)
				      end
				in
				  doit 0 []
				end
			      val (p1,header,rest) = readUntilPattern content
			      val  (stringTables,tables,preActs,toInclude,toOpen,fxslGlobals,yetAnotherArguments,patternsActionsList) = 
				case p1 of 
				  SOME pat =>
				    let
				      val _ =  GlobalsTable.initTable ()
				      val (stringTables,tables,preActs,toInclude,toOpen,fxslGlobals,yetAnotherArguments) = parseHeader dtd header
				      fun doit p a f =
					let
					  val (nextp,action,rest) = readUntilPattern f
					in
					  case nextp of
					    SOME pat1 => doit pat1 ((p,action)::a) rest
					  | NONE => (rev ((p,action)::a))
					end
				    in
				      (stringTables,tables,preActs,toInclude,toOpen,fxslGlobals,yetAnotherArguments,doit pat [] rest)
				    end
				| NONE => raise Exceptions.SpecificationError (pos,"at least one pattern must be specified")

			      
			      (* sees if a rule for the default
			      pattern was specified; return the
			      optional action for the default pattern,
			      and the other pairs of (pattern,action)
			      *)
			      fun extractDefault patternsActions =
				let
				  fun isDefault p =
				    (Util.trimUniCharSpaces (UniChar.Vector2Data p)) = 
				    UniChar.String2Data Names.default
				  fun doit lpsas d psActs =
				    case lpsas of
				      nil => (d,(rev psActs))
				    | ((pos,pat),a)::rpa => 
					if (isDefault pat) then doit rpa (SOME a) psActs
					else doit rpa d (((pos,pat),a)::psActs)
				in
				  doit patternsActions NONE []
				end

			      val (defaultAction,patternsActions) = 
				extractDefault patternsActionsList

			      (* parse and translate the default action *)
			      val defaultAction = 
				Option.map 
				(#2 o 
				 (WriteActions.writeXslAction 
				  dtd Names.functionName Names.docArgument NONE)) defaultAction

			      val (patterns,actions) = ListPair.unzip patternsActionsList

			      (* parse the patterns *)
			      val docDtd = DocDtd.initDocDtd()
			      val parsedPatterns = 
				Vector.fromList
				((List.map 
				  (PatternParser.parseUniCharVectorPattern docDtd)
				  patterns):GramData.Pattern list)
				 
			      val stringPatterns = 
				Vector.fromList
				(List.map (Vector2String o #2) patterns)


			      (* the translation function must get as
			      argument for every pattern the number of
			      secondaries in it, needed in the
			      automatic translation from dynamic to
			      binary patterns *)

			      fun countSecondaries pat =
				let
				  fun completePath cp =
				    case cp of
				      CP_NODE tp => targetNodePattern tp
				    | CP_OR (cp1,cp2) => 
					(completePath cp1) + (completePath cp2)
				    | CP_COMPOSED (ip,tp) =>
					(incompletePath ip) + (targetNodePattern tp)
				  and targetNodePattern (sec,np) =
				    if sec then 1 + (nodePattern np)
				    else nodePattern np
				  and nodePattern (_,_,qlist) =
				    List.foldl
				    (fn ((_,q),c) => c + (qualifier q))
				    0 qlist
				  and qualifier q =
				    case q of
				      Q_ATT _ => 0
				    | Q_CHILDREN fp => forestPattern fp
				  and forestPattern fp =
				    case fp of
				      RegExp.RE_ALT (re1,re2) =>
					(forestPattern re1) + (forestPattern re2)
				    | RegExp.RE_BASIC tp => (treePattern tp)
				    | RegExp.RE_EMPTY => 0
				    | RegExp.RE_NULL => 0
				    | RegExp.RE_OPT re => (forestPattern re)
				    | RegExp.RE_PLUS re => (forestPattern re)
				    | RegExp.RE_REP re => (forestPattern re)
				    | RegExp.RE_SEQ (re1,re2) => 
					(forestPattern re1) + (forestPattern re2)
				    | RegExp.RE_UNDER => 0
				  and treePattern tp = 
				    case tp of
				      TP (_,cp) => completePath cp
				    | HASH => 0
				  and incompletePath ip =
				    case ip of 
				      RegExp1.RE_ALT (re1,re2) =>
					(incompletePath re1) + (incompletePath re2)
				    | RegExp1.RE_BASIC tnp => targetNodePattern tnp
				    | RegExp1.RE_DEEP tnp => targetNodePattern tnp
				    | RegExp1.RE_EMPTY => 0
				    | RegExp1.RE_NULL => 0
				    | RegExp1.RE_OPT re => incompletePath re
				    | RegExp1.RE_PLUS re => incompletePath re
				    | RegExp1.RE_REP re => incompletePath re
				    | RegExp1.RE_SEQ (re1,re2) =>
					(incompletePath re1) + (incompletePath re2)
				    | RegExp1.RE_UNDER => 0

				  fun topTreePattern (ips,tnp) = 
				    (List.foldl (fn ((b,(b1,ip)),c) => (incompletePath ip)+c) 0 ips)
				    +(targetNodePattern tnp)				    
				    
				in
				  List.foldl
				  (fn ((qlist,(b,tp)),c) =>
				   c + (topTreePattern tp) +
				   (List.foldl
				    (fn ((_,fp),c) => c + (forestPattern fp))
				    0 qlist)) 0 pat
				end

			      val actions = Vector.fromList actions

			      val secondaries = Vector.map countSecondaries parsedPatterns

			      val dynPatternsActions = Vector.mapi
				(fn (i,action) =>
				 let
				   val (dynSelects,action) =
				     ((WriteActions.writeXslAction dtd 
				       Names.functionName 
				       Names.docArgument 
				       (SOME (Vector.sub (secondaries,i)))
				       action))
				   val dynSelects = 
				     List.map 
				     (PatternParser.parseUniCharVectorPattern docDtd)
				     dynSelects
				 in
				   (dynSelects,action)
				 end)
				actions
			    in
			      ((stringTables,tables,preActs,toInclude,toOpen,
				fxslGlobals,yetAnotherArguments),defaultAction,
			       docDtd,stringPatterns,parsedPatterns,dynPatternsActions)
			    end
			  else raise Exceptions.SpecificationError (pos,"fxt:spec element expected")
				| _ => doit (i+1)
		    end
	      in
		doit 0
	      end
	  end


	val ((stringTables,tables,preActs,toInclude,
	      toOpen,fxslGlobals,yetAnotherArguments),default,
	     dtd,stringPatterns,parsedPatterns,dynPatternsActions) =
	  parseTFSpec uriOption

	val actions = Vector.map #2 dynPatternsActions

	val ps = 
	  Vector.mapi 
	  (fn (i,pat) => 
	   (pat,
	    let
	      val dynSelects = #1 (Vector.sub (dynPatternsActions,i))
	    in
	      dynSelects
	    end))
	  parsedPatterns

	val tab = GramTables.initXTable()
	val (g,matchSpecs,intervals) = PatGrammar.Patterns2Grammar tab ps

	(*val _ = print (GramString.Grammar2tdString (tab,dtd) (g,hd matchSpecs))*)
	  
	val (f as (g1,_,_,_),(xDontCare,_,_)) = GramFlatten.flatten' g

	(*val _ = print (GramString.FlatGrammar2tdString (tab,dtd) g1)*)

	val (_,passes) = GramAnalyze.multAnalyze (f,matchSpecs,xDontCare,intervals)

	  
	val tables = Vector.fromList tables
	val ps =
	  Vector.map 
	  (fn (_,pat,_) => 
	   (PatternParser.parseUniCharVectorPattern dtd pat,nil))
	  tables
	val keys = Vector.map (fn (name,_,toString) => (name,toString)) tables
	val (g,matchSpecs,intervals) = PatGrammar.Patterns2Grammar (GramTables.initXTable()) ps
	val (f,(xDontCare,_,_)) = GramFlatten.flatten' g
	val (_,key_passes) = GramAnalyze.multAnalyze (f,matchSpecs,xDontCare,intervals)

	val tables = Vector.fromList stringTables
	val ps =
	  Vector.map 
	  (fn (_,pat,_,_) => 
	   (PatternParser.parseUniCharVectorPattern dtd pat,nil)) 
	  tables
	val stringKeys = Vector.map (fn (name,_,toString,toItem) => (name,toString,toItem)) tables
	val (g,matchSpecs,intervals) = PatGrammar.Patterns2Grammar (GramTables.initXTable()) ps
	val (f,(xDontCare,_,_)) = GramFlatten.flatten' g
	val (_,stringKey_passes) = GramAnalyze.multAnalyze (f,matchSpecs,xDontCare,intervals)
      in
	((stringKeys,stringKey_passes),(keys,key_passes),(rev preActs),
	 stringPatterns,dtd,toInclude,toOpen,fxslGlobals,
	 rev yetAnotherArguments,passes,actions,default)
      end
  end
