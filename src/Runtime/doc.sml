structure Doc =
  struct
    type Doc = int * (Fdm.Tree * IntSet.set IntListMap.map IntListMap.map * int list) array

    fun matchFile passes xmlFileName =
      let
	val inputFile =
	  if xmlFileName="-" then NONE
	  else SOME (Uri.String2Uri xmlFileName)
	val timer = Timer.startRealTimer ()
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Parsing...")
	  else ()
	val (noNodes,_,docTreeVector) = 
	  MultDocParser.parseDoc (!Transform.dtdRef) inputFile
	val time = Time.toMilliseconds (Timer.checkRealTimer timer)
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"took "^
			   (LargeInt.toString time)^" milliseconds.\n")
	  else ()
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Matching (static) - ")
	  else ()
	val timer = Timer.startRealTimer ()
	fun hasSec doMatch = 
	  Vector.foldl
	  (fn ((doMatch,doMatch'),b) =>
	   b orelse ((Vector.length doMatch') > 0)) false doMatch

	val (siblings,matchTable) = 
	  case passes of 
	    MatchData.SINGLE 
	    (y as 
	     (yVec,sigVec,rhsVec,tpVec,
	      otherRules,otherY0s,q0,doMatch,ys_of_y,regExpNameRules,
	      preForms,couldMatchAfterTag,couldMatchAfterAtt,postForms))
	    => 
	    if hasSec doMatch then
	      let
		val _ = 
		  if !FxtOptions.O_PROFILE then
		    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"One pass, with binary patterns...")
		  else ()
	      in
		MatchBinOnePass.match y (noNodes,docTreeVector) (!Transform.dtdRef)
	      end
	    else
	      let
		val _ = 
		  if !FxtOptions.O_PROFILE then
		    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"One pass, only primary patterns...")
		  else ()
	      in
		MatchSimpleOnePass.match y (noNodes,docTreeVector) (!Transform.dtdRef)
	      end
	  | MatchData.DOUBLE 
	      (y as
	       (sigVec,yVec,rhsVec,tpVec,otherRules,
		otherY0s,qr0,ql0info,doMatch,
		incomingXVec,regExpNameRules,ys_of_y,form))
	      => 
	      if hasSec doMatch then
		let
		  val _ = 
		    if !FxtOptions.O_PROFILE then
		      TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Two passes, with secondary patterns...")
		    else ()
		in
		  MatchBinTwoPasses.match y (noNodes,docTreeVector) (!Transform.dtdRef)
		end
	      else		
		let
		  val _ = 
		    if !FxtOptions.O_PROFILE then
		      TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Two passes, only primary patterns...")
		    else ()
		in
		  MatchSimpleTwoPasses.match y (noNodes,docTreeVector) (!Transform.dtdRef)
		end
	val time = Time.toMilliseconds (Timer.checkRealTimer timer)
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"took "^
			   (LargeInt.toString time)^" milliseconds.\n")
	  else ()	  
      in
	List.map (fn i => (i,matchTable)) siblings
      end


    fun matchFdm passes fdmForest =
      let
	val noNodes = Fdm.countForestNodes fdmForest
	val timer = Timer.startRealTimer ()
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Matching for tables...")
	  else ()
	val (siblings,matchTable) = 
	  case passes of 
	    MatchData.SINGLE y => 
	      MatchBinOnePass.match y (noNodes,fdmForest) (!Transform.dtdRef)
	  | MatchData.DOUBLE y => 
	      MatchBinTwoPasses.match y (noNodes,fdmForest) (!Transform.dtdRef)
	val res = List.map (fn i => (i,matchTable)) siblings
	val time = Time.toMilliseconds (Timer.checkRealTimer timer)
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"took "^
			   (LargeInt.toString time)^" milliseconds.\n")
	  else ()	  

      in
	res
      end


    fun docSons (i,t) =
      let
	val (_,_,sons) = Array.sub (t,i)
      in
	List.map (fn i => (i,t)) sons
      end

    fun docTree (i,t) =
      let
	val (t,_,_) = Array.sub (t,i)
      in
	t
      end

    fun matchedPattern (i,t) =
      let
	val (_,matchMap,_) = Array.sub (t,i)
      in
	case IntListMap.firsti matchMap of (* the first in map is the first in spec *)
	  SOME (i,_) => i
	| NONE => ~7 (* the default action will catch this case*)
      end
    
    fun secondaries (i,t) secOrd =
      let
	val (_,matchMap,_) = Array.sub (t,i)
	val secOrd = 
	  case Int.fromString secOrd of
	    SOME i => (i-1) (* first ist specified as one, its index is however 0 *)
	  | NONE => 0 (*if secOrd is not a number the first secondary in the pattern is chosen*)
      in
	case IntListMap.first matchMap of (* only the secondaries of the first matched pattern are of interest *)
	  SOME secondaries => 
	    (case IntListMap.find (secondaries,secOrd) of
	       SOME secs => List.map (fn i => (i,t)) (IntSet.listItems secs) (* in document order *)
	     | NONE => nil (* no secondaries => nothing is selected *))
	| NONE => nil
      end
    
    fun docMap f docs = Vector.concat (List.map f docs)
    fun docMapFilter f p docs = docMap f (List.filter (p o docTree) docs)

    fun matchTable docList =
      case docList of
	nil => Array.fromList nil
      | (_,t)::r => t

    fun dynamicSelect pattern (doc as (i,table)) =
      let
	val (tree,_,_) = Array.sub (table,i)
	val tab = GramTables.initXTable()
	val pos = ("",0,0)
	val pattern  =  
	  PatternParser.parseUniCharVectorPattern (!Transform.dtdRef) (pos,pattern)
	val (g,targets,intervals) = PatGrammar.Patterns2Grammar tab #[(pattern,nil)]
	val (f as (g1,_,_,_),(xDontCare,_,_)) = GramFlatten.flatten' g
	val (_,passes) = GramAnalyze.multAnalyze (f,targets,xDontCare,intervals)
	val [(_,dynTable)] = matchFdm passes #[tree]
	val matches =
	  Array.foldri
	  (fn (j,(_,matchMap,_),matches) =>
	   case IntListMap.firsti matchMap of (* the first in map is the first in spec *)
	     SOME (0,_) => ((j+i),table)::matches
	   | _ => matches) nil dynTable
      in
	matches
      end


  end
