structure MatchDataTreeDom =
  struct

    open MatchData

    exception NO_TEXT_CONTENT

    val Vector2String = Util.myVector2String

    fun getAttributes t =
      case t of
	T_ELEM (_,atts,_) => atts

    fun sons t =
      case t of
	T_ELEM (_,_,c) => c

    fun getAttribute dtd attIdx (pos,t) =
      case t of 
	T_ELEM (elIdx,atts,_) =>
	  let
	    fun doit i =
	      if i < Vector.length atts then
		let
		  val (idx,value) = Vector.sub(atts,i)
		in
		  if idx = attIdx then value
		  else doit (i+1)
		end
	      else
		raise Exceptions.SpecificationError 
		  (pos,
		   ((UniChar.Data2String (Dtd.Index2AttNot dtd attIdx))^
		    " attribute required in "
		    ^(UniChar.Data2String (Dtd.Index2Element dtd elIdx))^
		    " element"))
	  in
	    doit 0
	  end

    fun hasAttribute attIdx t =
      case t of
	T_ELEM (_,atts,_) =>
	  let 
	    fun doit i =
	      if i < Vector.length atts then
		let
		  val (idx,value) = Vector.sub(atts,i)
		in
		  if idx = attIdx then true
		  else doit (i+1)
		end
	      else false
	  in
	    doit 0
	  end

    fun isElement t =
      case t of
	T_ELEM _ => true
      | _ => false

    fun getElementIdx t =
      case t of
	T_ELEM (i,_,_) => i

    fun isProcessingInstruction t =
      case t of
	T_PI _ => true
      | _ => false

    fun isText t =
      case t of
	T_TEXT _ => true
      | _ => false

    fun getElement dtd elIdx = 
      UniChar.Data2Vector (Dtd.Index2Element dtd elIdx)

    fun getAttributeName dtd (attIdx,_) = 
      UniChar.Data2Vector (Dtd.Index2AttNot dtd attIdx)

    fun getAttributeValue (_,value) = value

    fun getElementType dtd t =
      case t of
	T_ELEM (elIdx,_,_) => getElement dtd elIdx

     fun getProcessingInstruction t =
	case t of
	  T_PI (processor, #[(_,T_TEXT text)]) => 
	    (processor,text)

     fun getText t =
       case t of
	 T_TEXT txt => txt
       | _  => raise NO_TEXT_CONTENT

    fun getTextContent tree =
      case tree of
	T_ELEM (_,_,ss) => Vector.foldl (fn ((pos,t),a) => Unicode.concatVectors (a,(getText t))) Unicode.nullVector ss
      | T_TEXT t => t
      | _ => raise NO_TEXT_CONTENT


  end