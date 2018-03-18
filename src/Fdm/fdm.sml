structure Fdm : FDM =
  struct

    val dtd = Transform.dtdRef

    open MatchData DocDtd

    type Attribute = MatchData.AttSpc
    datatype Tree =
      ELEM of int * Attribute vector * Forest
    | TEXT of UniChar.Vector
    | PI of UniChar.Vector * Forest
    withtype Forest = Tree vector
    val emptyForest:Forest = Vector.fromList nil

    open Vector
      
    val emptyTree = TEXT UniChar.nullVector

    val toList = Util.vector2List

    fun concatForests v = Vector.concat (toList v)

    fun lengthPred pred f = Vector.foldl (fn (t,i) => if (pred t) then i+1 else i) 0 f

    fun sort rel forest =
      (*ListMergeSort.sort does not preserve 
       the order of elements with equal keys*)
      Vector.fromList (ListMergeSort.sort rel (toList forest))
      (*let
	val a = Array.array(Vector.length forest,TEXT #[])
	val _ = Array.copyVec {src=forest,si=0,len=NONE,dst=a,di=0}
	val _ = ArrayQSort.sort rel a
      in
	Array.extract (a,0,NONE)
      end*)

    fun deleteAll pred f = 
      Vector.fromList 
      (rev (Vector.foldl 
	    (fn (t,l) => if (pred t) then l else (t::l)) 
	    [] f))
    fun filter pred f = 
      Vector.fromList 
      (rev (Vector.foldl 
	    (fn (t,l) => if (pred t) then (t::l) else l) 
	    [] f))
    fun filterFirst pred f =
      let
	val l = Vector.length f
	fun doit i =
	  if i >= l then emptyTree
	  else 
	    let
	      val t = Vector.sub(f,i)
	    in
	      if (pred t) then t
	      else (doit (i+1))
	    end
      in
	doit 0
      end
    fun deleteFirstN n pred f =
      let
	val (l,deleted) = 
	  Vector.foldl
	  (fn (t,(l,i)) =>
	   if (i<n) then
	     if (pred t) then (l,i+1)
	     else (t::l,i)
	   else (t::l,i))
	  ([],0) f
      in
	(deleted,Vector.fromList (rev l))
      end
    fun deleteFirst pred f =
      let 
	val (l,found) = 
	  Vector.foldl
	   (fn (t,(l,e)) => 
	    if e then ((t::l),true)
	    else if (pred t) then (l,true)
		 else (t::l,false))
	   ([],false) f
      in
	(found,Vector.fromList (rev l))
      end
    fun deleteIth n f =
      Vector.fromList 
      (rev (Vector.foldli 
	    (fn (i,t,l) => if (i=n) then l
			   else t::l)
	    [] (f,0,NONE)))
    fun deleteLast pred f =
      let
	fun findLast f pred =
	  Vector.foldli
	  (fn (i,t,j) => if (pred t) then SOME i
			 else j)
	  NONE (f,0,NONE)
      in
	case (findLast f pred) of
	  SOME i => (true,deleteIth i f)
	| NONE => (false,f)
      end
    fun vectorRev f = Vector.fromList (List.rev (toList f))
    fun deleteLastN n pred f =
      let
	val (deleted,nf) =
	  deleteFirstN n pred (vectorRev f)
      in
	(deleted,vectorRev nf)
      end

    fun isElement tree =
      case tree of
	ELEM _ => true
      | _ => false

    fun getElementType t =  
      case t of
	ELEM(i,_,_) =>  Unicode.Data2Vector (Index2Element (!dtd) i)
      |_ => raise (Exceptions.RuntimeError "getElementType: not an element")

    fun hasElementType name t = 
      if isElement t then 
	((getElementType t) = name)
      else false
	
    fun text name =
      TEXT  name

    fun getAttributes tree =
      case tree of
	ELEM(_,a,_) => a
      | _ => raise (Exceptions.RuntimeError "getAttributes: not an element")

    fun hasAttribute name t = 
      let
	val attrs = getAttributes t
	val name = Element2Index (!dtd) (Unicode.Vector2Data name)
	fun doit ((name1,_),found) = 
	  found orelse name=name1
      in
	Vector.foldl doit false attrs
      end

    fun getAttribute atr t = 
      let
	val attrs = getAttributes t
	val l = Vector.length attrs
	val atrI = Element2Index (!dtd) (Unicode.Vector2Data atr)
	fun doit i =
	  if i >= l then raise (Exceptions.RuntimeError "getAttribute: no such attribute")
	  else
	    let 
	      val (name,value) = Vector.sub (attrs,i)
	    in
	      if name=atrI then value
	      else doit (i+1)
	    end
      in
	doit 0
      end

    val myVector2String = Util.myVector2String

    fun element e attributes sons =
	ELEM(Element2Index (!dtd) (Unicode.Vector2Data e),attributes,sons)

    fun processingInstruction processor instruction =
      let
	val content = #[TEXT instruction]
      in
	PI(processor,content)
      end

    fun getElement tree =
      case tree of
	ELEM (e,a,s) => (Unicode.Data2Vector (Index2Element (!dtd) e),a,s)
      | _ => raise (Exceptions.RuntimeError "getElement: not an element")

    fun isText tree =
      case tree of
	TEXT _ => true
      | _ => false

    fun getText tree =
      case tree of
	TEXT t => t
      | _ => raise (Exceptions.RuntimeError "getText: not text")

    (*fun getElement tree =
      case tree of
	ELEM (i,atts,sons) => 
	  ( i,atts,sons)
      | _ => raise (Exceptions.RuntimeError "getElement: not an element")*)

    fun hasTextContent tree =
      case tree of
	ELEM (_,_,#[TEXT t]) => true
      | TEXT t => true 
      | _ => false

    fun getTextContent tree =
      case tree of
	ELEM (_,_,ss) => Vector.foldl (fn (t,a) => Unicode.concatVectors (a,(getText t))) Unicode.nullVector ss
      | TEXT t =>  t 
      | _ => raise (Exceptions.RuntimeError "getTextContent: no text content")

    fun isProcessingInstruction tree =
      case tree of
	PI _ => true
      | _ => false 

    fun getProcessingInstruction tree =
      case tree of
	PI (processor,#[TEXT content]) => (processor,content)
      | _ => raise (Exceptions.RuntimeError "getProcessingInstruction: not a processing instruction")

    fun getElementName tree =
      case tree of
	ELEM(i,_,_) => ( i)
      |_ => raise (Exceptions.RuntimeError "getElementName: not an element")


    fun makeAttribute name value = (Element2Index (!dtd) (Unicode.Vector2Data name),value)

    fun getAttributeValue (_,v) = v

    fun getAttributeName (i,_) = Unicode.Data2Vector (Index2Element (!dtd) i)

   fun addAttribute atts name value = (*overwrite if already present*)
     let
       val att as (name,value) = makeAttribute name value
       fun doit (attr as (name1,_),(found,attrs)) =
	 if name=name1 then
	   if found then raise Exceptions.InternalError (*FDM: duplicate attribute*)
	   else (true,att::attrs)
	 else
	   (found,attr::attrs)
       val (found,attrs) = Vector.foldr doit (false,[]) atts
       val attrs =
	 if found then attrs
	 else att::attrs
     in
       Vector.fromList attrs
     end

   fun addAttributes atts1 atts2 =
     Vector.foldl 
     (fn (attr,acc) => 
      addAttribute acc (getAttributeName attr) (getAttributeValue attr))
     atts1
     atts2

    fun insertAttribute t name value =
      case t of
	ELEM(el,atts,sons) =>
	  let
	    val newAtts = addAttribute atts name value
	  in
	    ELEM(el,newAtts,sons)
	  end
      | _ => raise (Exceptions.RuntimeError "insertAttribute: not an element")

    fun replaceAttribute atts name value =
      let
	val i = Element2Index (!dtd) (Unicode.Vector2Data name)
      in
	Vector.fromList
	(Vector.foldr
	 (fn (att as (attName,_),l) =>
	  if attName = i then (i,value)::l
	  else att::l) nil atts)
      end
    
    fun changeAttribute t name value =
      case t of
	ELEM(el,atts,sons) =>
	  let
	    val newAtts = replaceAttribute atts name value
	  in
	    ELEM(el,newAtts,sons)
	  end
      | _ => raise (Exceptions.RuntimeError "changeAttribute: not an element")

    fun deleteAttribute atts name =
      let
	val i = Element2Index (!dtd) (Unicode.Vector2Data name)
      in
	Vector.fromList
	(Vector.foldr
	 (fn (att as (iA,v),l) =>
	  if iA = i then l
	  else att::l) nil atts)
      end


    fun removeAttribute t name =
      case t of
	ELEM(el,atts,sons) =>
	  let
	    val newAtts = deleteAttribute atts name
	  in
	    ELEM(el,newAtts,sons)
	  end
      | _ => raise (Exceptions.RuntimeError "removeAttribute: not an element")

    fun elementIs name t =
      if isElement t then 
	((getElementName t) = name)
      else false

    exception InternalError of string

    fun sons tree =
      case tree of
	ELEM (_,_,content) =>  content
      | TEXT _ => vector[]
      | PI (_,content) => content

    fun selectSon tree name =
      let
	val sons = sons tree
	val l = Vector.length sons
	fun doit ss i =
	  if i >= l then raise (Exceptions.RuntimeError "selectSon: no such son")
	  else 
	    let
	      val s = Vector.sub (ss,i)
	    in
	      if (isElement s) andalso (getElementName s) = name then s
	      else doit ss (i+1)
	    end
      in
	doit sons 0
      end


    fun openFile fname encodingNameOption =
      case encodingNameOption of
	SOME enc => Encode.encOpenFile(fname,Encoding.NOENC,enc)
      | NONE => Encode.encOpenFile(fname,Encoding.NOENC, "UTF-8")

    fun closeFile f = Encode.encCloseFile f

    local open 
      Unicode Encode Encoding Errors UtilError UtilString
    in
      fun putTree1 f t =
	let
	  fun putChar(f,c) = encPutChar(f,c) 
	    handle EncodeError(fname,msg) => encAdapt(f,fname) before  (TextIO.print ("Encoding error"))
	      
	  fun putData(f,cs) = List.foldl (fn (c,f) => putChar(f,c)) f cs
	  fun putVector(f,cv) = Vector.foldl (fn (c,f) => putChar(f,c)) f cv
	  fun putString(f,str) = putData(f,String2Data str)
	  fun putNl f = putChar(f,0wx0A)
	  fun putBlank f = putChar(f,0wx20)
	  val hexDigits = Vector.tabulate(16,fn i => Word.fromInt((if i<10 then 48 else 55)+i))
	  fun hexDigit n = Unsafe.Vector.sub(hexDigits,Word.toInt n)
	  fun charRefSeq c =
	    if c=0wx00 then [0wx26,0wx23,0wx78,0wx30,0wx3b] (* "&#x0;" *)
	    else let fun mk_hex yet n = if n=0w0 then yet
					else mk_hex (hexDigit(n mod 0w16)::yet) (n div 0w16)
		 in 0wx26::0wx23::0wx78::mk_hex [0wx3b] c
		 end
          fun putCharRef(f,c) = putData(f,charRefSeq c)
	    
	  fun putDataChar(f,c) = 
	    case c 
	      of 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
	    | 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
	    | 0wx3E => putData(f,[0wx26,0wx67,0wx74,0wx3b]) (* "&gt;" *)
	    | _ => if encValidChar(f,c) then putChar(f,c) else putCharRef(f,c)

	  fun putLiteralDataChar(f,c) = 
	    if encValidChar(f,c) then putChar(f,c) else putCharRef(f,c)
		
	  fun putDataVector putDataChar (f,cv) =
	    Vector.foldl (fn (c,f) => putDataChar(f,c)) f cv

	  val putDataVector = 
	    putDataVector
	    (if !FxtOptions.O_ESCAPE then putLiteralDataChar
	     else putDataChar)

	  fun putAttValue (f,cv,q) = 
	    let 
	      fun putOne(c,f) = 
		case c 
		  of 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
		| 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
		| _ => if c<>q andalso encValidChar(f,c) then putChar(f,c) else putCharRef(f,c)
	      val f1 = putChar(f,q)
	      val f2 = Vector.foldl putOne f1 cv
	      val f3 = putChar(f2,q)
	    in f3
	    end
	in
	  case t
	    of 
	      TEXT txt => putDataVector(f,txt)
	    | PI(target,content) => 
	      let val f1 = putString (f,"<?")
		val f2 = putVector (f1,target)
		val f3 = if Vector.length content=0 then f2
			 else Vector.foldl 
			   (fn (t,f) => putTree1 f t) 
			   (putBlank f2) content
		val f4 = putString (f3,"?>")
	      in f4
	      end
	    | ELEM(i,atts,content) => 
	      let 
		val elem = Index2Element (!dtd) i
		val f1 = putString (f,"<")
		val f2 = (putData(f1,elem) handle e => raise e)
		val f3 = Vector.foldl
		  (fn ((a,cv),f) 
		   => let 
			val f1 = putBlank f
			val f2 = putData(f1,Index2Element (!dtd) a)
			val f3 = putString(f2,"=")
			val f4 = putAttValue(f3,cv,0wx27)
		      in 
			f4
		      end) f2 atts
		val f4 = putString (f3,">")
		val f5 = Vector.foldl 
		  (fn (t,f) => putTree1 f t) f4 content
		val f6 = putString (f5,"</")
		val f7 = (putData(f6,elem) handle e => raise e)
		val f8 = putString(f7,">")
	      in f8
	      end
	end
    end

    fun putTree t fname encodingNameOption =
      closeFile (putTree1 (openFile fname encodingNameOption) t)
 

    fun putForest f fname encoding =
      (closeFile 
       (Vector.foldl (fn (r,f) => (putTree1 f r)) 
	(openFile fname encoding) f))
       handle _ => raise Exceptions.RuntimeError ("Could not open "^fname^" for output in the specified encoding")

    local open 
      Unicode Errors UtilError UtilString
    in
      fun Tree2UnicodeData t = (*to optimize*)
	let
	  fun doit f =
	    let
	      fun putChar(f,c) = f@[c]
	      fun putData(f,cs) = List.foldl (fn (c,f) => putChar(f,c)) f cs
	      fun putVector(f,cv) = Vector.foldl (fn (c,f) => putChar(f,c)) f cv
	      fun putString(f,str) = putData(f,String2Data str)
	      fun putNl f = putChar(f,0wx0A)
	      fun putBlank f = putChar(f,0wx20)
	      val hexDigits = Vector.tabulate(16,fn i => Word.fromInt((if i<10 then 48 else 55)+i))
	      fun hexDigit n = Unsafe.Vector.sub(hexDigits,Word.toInt n)
	      fun charRefSeq c =
		if c=0wx00 then [0wx26,0wx23,0wx78,0wx30,0wx3b] (* "&#x0;" *)
		else let fun mk_hex yet n = if n=0w0 then yet
					    else mk_hex (hexDigit(n mod 0w16)::yet) (n div 0w16)
		     in 0wx26::0wx23::0wx78::mk_hex [0wx3b] c
		     end
	      fun putCharRef(f,c) = putData(f,charRefSeq c)
	    
	      fun putDataChar(f,c) = 
		case c 
		  of 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
		| 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
		| 0wx3E => putData(f,[0wx26,0wx67,0wx74,0wx3b]) (* "&gt;" *)
		| _ => if (EncodeMisc.validCharAscii c) then putChar(f,c) else putCharRef(f,c)

	      fun putLiteralDataChar(f,c) = 
		if (EncodeMisc.validCharAscii c) then putChar(f,c) else putCharRef(f,c)
		
	      fun putDataVector putDataChar (f,cv) =
		Vector.foldl (fn (c,f) => putDataChar(f,c)) f cv

	      val putDataVector = 
		putDataVector
		(if !FxtOptions.O_ESCAPE then putLiteralDataChar
		 else putDataChar)

	      fun putAttValue (f,cv,q) = 
		let 
		  fun putOne(c,f) = 
		    case c 
		      of 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
		    | 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
		    | _ => if c<>q andalso (EncodeMisc.validCharAscii c) then putChar(f,c) else putCharRef(f,c)
		  val f1 = putChar(f,q)
		  val f2 = Vector.foldl putOne f1 cv
		  val f3 = putChar(f2,q)
		in f3
		end
	    in
	      case t
		of 
		  TEXT txt => putDataVector(f,txt)
		| PI(target,content) => 
		  let val f1 = putString (f,"<?")
		    val f2 = putVector (f1,target)
		    val f3 = if Vector.length content=0 then f2
			     else Vector.foldl 
			       (fn (t,f) => (f@(Tree2UnicodeData t)))
			       (putBlank f2) content
		    val f4 = putString (f3,"?>")
		  in f4
		  end
		| ELEM(i,atts,content) => 
		  let 
		    val elem = Index2Element (!dtd) i
		    val f1 = putString (f,"<")
		    val f2 = (putData(f1,elem) handle e => raise e)
		    val f3 = Vector.foldl
		      (fn ((a,cv),f) 
		       => let 
			    val f1 = putBlank f
			    val f2 = putData(f1,Index2Element (!dtd) a)
			    val f3 = putString(f2,"=")
			    val f4 = putAttValue(f3,cv,0wx27)
			  in 
			    f4
			  end) f2 atts
		    val f4 = putString (f3,">")
		    val f5 = Vector.foldl 
		      (fn (t,f) => (f@(Tree2UnicodeData t))) f4 content
		    val f6 = putString (f5,"</")
		    val f7 = (putData(f6,elem) handle e => raise e)
		    val f8 = putString(f7,">")
		  in f8
		  end
	    end
	in
	  (doit [])
	end
    end

    val Tree2UnicodeVector = Unicode.Data2Vector o Tree2UnicodeData

    fun printDocTree docTree =
      closeFile (putTree1 (openFile "-" NONE) docTree)

    fun printTree t =
      putTree t "-" NONE

    fun printForest f =
      putForest f "-" NONE

    fun printDocTreeList docTreeList = List.app printDocTree docTreeList

    fun countTreeNodes t =
      case t of
	ELEM (_,_,sons) => 1 + (countForestNodes sons)
      | TEXT _ => 1
      | PI _ => 2

    and countForestNodes f = Vector.foldl (fn (t,c) => (countTreeNodes t)+c) 0 f


  end