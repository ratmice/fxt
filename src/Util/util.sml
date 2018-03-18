structure Util =
  struct
    fun printUsage stream prog usage header = 
      let 
	val KEY_WIDTH = 30
	val LINE_WIDTH = 80
	val EMPTY_KEY = UtilString.nBlanks KEY_WIDTH
	fun appendKeys col nil = if col>KEY_WIDTH then "\n"^EMPTY_KEY
				 else UtilString.nBlanks (KEY_WIDTH-col)
	  | appendKeys col [key] = key^" "^appendKeys (col+1+String.size key) nil
	  | appendKeys col (key::keys) = let val col1 = col+2+String.size key
					 in if col1>KEY_WIDTH 
					      then key^",\n"^appendKeys 0 keys
					    else key^", "^appendKeys col1 keys
					 end
	fun makeKey keylist = appendKeys 0 keylist
	val makeText = UtilString.breakLines(LINE_WIDTH-KEY_WIDTH)
	fun format (keylist,text) = 
	  let val key = makeKey keylist
	  in case makeText text
	    of nil => [key]
	  | line::lines => key^line::map (fn line => EMPTY_KEY^line) lines
	  end
	val _ = app (fn x => TextIO.output(stream,x))
	  ["Usage: ",prog," [option ...] "^header^"\n","where option is one of:\n\n"]
	val _ = app (fn item => app (fn x => TextIO.output(stream,x^"\n")) 
		     (case item 
			of Options.U_SEP => [""]
		      | Options.U_TITLE txt => ["",txt]
		      | Options.U_ITEM option => format option)) usage
      in ()
      end


    local 
      fun doit l =
	case l of
	  c::r => if (Char.isSpace c) then doit r
		  else l
	| _ => l
    in
      fun skipLeadingSpaces s =
	String.implode (doit (String.explode s))
      fun skipTrailingSpaces s =
	String.implode (rev (doit (rev (String.explode s))))
      fun trimSpaces s = (skipTrailingSpaces (skipLeadingSpaces s))
    end

    local 
      fun doit l =
	case l of
	  c::r => if (UniClasses.isS c) then doit r
		  else l
	| _ => l
    in
      fun skipLeadingUniCharSpaces v =
	doit v
      fun skipTrailingUniCharSpaces v =
	rev (doit (rev v))
      fun trimUniCharSpaces v = (skipTrailingUniCharSpaces (skipLeadingUniCharSpaces v))
    end


    fun makeIdentifier s =
      case String.explode s of
	[] => "fxt"
      | l =>
	  let
	    fun doit l =
	      case l of
		[] => NONE
	      | h::r =>
		  if (h >= #"A" andalso h <= #"Z") orelse (h >= #"a" andalso h <= #"z") then SOME (h,r)
		  else doit r
	  in
	    case doit l of
	      NONE => "fxt"
	    | SOME (first,r) => 
		let
		  fun doit1 l a =
		    case l of
		      [] => rev a
		    | h::r =>
			if (h >= #"A" andalso h <= #"Z") orelse 
			  (h >= #"a" andalso h <= #"z") orelse
			  (h >= #"0" andalso h <= #"9") orelse
			   h = (#"_") orelse h = (#"'") then doit1 r (h::a)
			else doit1 r a
		in
		  String.implode (first::(doit1 r []))
		end
	  end

    

    fun capitalizeFirst s =
      case (String.explode s) of
	c::r => String.implode ((Char.toUpper c)::r)
      | _ => s

    fun isSpace v = 
      let
	fun doit i =
	  if (i < (Vector.length v)) then
	    if UniClasses.isS (Vector.sub(v,i)) then doit (i+1)
	    else false
	  else
	    true
      in
	doit 0
      end
      

    fun vector2List v = Vector.foldr (fn (e,l) => (e::l)) [] v

    fun vectorPairUnzip v =
      let
	val (l1,l2) = Vector.foldr (fn ((e1,e2),(l1,l2)) => (e1::l1,e2::l2)) ([],[]) v
      in
	(Vector.fromList l1, Vector.fromList l2)
      end


    fun vectorAppend v1 v2 = Vector.concat [v1,v2]

    fun myChar2String c = 
      if c<0wx100 then String.implode [UniChar.Char2char c]
      else UniChar.Char2Uni c

    fun myData2String cs = String.concat (map myChar2String cs)

    fun myVector2String vec = 
	myData2String (UniChar.Vector2Data vec)

    val Vector2String = myVector2String


    fun String2Int s =
      case (Int.fromString s) of
	SOME i => i
      | _ => raise Exceptions.RuntimeError "Util.String2Int: bad integer format"

    fun copyFile fileName indent = 
	let
	    val inputStream = TextIO.openIn fileName
	    fun doit () = 
		let
		    val line = TextIO.inputLine inputStream
		in
		    if line = "" then TextIO.closeIn inputStream
		    else ((print (indent^line));doit ())
		end
	in
	    doit()
	end

    fun insertFile name = copyFile name ""

    fun File2String fileName = 
	let
	    val inputStream = TextIO.openIn fileName
	    fun doit s = 
		let
		    val line = TextIO.inputLine inputStream
		in
		    if line = "" then ((TextIO.closeIn inputStream);s)
		    else doit (s^line)
		end
	in
	    doit ""
	end

    fun encPutString file s =
      Vector.foldl (fn (c,f) => Encode.encPutChar (f,c)) file (UniChar.String2Vector s)

  end	