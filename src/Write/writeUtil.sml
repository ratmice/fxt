structure WriteUtil =
  struct
    fun stringIndent s indent =
      let
	val sl = String.tokens (fn c => (c = (#"\n"))) s
	fun doit l a=
	  case l of
	    [] => a
	  | s::r => doit r (a^(indent^s^"\n"))
      in
	doit sl ""
      end

    fun writeString s = 
      let
	val s = String.translate 
	  (fn c => 
	   if c = (#"\"") then "\\\"" else (Char.toString c)) s
      in
	"\""^s^"\"" 
      end

    fun writeOption writeContent opt =
	   case opt of
	     SOME c => ("(SOME "^(writeContent c)^")")
      | NONE => ("NONE")

    fun writePair indent newLine write1 write2 (e1,e2) =
      indent^
      "("^(write1 e1)^","^
      (if newLine then ("\n"^indent) else "")^
      (write2 e2)^")"

    fun writeTriplet write1 write2 write3 (e1,e2,e3) =
      ("("^(write1 e1)^","^(write2 e2)^","^(write2 e3)^")")

    fun writeVector indent newLine writeElement v = 
      let
	val l = Vector.length v
	fun doit i s =
	  if (l > i) then doit (i+1) (s^","^
				      (if newLine then "\n" else "")
					 ^((writeElement (Vector.sub(v,i)))))
	  else s
      in
	(indent^
	 "(vector["^
	 (if newLine then "\n" else "")^
	    (if (l>=1) then (writeElement (Vector.sub(v,0)))
	     else "")^
	 (if (l>=2) then (doit 1 "")
	  else "")^
	    "])"
	    )
      end

    fun writeVectorAsList writeElement v =
      let
	val l = Vector.length v
	fun doit i s =
	  if (l > i) then 
	    doit (i+1) (s^","^((writeElement (Vector.sub(v,i)))))
	  else s
      in
	 "["^
	 (if (l>=1) then (writeElement (Vector.sub(v,0)))
	  else "")^
	 (if (l>=2) then (doit 1 "")
	  else "")^
	  "]"
      end

    fun writeList (left,separator,right) indent newLine writeElement l = 
      let
	fun doit l s =
	  case l of
	    h::r => doit r (s^separator^
			    (if newLine then "\n" else "")^
			       indent^
			       (writeElement h))
	  | _ => s^(if newLine then "\n" else "")
      in
	(indent^left^
	 (if newLine then "\n" else "")^
	 (case l of
	    h::r => indent^(writeElement h)
	  | [] => "")^
	 (case l of
	    e1::e2::r => (doit (e2::r) "")
	  | _ => (if newLine then "\n" else ""))^
	    indent^right
	    )
      end

    fun writeComment indent c =
      let
	fun avoidStartComment s = 
	  let
	    fun doit wasLParen l a =
	      case l of
		[] => String.implode (rev a)
	      | h::r => 
		  if wasLParen then
		    if h =(#"*") then
		      doit false r ((#"*")::(#" ")::a)
		    else
		      doit (h=(#"(")) r (h::a)
		  else
		    doit (h=(#"(")) r (h::a)
	  in
	    doit false (String.explode s) []
	  end
	fun avoidEndComment s = 
	  let
	    fun doit wasWild l a =
	      case l of
		[] => String.implode (rev a)
	      | h::r => 
		  if wasWild then
		    if h =(#")") then
		      doit false r ((#")")::(#" ")::a)
		    else
		      doit (h=(#"*")) r (h::a)
		  else
		    doit (h=(#"*")) r (h::a)
	  in
	    doit false (String.explode s) []
	  end
      in	
	(indent^"(*--  "^
	 (avoidEndComment (avoidStartComment c))^
	 "  --*)")
      end

    fun writeUniCharVector v = 
      let
	val l = Vector.length v
	fun writeElement w = "0wx"^(Word.toString w)
	fun doit i s =
	  if (l > i) then doit (i+1) (s^","^((writeElement (Vector.sub(v,i)))))
	  else s
      in
	 "(#["^
	 (if (l>=1) then (writeElement (Vector.sub(v,0)))
	  else "")^
	 (if (l>=2) then (doit 1 "")
	  else "")^
	    "]) "^(writeComment "" (UniChar.Vector2String v))
      end
  end
      