structure Table = 
  struct

    fun split n el f =
    (* n = number of columns *)
    (* el is the row element *)
    (* f is the forest to be split *)
      let
	fun doit i a =
	  (* arrange horizontally *)
	  if (i >= Vector.length f) then Vector.fromList (List.rev a)
	  else 
	    let
	      val t = (Vector.sub (f,i))
	    in
	      if t = Fdm.emptyTree then doit (i+1) a
	      else
		doit 
		(i+1) 
		(
		 t :: 
		 (if (i mod n) = 0 then ((Fdm.element el #[] #[]))::a
		  else a))
	    end
	val l = Vector.length f
	val rowNos = (l div n) + 1
	fun doit1 i a =
	  (* arrange vertically *)
	  if i >= rowNos then Vector.fromList (List.rev a)
	  else
	    doit1 (i+1)
	    ((List.tabulate 
	      (n, 
	       (fn j => 
		if (i+(n-j-1)*rowNos) < l then Vector.sub(f,i+(n-j-1)*rowNos)
		else Fdm.emptyTree)))@[(Fdm.element el #[] #[])]@a)
      in
	(*doit 0 []*)
	doit1 0 []
      end

    fun replaceSpaces s =
      UniChar.Data2Vector 
      (List.map 
       (fn c => if (UniClasses.isS c) orelse (c=0wx22) orelse (c=0wx27) then 0wx5F 
		else c) 
       (UniChar.Vector2Data s))

  end