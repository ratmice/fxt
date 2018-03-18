structure GlobalsTable =
  struct
    exception NOT_FOUND

    datatype Info = INT | BOOL | REAL | STRING | TREE | FOREST| USER of string

    val table = ref ([] : (string * (string*Info)) list)

    fun initTable () = table := []

    fun addEntry key info = table := ((key,info)::(!table))

    fun getEntry key =
      let
	fun doit l =
	  case l of
	    (k,info)::r => if key=k then info
			  else doit r
	  | [] => raise NOT_FOUND
      in
	doit (!table)
      end
  end