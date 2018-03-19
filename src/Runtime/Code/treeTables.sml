structure TreeTables =
  struct
    type table = (Unicode.Vector,Doc.Doc list) HashTable.hash_table
    val tables = ref ([]: ((Unicode.Vector*table) list))
    fun initTables () = tables := ([]: ((Unicode.Vector*table) list))
    fun addTable name =
      let
	val table = (name, HashTable.mkTable (UniChar.hashVector,(op =)) (Params.treeTableSize,Exceptions.NoSuchKey))
      in
	tables := table::(!tables)
      end
    fun getTable name =
      let
	fun doit l =
	  case l of
	    nil => raise Exceptions.NoSuchTable (Unicode.Vector2String name)
	  | (n,t)::r => if n=name then t
			else doit r
      in
	doit (!tables)
      end

    fun fillTables keys matchTableKeys matchTable =
      Array.appi 
      (fn (i,(t,matchMap,sons)) =>
       IntListMap.appi 
       (fn (j,_) => 
	let
	  val (name,toString) = Vector.sub(keys,j)
	  val table = getTable name
	  val key = toString t
	in
	  case (HashTable.find table key) of
	    SOME docs => HashTable.insert table (key,(i,matchTable)::docs) (* beware order*)
	  | NONE => HashTable.insert table (key,[(i,matchTable)])
	end) matchMap) matchTableKeys

    fun lookupKey tableName keyName =
      let
	val table = getTable tableName
      in
	HashTable.lookup table keyName
      end

    fun findKey tableName keyName = HashTable.find (getTable tableName) keyName 

  end
