structure StringTables =
  struct
    type table = (Unicode.Vector,Unicode.Vector list) HashTable.hash_table
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

    fun fillTables keys matchTable =
      Array.appi 
      (fn (i,(tree,matchMap,sons)) =>
       IntListMap.appi 
       (fn (j,_) => 
	let
	  val (name,toString,toItem) = Vector.sub(keys,j)
	  val table = getTable name
	  val key = toString tree
	  val item = toItem tree
	in
	  case (HashTable.find table key) of
	    SOME docs => HashTable.insert table (key,item::docs)
	  | NONE => HashTable.insert table (key,[item])
	end) matchMap) matchTable

    fun lookupKey tableName keyName =
      let
	val table = getTable tableName
      in
	HashTable.lookup table keyName
      end

    fun findKey tableName keyName =
      let
	val table = getTable tableName
      in
	HashTable.find table keyName
      end

  end
