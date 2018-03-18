structure DocParser =
  struct
    structure ParseDoc = Parse (structure Dtd = DocDtd0
				structure Hooks = DocHooks0
				structure Resolve = ResolveNull
				structure ParserOptions = RunOptions)
      
    fun parseDoc dtd uri = 
      ParseDoc.parseDocument uri (SOME dtd) (DocHooks.docStart dtd)

  end