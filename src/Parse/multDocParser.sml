structure MultDocParser =
  struct
    structure ParseDoc = Parse (structure Dtd = DocDtd0
				structure Hooks = MultDocHooks0
				structure Resolve = ResolveNull
				structure ParserOptions = RunOptions)
      
    fun parseDoc dtd uri = 
      ParseDoc.parseDocument uri (SOME dtd) (MultDocHooks.docStart dtd)

  end