structure ParserOptions = 
  struct 
    structure FxpOptions = FxpOptions()
    open  FxpOptions

    fun setParserDefaults () = 
      let
	val _ = FxpOptions.setParserDefaults ()
	val _ = FxpOptions.O_INCLUDE_EXT_PARSED := true
      in
	()
      end
  end
