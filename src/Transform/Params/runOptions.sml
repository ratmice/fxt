signature RunOptions =
   sig
      include ParserOptions

      val O_ERROR_DEVICE    : TextIO.outstream ref
      val O_SILENT : bool ref

      val shortRunUsage : Options.Usage
      val runUsage : Options.Usage

      val setRunDefaults : unit -> unit
      val setRunOptions  : Options.Option list * (string -> unit) 
	 -> (string option * bool * string option * string option * string option * string list)
      val setGrepOptions : unit -> unit
   end

structure RunOptions : RunOptions =
   struct
     open Options ParserOptions
       
     val O_ERROR_DEVICE = ref TextIO.stdErr
     val O_SILENT = ref false

     val shortRunUsage =
       [
	U_ITEM(["-i <file>","--input=<file>"],"XML input file"),
	U_ITEM(["-o <file>","--output=<file>"],"XML output file"),
	U_ITEM(["-e <file>","--error-output=<file>"],"Redirect errors to file (stderr)"),
	U_ITEM(["-s","--silent"],"Suppress reporting of errors and warnings"),
	U_ITEM(["--encoding=<encodingName>"],"Encode the output in the specified format "),
	U_SEP,
	U_ITEM(["-?","--help"],"Print a summary of options"),
	U_ITEM(["--"],"Do not recognize remaining arguments as options")
	]       
     val runUsage = 
       shortRunUsage@
       [U_SEP,
	U_ITEM(["or one of the fxp parser options:"],"")]@       
       ParserOptions.parserUsage
       
     fun setRunDefaults () = 
       let 
	 val _ = O_SILENT       := false
	 val _ = O_ERROR_DEVICE := TextIO.stdErr
       in ()
       end

     fun setGrepOptions () =
       let
	 val _ = GrepOptions.O_SILENT       := !O_SILENT
	 val _ = GrepOptions.O_ERROR_DEVICE := !O_ERROR_DEVICE
       in
	 ()
       end

     fun setRunOptions (opts,optError) =
	 let
            fun onlyOne what = "at most one "^what^" may be specified"
            fun unknown pre opt = String.concat ["unknown option ",pre,opt]
            fun hasNoArg pre key = String.concat ["option ",pre,key," expects no argument"]
            fun mustHave pre key = String.concat ["option ",pre,key," must have an argument"]
            fun mustBeNum pre key = String.concat 
               ["the argument to option ",pre,key," must be a number"]

            fun check_noarg(key,valOpt) = 
               if isSome valOpt then optError (hasNoArg "--" key) else () 

	    fun do_long (pars as (err,help,xmlInput,xmlOutput,encoding,args)) (key,valOpt) =
	       case key 
		 of "help" => (err,true,xmlInput,xmlOutput,encoding,args) before check_noarg(key,valOpt)
	       | "input" => (case valOpt
			       of NONE => pars before optError (mustHave "--" key)
			     | SOME s => (err,help,SOME s,xmlOutput,encoding,args))
	       | "output" => (case valOpt
				of NONE => pars before optError (mustHave "--" key)
			      | SOME s => (err,help,xmlInput,SOME s,encoding,args))
	       | "error-output" => (case valOpt
				      of NONE => pars before optError (mustHave "--" key)
				    | SOME s => (SOME s,help,xmlInput,xmlOutput,encoding,args))
	       | "silent" => pars before O_SILENT := true before check_noarg(key,valOpt)
	       | "encoding" => (case valOpt
				  of NONE => pars before optError (mustHave "--" key)
				| SOME s => (err,help,xmlInput,xmlOutput,SOME s,args))
	       | _ => pars before optError(unknown "--" key)
		    
	    fun do_short (pars as (err,help,xmlInput,xmlOutput,encoding,args)) (cs,opts) =
	       case cs 
		 of nil => doit pars opts
	       | [#"s"] => doit pars opts before O_SILENT := true
	       | [#"e"] => (case opts 
			      of OPT_STRING s::opts1 => doit (SOME s,help,xmlInput,xmlOutput,encoding,args) opts1
			    | _ => (optError (mustHave "-" "e"); doit pars opts))
	       | [#"i"] => (case opts 
			      of OPT_STRING s::opts1 => doit (err,help,SOME s,xmlOutput,encoding,args) opts1
			    | _ => (optError (mustHave "-" "i"); doit pars opts))
	       | [#"o"] => (case opts 
			      of OPT_STRING s::opts1 => doit (err,help,xmlInput,SOME s,encoding,args) opts1
			    | _ => (optError (mustHave "-" "i"); doit pars opts))
	       | cs => 
		   doit (foldr
			 (fn (c,pars) 
			  => case c of 
			  #"s" => pars before O_SILENT := true
			| #"e" => pars before optError (mustHave "-" "e")
			| #"i" => pars before optError (mustHave "-" "i")
			| #"o" => pars before optError (mustHave "-" "o")
			| #"?" => (err,true,xmlInput,xmlOutput,encoding,args)
			| c => pars before 
			    optError(unknown "-" (String.implode [c])))
			 pars cs) opts
		   
	    and doit pars nil = pars
	      | doit (pars as (err,help,xmlInput,xmlOutput,encoding,args)) (opt::opts) =
	       case opt 
		 of OPT_LONG(key,valOpt) => doit (do_long pars (key,valOpt)) opts
		  | OPT_SHORT cs => do_short pars (cs,opts)
		  | OPT_STRING s => 
		   doit (err,help,xmlInput,xmlOutput,encoding,s::args) opts
		  | OPT_NOOPT => doit pars opts
		  | OPT_NEG cs => let val _ = if null cs then ()
					      else app (fn c => optError
							(unknown "-n" (String.implode[c]))) cs
				  in doit pars opts
				  end
	    val opts1 = ParserOptions.setParserOptions (opts,optError)
	 in
	   doit (NONE,false,NONE,NONE,NONE,[]) opts1
	 end
   end
