signature FxtOptions =
   sig
      include ParserOptions

      val O_ERROR_DEVICE    : TextIO.outstream ref
      val O_ERROR_LINEWIDTH : int ref
      val O_FXT_DEBUG      : int ref
      val O_OUTPUT_EXECUTABLE : bool ref
      val O_SILENT : bool ref
      val O_PROFILE : bool ref
      val O_ESCAPE : bool ref
      val O_REMOVE_SELECTS : bool ref

      val fxtUsage : Options.Usage

      val setFxtDefaults : unit -> unit
      val setRunOptions  : unit -> unit
      val setFxtOptions  : Options.Option list * (string -> unit) 
	 -> bool * bool * string option * string option * string option * string option * string option * string list
   end

structure FxtOptions : FxtOptions =
   struct
     open Options ParserOptions


     val O_VALIDATE = ref false

      val O_ERROR_DEVICE         = ref TextIO.stdErr
      val O_ERROR_LINEWIDTH      = ref 80 
      val O_FXT_DEBUG            = ref 0
      val O_OUTPUT_EXECUTABLE    = ref true
      val O_SILENT = ref false
      val O_PROFILE = ref false
      val O_ESCAPE = ref false
      val O_REMOVE_SELECTS = ref true


      val shortFxtUsage =
	[
	 U_ITEM(["-x","--no-executable"],"Does not output an executable"),
	 U_ITEM(["-s","--silent"],"Suppress reporting of errors and warnings"),
	 U_ITEM(["-e <file>","--error-output=<file>"],"Redirect errors to file (stderr)"),
	 U_ITEM(["-i <file> [-o <file>]"],"XML input and output file for the immediate run of the transformation (no image is output)"),
	 U_ITEM(["--profile"],"Report execution times"),
	 U_ITEM(["--literal"],"Do not escape <,& and > in the output"),
	 U_ITEM(["--static[=(yes|no)]"],"Remove dynamic selects by using binary match patterns (yes)"),
	 U_SEP,
	 U_ITEM(["--version"],"Print the version number and exit"),
	 U_ITEM(["-?","--help"],"Print a summary of options"),
	 U_ITEM(["--"],"Do not recognize remaining arguments as options")
	 ]

       
      val fxtUsage = 
	shortFxtUsage@
	[U_SEP,
	 U_ITEM(["or one of the fxp parser options:"],"")]@
	ParserOptions.parserUsage

      fun setFxtDefaults () = 
	 let 
	   val _ = O_ERROR_DEVICE         := TextIO.stdErr
	   val _ = O_ERROR_LINEWIDTH      := 80 
	   val _ = O_FXT_DEBUG            := 0
	   val _ = O_OUTPUT_EXECUTABLE    := true
	   val _ = O_SILENT               := false
	   val _ = O_PROFILE              := false
	   val _ = O_ESCAPE               := false
	   val _ = O_REMOVE_SELECTS       := true
	 in ()
	 end

      fun setRunOptions () =
       let
	 val _ = RunOptions.O_SILENT         := !O_SILENT
	 val _ = RunOptions.O_ERROR_DEVICE   := !O_ERROR_DEVICE
       in
	 ()
       end

      fun setFxtOptions (opts,optError) =
	 let
            fun onlyOne what = "at most one "^what^" may be specified"
            fun unknown pre opt = String.concat ["unknown option ",pre,opt]
            fun hasNoArg pre key = String.concat ["option ",pre,key," expects no argument"]
            fun mustHave pre key = String.concat ["option ",pre,key," must have an argument"]
            fun mustBeNum pre key = String.concat 
               ["the argument to option ",pre,key," must be a number"]

            fun check_noarg(key,valOpt) = 
               if isSome valOpt then optError (hasNoArg "--" key) else () 

	    fun do_long (pars as (v,h,e,f,xmlInput,xmlOutput,enc,args)) (key,valOpt) =
	       case key 
		 of "help" => (v,true,e,f,xmlInput,xmlOutput,enc,args) before check_noarg(key,valOpt)
		  | "version" => (true,h,e,f,xmlInput,xmlOutput,enc,args) before check_noarg(key,valOpt)
		  | "no-executable" => (v,h,e,f,xmlInput,xmlOutput,enc,args) before O_OUTPUT_EXECUTABLE := false before check_noarg(key,valOpt)
		  | "error-output" => (case valOpt
					 of NONE => pars before optError (mustHave "--" key)
					  | SOME s => (v,h,SOME s,f,xmlInput,xmlOutput,enc,args))
		  | "silent" => pars before O_SILENT := true before check_noarg(key,valOpt)
		  | "encoding" => (case valOpt
				     of NONE => pars before optError (mustHave "--" key)
				   | SOME s => (v,h,e,f,xmlInput,xmlOutput,SOME s,args))
		  | "profile" => pars before O_PROFILE := true before check_noarg(key,valOpt)
		  | "literal" => pars before O_ESCAPE := true before check_noarg(key,valOpt)

		  | "static" => (case valOpt of
				   NONE => pars before O_REMOVE_SELECTS := true
				 | SOME "yes" => pars before O_REMOVE_SELECTS := true
				 | SOME "no" => pars before O_REMOVE_SELECTS := false
				 | SOME s => pars before optError 
				     "the argument to option --static must be yes or no")
		   		   
		  | "debug" => (case valOpt
                                  of NONE => pars before optError (mustHave "--" key)
                                   | SOME s => case Int.fromString s
                                                 of NONE => pars before 
                                                    optError (mustBeNum "--" key)
                                                  | SOME n => pars before O_FXT_DEBUG := n)
		  | _ => pars before optError(unknown "--" key)
		    
	    fun do_short (pars as (v,h,e,f,xmlInput,xmlOutput,enc,args)) (cs,opts) =
	       case cs 
		 of nil => doit pars opts
	       | [#"s"] => doit pars opts before O_SILENT := true
		   
	       | [#"e"] => (case opts 
			      of OPT_STRING s::opts1 => doit (v,h,SOME s,f,xmlInput,xmlOutput,enc,args) opts1
			    | _ => (optError (mustHave "-" "e"); doit pars opts))
	       | [#"i"] => (case opts 
			      of OPT_STRING s::opts1 => doit (v,h,e,f,SOME s,xmlOutput,enc,args) opts1
			    | _ => (optError (mustHave "-" "i"); doit pars opts))
	       | [#"o"] => (case opts 
			      of OPT_STRING s::opts1 => doit (v,h,e,f,xmlInput,SOME s,enc,args) opts1
			    | _ => (optError (mustHave "-" "i"); doit pars opts))
	       | cs => doit (foldr
			     (fn (c,pars) 
			      => case c of 
			      #"x" => pars before O_OUTPUT_EXECUTABLE := false
			    | #"s" => pars before O_SILENT := true
			    | #"i" => pars before optError (mustHave "-" "i")
			    | #"o" => pars before optError (mustHave "-" "o")
			    | #"e" => pars before optError (mustHave "-" "e")
			    | #"?" => (v,true,e,f,xmlInput,xmlOutput,enc,args)
			    | c => pars before 
			      optError(unknown "-" (String.implode [c])))
			     pars cs) opts
		   
	    and doit pars nil = pars
	      | doit (pars as (v,h,e,f,xmlInput,xmlOutput,enc,args)) (opt::opts) =
	       case opt 
		 of OPT_LONG(key,valOpt) => doit (do_long pars (key,valOpt)) opts
		  | OPT_SHORT cs => do_short pars (cs,opts)
		  | OPT_STRING s => 
		   if isSome f then
		     doit (v,h,e,f,xmlInput,xmlOutput,enc,s::args) opts
		   else doit (v,h,e,SOME s,xmlInput,xmlOutput,enc,args) opts
		  | OPT_NOOPT => doit pars opts
		  | OPT_NEG cs => let val _ = if null cs then ()
					      else app (fn c => optError
							(unknown "-n" (String.implode[c]))) cs
				  in doit pars opts
				  end
	 in 
	    doit (false,false,NONE,NONE,NONE,NONE,NONE,[]) opts
	 end
   end
