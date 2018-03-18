structure Export =
  struct
    fun doit prog inputFile outputFile encoding args wrapper checkArgs =
      let
	val _ = checkArgs args
	val docList = 
	  Doc.matchFile (Option.valOf (!Transform.passesRef)) inputFile
	val timer = Timer.startRealTimer ()
	val result = Doc.docMap (wrapper  inputFile docList args) docList
	val time = Time.toMilliseconds (Timer.checkRealTimer timer)
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Transforming (possible table processing included) took "^
			   (LargeInt.toString time)^" milliseconds.\n")
	  else ()	  
      in
	Fdm.putForest result outputFile encoding
      end
    handle e => 
      let
	val _ =  
	  case e of 
	    Exceptions.Arguments n =>
	      if !RunOptions.O_SILENT then ()
	      else
		let
		  val _ = 
		    case n of 
		      0 =>
			TextIO.output(!RunOptions.O_ERROR_DEVICE,"Transformation expects no arguments.\n")		      
		    | 1 =>
			TextIO.output(!RunOptions.O_ERROR_DEVICE,"Transformation expects one argument.\n")
		    | _ =>
			TextIO.output(!RunOptions.O_ERROR_DEVICE,"Transformation expects "^(Int.toString n)^" arguments.\n")		      
		  val _ = 
		    let
		      fun doit i args = 
			if i=n then args
			else doit (i+1) (args^"arg"^(Int.toString i)^" ")
		    in
		      Util.printUsage (!RunOptions.O_ERROR_DEVICE) prog RunOptions.shortRunUsage (doit 0 "")
		    end
		in
		  ()
		end
	  | Exceptions.RuntimeError s => 
		if !(RunOptions.O_SILENT) then () 
		else 
		  TextIO.output
		  (!RunOptions.O_ERROR_DEVICE,
		   "Runtime Error: "^s^"\n")
	  | Exceptions.InternalError => 
		  if !(RunOptions.O_SILENT) then () 
		  else 
		    TextIO.output
		    (!RunOptions.O_ERROR_DEVICE,
		     "Internal error.\n")
	  | _ =>
		    if !(RunOptions.O_SILENT) then () 
		    else
		      let
			val _ = 
			  TextIO.output (!RunOptions.O_ERROR_DEVICE,"Exception: ")
			val _ = 
			  List.app (fn s => TextIO.output (!RunOptions.O_ERROR_DEVICE,s)) (SMLofNJ.exnHistory e)
			val _ =
			  TextIO.output (!RunOptions.O_ERROR_DEVICE,"\n")
		      in
			()
		      end
      in
	OS.Process.exit OS.Process.failure
      end
        
    fun export imageName wrapper checkArgs =
      let
	fun exportdoit (prog,argList) =
	  let
	    val timer = Timer.startRealTimer ()
	    val hadError = ref false

	    fun optError msg = 
	      let 
		val _ = TextIO.output(TextIO.stdErr,prog^": "^msg^".\n")
	      in 
		hadError := true
	      end
	    fun exitError msg = 
	      ((TextIO.output(TextIO.stdErr,prog^": "^msg^".\n"));(OS.Process.failure))
	    fun exitHelp () = 
	      ((Util.printUsage TextIO.stdOut prog RunOptions.runUsage "[argument ...]");(OS.Process.success))
	    fun exitVersion () = 
	      ((app print [prog," version ",FxtVersion.FXT_VERSION,
			   " using fxlib version ",Version.FXP_VERSION,
			   " and fxgrep version ",GrepVersion.FXGREP_VERSION,"\n"]);(OS.Process.success))
	    fun summOpt () = "For a summary of options type "^prog^" --help"
	    fun noSuchFile(f,cause) = "can't open file '"^f^"': "^exnMessage cause

	    val opts = Options.parseOptions argList
	    val _ = RunOptions.setRunDefaults()
	    val (err,help,xmlInput,xmlOutput,encoding,args) = 
	      RunOptions.setRunOptions (opts,optError)
	  in
	    if !hadError then exitError (summOpt ()) 
	    else
	      if help then exitHelp ()
	      else
		let
		  val _ = case err of 
		    SOME "-" => RunOptions.O_ERROR_DEVICE := TextIO.stdErr
		  | SOME f => (RunOptions.O_ERROR_DEVICE := TextIO.openOut f
			       handle IO.Io {cause,...} => ((exitError(noSuchFile(f,cause)));()))
		  | NONE => ()

		  val inputFile =
		    if isSome xmlInput then 
		      valOf xmlInput
		    else "-" 
		  val outputFile =
		    if isSome xmlOutput then 
		      valOf xmlOutput
		    else "-"
		  val encoding =
		    if isSome encoding then encoding
		    else NONE

		  val _ = RunOptions.setGrepOptions ()

		  val _ = 
		    doit prog inputFile outputFile encoding args wrapper checkArgs
		  val _ = if isSome err then TextIO.closeOut (!RunOptions.O_ERROR_DEVICE) else () 
		in
		  OS.Process.success
		  before 
		  let
		    val time = Time.toMilliseconds 
		      (Timer.checkRealTimer timer)
		    val _ = 
		      if !FxtOptions.O_PROFILE then
			TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Total (no startup time): "^
				       (LargeInt.toString time)^" milliseconds.\n")
		      else ()	  
		  in
		    ()
		  end

		end
	  end
      in
	SMLofNJ.exportFn(imageName,exportdoit)
      end
  end
