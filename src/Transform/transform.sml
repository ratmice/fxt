structure Transform =
  struct
    val passesRef = ref (NONE:MatchData.Passes option)
    val dtdRef = ref (DocDtd.initDocDtd ())
    val keyTablesPassesRef = ref (NONE:MatchData.Passes option)
    val stringKeyTablesPassesRef = ref (NONE:MatchData.Passes option)

    val out = ref TextIO.stdOut
    fun print x = TextIO.output(!out,x)
    fun printOut x = TextIO.output(TextIO.stdOut,x)

    val indent1 = "  "
    val indent2 = "    "
    val indent3 = "      "
    val indent4 = "        "

    fun code file = 
      let
	val (uriOption,specFileName) = 
	  case file of
	    SOME f => (SOME (Uri.String2Uri f),f)
	  | NONE => (NONE,"stdin")
	val (stringTables,tables,preActs,
	     patterns,dtd,toInclude,toOpen,fxslGlobals,
	     furtherArguments,passes,actions,default) = 
	    (ParseSpec.readSpec uriOption)

	val _ = passesRef := SOME passes
	val _ = dtdRef := dtd

	val specFileDir = OS.Path.dir specFileName

	val outputDir = OS.Path.joinDirFile 
	  {dir=specFileDir,file=Names.outputDir}
	val command = "test -d "^outputDir
	val s = (OS.Process.system command) 
	  handle e => raise (Exceptions.SystemCommand command)
	val _ = 
	  if s = OS.Process.failure then 
	    OS.FileSys.mkDir outputDir
	  else ()

	val {base=filePrefix,ext=ext}  = 
	  OS.Path.splitBaseExt (OS.Path.file specFileName)
	val prefix = 
	  Util.capitalizeFirst (Util.makeIdentifier filePrefix)

	val keyTables = Names.keyTablesStructure
	val keyNames = Names.keyNames
	val stringKeyTables = Names.stringKeyTablesStructure

	fun mkFileName name =
	  OS.Path.joinDirFile
	  {dir=outputDir,file=filePrefix^name}

	val fxtFileName = mkFileName "Fxt.sml"
	val imageFileName = mkFileName "_image"
	val scriptFileName = 
	  OS.Path.joinDirFile {dir=specFileDir,file=filePrefix^".sh"}

	fun writeSMLCode () =
	  let
	    fun writeTransformator () =
	      let
		fun writeCheckArgs indent =
		  let
		    val _ = print (indent^"fun checkArgs args =\n")
		    val _ = print (indent^"  if (List.length args)<>"^(Int.toString (List.length furtherArguments))^" then\n")
		    val _ = print (indent^"    raise Exceptions.Arguments "^(Int.toString (List.length furtherArguments))^"\n")
		    val _ = print (indent^"  else ()\n")
		  in
		    ()
		  end

		fun writeWrapper indent =
		  let
		    val _ = print (indent^"fun wrapper xmlInputFile docList args d =\n")
		    val _ = print (indent^"  let\n")
		    val _ = List.app 
		      (fn a => 
                            print (indent^"    val _ = "^a^"\n")) preActs
		    val _ = if furtherArguments<>nil then
			    print (indent^"    val args = Vector.fromList args\n")
			    else ()

		    val _ = Vector.appi 
		      (fn (i,arg) => 
		            print (indent^"    val _ = Globals.push "^Names.globalsStructure^"."^arg^" (Vector.sub (args,"^(Int.toString i)^"))\n")) 
		      (Vector.fromList furtherArguments)

		    val _ = print (indent^"    val _ = Globals.push "^Names.globalsStructure^"."^Names.inputFile^" xmlInputFile\n")

		    val _ = case tables of
		      (#[],_) => ()
		    | (keys,_) =>
			let
			  val _ = 
                            print (indent^"    val _ = TreeTables.initTables()\n")
			  val _ = Vector.app (fn (name,_) =>
			    print (indent^"    val _ = TreeTables.addTable "^(WriteUtil.writeUniCharVector name)^"\n")) keys
			  val _ = 
			    print (indent^"    val tables = ")
			  val _ = print (WriteUtil.writeVector "" false 
					 (WriteUtil.writePair "" false
					  WriteUtil.writeUniCharVector 
					  (fn x=>x)) keys^"\n")
			  val _ =
			    print (indent^"    val fdmForest = Vector.fromList (List.map Doc.docTree docList)\n")
			  val _ = 
			    print ("        val docList1 = Doc.matchFdm (Option.valOf (!Transform.keyTablesPassesRef)) fdmForest\n")
			  val _ = 
                            print (indent^"    val _ = TreeTables.fillTables tables (Doc.matchTable docList1)  (Doc.matchTable docList)\n")
			in
			  ()
			end
		      
		    val _ = case stringTables of
		      (#[],_) => ()
		    | (keys,_) =>
			let
			  val _ = 
                            print (indent^"    val _ = StringTables.initTables()\n")
			  val _ = Vector.app (fn (name,_,_) =>
			    print (indent^"    val _ = StringTables.addTable "^(WriteUtil.writeUniCharVector name)^"\n")) keys
			  val _ = 
			    print (indent^"    val tables = ")
			  val _ = print (WriteUtil.writeVector "" false 
					 (WriteUtil.writeTriplet 
					  WriteUtil.writeUniCharVector 
					  (fn x=>x)
					  (fn x=>x)) keys^"\n")
			  val _ =
			    print (indent^"    val fdmForest = Vector.fromList (List.map Doc.docTree docList)\n")
			  val _ = 
			    print ("        val docList1 = Doc.matchFdm (Option.valOf (!Transform.stringKeyTablesPassesRef)) fdmForest\n")
			  val _ = 
                            print (indent^"    val _ = StringTables.fillTables tables (Doc.matchTable docList1)\n")
			in
			  ()
			end

		    val _ = print (indent^"  in\n")
		    val _ = print (indent^"    "^Names.functionName^" d\n")
		    val _ = print (indent^"  end\n")
		  in
		    ()
		  end
		
		fun writeTransformFunction indent =
		  let
		    fun printActions indent = 
		      let
			fun actionCase i=
			  (
			   (Int.toString i)^
			   "=> \n"^
			     (WriteUtil.stringIndent (Vector.sub(actions,i)) (indent^"  "))^
			     "\n")
			val l = Vector.length actions
		      in
			(
			 (
			  if (l>0) then
			    (
			     (print ((WriteUtil.writeComment indent ("generated from pattern: "^(Vector.sub(patterns,0))))^"\n"));
			     (print (indent^(actionCase 0)));
			     (
			      if (l>1) then
				VectorSlice.appi
				(fn (index,action) =>
				 (
				  (print ((WriteUtil.writeComment indent ("generated from pattern: "^(Vector.sub(patterns,index))))^"\n"));
				  (print (indent^"|"^(actionCase index)))
				  ))
				(VectorSlice.slice(actions,1,NONE))
			      else ()
				)
			     )
			  else ()
			    );
			 (case default of
			    SOME _ =>
			      (print ((WriteUtil.writeComment indent ("generated for the default case"))^"\n"))
			  | NONE =>
			      (print ((WriteUtil.writeComment indent ("generated by default  for the default case"))^"\n")));
			    (print indent);
			    (if l>0 then print "|"
			     else ());
			       (print "_=>\n");
				 (case default of 
				    SOME d => 
				      print ((WriteUtil.stringIndent d (indent^"  "))^"\n")
				  | NONE =>
				      print ((WriteUtil.stringIndent Params.defaultAction (indent^"  "))^"\n")))
		      end
		    
		    val _ = print (indent^"fun "^Names.functionName^" "^Names.docArgument^" ")
		    val _ = print "=\n"
		    val _ = print (indent^"  let\n")
		    val _ = print (indent^"    val "^Names.current^" = (Doc.docTree "^Names.docArgument^")\n")
		    val _ = print (indent^"  in\n")
		    val _ = print (indent^"    case (Doc.matchedPattern "^Names.docArgument^") of\n")
		    val _ = printActions (indent^"      ")
		    val _ = print (indent^"  end\n")
		  in
		    ()
		  end
		
		
		val _ = print ("structure "^prefix^Names.fxtStructure^" =\n")
		val _ = print (indent1^"struct\n")
		val _ = print (indent2^"open "^toOpen^"\n\n")
		val _ = print (indent2^"structure "^Names.globalsStructure^" =\n")
		val _ = print (indent2^"  struct\n")
		val _ =
		  List.app (fn (name,(typ,_)) 
			    => (
                        print (indent2^"    val "^name^
			       " = ref ([] : (("^
			       typ^") list))\n"))) 
		  fxslGlobals

		val _ = print (indent2^"  end\n")
		val _ = print "\n"

		val _ = writeTransformFunction indent2
		val _ = print "\n"

		val _ = writeCheckArgs indent2
		val _ = print "\n"

		val _ = writeWrapper indent2
		val _ = print "\n"
		  
		val _ = print (indent1^"end\n")
	      in
		()
	      end

	    val _ = case tables of
	      (#[],_) => ()
	    | (keys,passes) => keyTablesPassesRef := SOME passes

	    val _ = case stringTables of
	      (#[],_) => ()
	    | (keys,passes) => stringKeyTablesPassesRef := SOME passes

	    val _ = out := TextIO.openOut fxtFileName
	    val _ = writeTransformator ()
	    val _ = TextIO.closeOut (!out)

	    val _ = out := TextIO.stdOut
	  in
	    ()
	  end

	fun compile () =
	  let
	    fun useSilently s =
	      let
		val saved = (!Control.Print.signatures,
			     !Control.Print.out)
		fun done () = 
		  let
		    val _ = Control.Print.signatures := #1 saved
		    val _ = Control.Print.out := #2 saved
		  in
		    ()
		  end
		val msg = ref ""
	      in
		(Control.Print.out :=
		 {say = fn s => msg := (!msg)^s, flush = fn _ => ()});
		(Control.Print.signatures := 2);
		(Backend.Interact.useFile s; done ())
		handle _ => (done();raise (Exceptions.CompileError (!msg)))
	      end

	    fun use s =
	      let
		val saved = !Control.Print.signatures
		val _ = Control.Print.signatures := 1
		val _ = 
		  (Backend.Interact.useFile s)
		  handle e => raise (Exceptions.CompileError "")
		val _ = Control.Print.signatures := saved
	      in
		()
	      end

	    val compile = useSilently
	    val _ = List.app compile toInclude
	    val _ = compile fxtFileName
	  in
	    ()
	  end

	fun writeImage () =
	  let
	    val exportCommand = 
	      "Export.export \""^imageFileName^"\" "^
	      prefix^Names.fxtStructure^".wrapper"^" "^
	      prefix^Names.fxtStructure^".checkArgs"
	    val inS = TextIO.openString exportCommand
	    val _ = Backend.Interact.useStream inS
	    val _ = TextIO.closeIn inS
	  in
	    ()
	  end

	fun writeScript () =
	  let
	    val _ = out := TextIO.openOut scriptFileName
	    val _ = print ("#!/bin/sh -f\n");
	    val _ = print ("\n")
	    val _ = print ("SML_BINDIR="^InstallParams.SML_BINDIR^"\n")
	    val _ = print ("ARCH_N_OPSYS=`${SML_BINDIR}/.arch-n-opsys`\n")
	    val _ = print ("if [ \"$?\" != \"0\" ]; then\n")
	    val _ = print ("  echo \"$CMD: unable to determine architecture/operating system\"\n")
	    val _ = print ("  exit 1\n")
	    val _ = print ("fi\n")
	    val _ = print ("eval ${ARCH_N_OPSYS}\n")
	    val _ = print ("\n")
	    val _ = (print ("HEAP="^(OS.FileSys.fullPath (OS.Path.dir imageFileName))^"/"^(OS.Path.file imageFileName)^"\n"))
	    val _ = print ("\n")
	    val _ = print ("RUN=${SML_BINDIR}/.run/run.${ARCH}-${OPSYS}\n")
	    val _ = print ("\n")
	    val _ = print ("exec ${RUN} @SMLcmdname=$0 @SMLdebug=/dev/null @SMLload=${HEAP} \"$@\"\n")
	    val _ = print ("\n")
	    val _ = TextIO.closeOut (!out)
	    val _ = (OS.Process.system ("chmod +x "^scriptFileName)) handle e => raise (Exceptions.SystemCommand "chmod")
	    val _ = out := TextIO.stdOut
	  in
	    ()
	  end

	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Generating code...")
	  else ()
	val timer = Timer.startRealTimer ()
	val _ = writeSMLCode ()
	val time = Time.toMilliseconds (Timer.checkRealTimer timer)
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"took "^
			   (LargeInt.toString time)^" milliseconds.\n")
	  else ()	  
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"Compiling generated code...")
	  else ()
	val timer = Timer.startRealTimer ()
	val _ = compile ()
	val time = Time.toMilliseconds (Timer.checkRealTimer timer)
	val _ = 
	  if !FxtOptions.O_PROFILE then
	    TextIO.output (!FxtOptions.O_ERROR_DEVICE,"took "^
			   (LargeInt.toString time)^" milliseconds.\n")
	  else ()	  
      in
	(prefix,writeImage,writeScript)
      end

    fun transform (_,argList) =
      let
	val timer = Timer.startRealTimer ()
	val prog = "fxt"
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
	  ((Util.printUsage TextIO.stdOut prog FxtOptions.fxtUsage "specFile [argument ...]");(OS.Process.success))
	fun exitVersion () = 
	  ((app print [prog," version ",FxtVersion.FXT_VERSION,
		       " using fxlib version ",Version.FXP_VERSION,
		       " and fxgrep version ",GrepVersion.FXGREP_VERSION,"\n"]);(OS.Process.success))
	fun summOpt () = "For a summary of options type "^prog^" --help"
	fun noSuchFile(f,cause) = "can't open file '"^f^"': "^exnMessage cause

	val opts = Options.parseOptions argList
	val _ = ParserOptions.setParserDefaults ()
	val opts1 = ParserOptions.setParserOptions (opts,optError)
	val _ = FxtOptions.setFxtDefaults()
	val (vers,help,err,file,xmlInput,
	     xmlOutput,encoding,args) = 
	  FxtOptions.setFxtOptions (opts1,optError)
      in
	if !hadError then exitError (summOpt ()) 
	else
	  if vers then exitVersion () 
	  else
	    if help then exitHelp ()
	    else
	      let
		val _ = case err of 
		  SOME "-" => FxtOptions.O_ERROR_DEVICE := TextIO.stdErr
		| SOME f => (FxtOptions.O_ERROR_DEVICE := TextIO.openOut f
			     handle IO.Io {cause,...} => ((exitError(noSuchFile(f,cause)));()))
		| NONE => ()
		val (prefix,writeImage,writeScript) = code file
		val _ = if isSome err then TextIO.closeOut (!FxtOptions.O_ERROR_DEVICE) else ()
	      in
		(case xmlInput of
		   SOME input =>
		     let
		       
		       val _ = FxtOptions.setRunOptions ()

		       val inputFile = valOf xmlInput
		       val outputFile =
			 if isSome xmlOutput then 
			   valOf xmlOutput
			 else "-"
		       val encoding =
			 if isSome encoding then "(SOME "^(valOf encoding)^")"
			     else "NONE"
		       
		       val command = 
			 "Export.doit \"fxt\" \""^input^"\" \""^
		          outputFile^"\" "^encoding^" "^
			 (WriteUtil.writeList 
			  ("[",",","]") 
			  "" false WriteUtil.writeString (rev args))^" "^
			 prefix^Names.fxtStructure^".wrapper"^" "^
			 prefix^Names.fxtStructure^".checkArgs"^" "
			 
		       fun useStreamSilently s =
			 let
			   val saved = !Control.Print.out
			  fun done () = 
			    Control.Print.out := saved
			  val _ =
			    (Control.Print.out :=
			     {say = fn _ => (), flush = fn _ => ()});
			 in
			   (Backend.Interact.useStream s; done ())
			   handle _ => (done();raise (Exceptions.CompileError command))
			 end
		       val inS = TextIO.openString command
		       val _ = useStreamSilently inS
		       val _ = TextIO.closeIn inS
		     in
		       OS.Process.success
		     end
		 | NONE =>
		    if (!(FxtOptions.O_OUTPUT_EXECUTABLE)) then
		      let
			val _ = writeScript ()
			val _ = writeImage ()
		      in
			OS.Process.success
		      end
		    else OS.Process.success) 
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
	    handle e => 
	      let
		val _ =  
		  case e of 
		    Exceptions.SpecificationError (pos,s) =>
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			TextIO.output
			(!FxtOptions.O_ERROR_DEVICE,
			 (Errors.Position2String pos)^" Specification Error: "^s^"\n")
		  | Exceptions.ParseError  => 
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			TextIO.output
			(!FxtOptions.O_ERROR_DEVICE,"Specification file not found or not a well-formed XML document.\n")
		  | Exceptions.CompileError  m => 
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			TextIO.output
			(!FxtOptions.O_ERROR_DEVICE,m^"Compile error\n")
		  | Exceptions.RuntimeError s => 
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			TextIO.output
			(!FxtOptions.O_ERROR_DEVICE,
			 "Runtime Error: "^s^"\n")
		  | Exceptions.InternalError => 
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			TextIO.output
			(!FxtOptions.O_ERROR_DEVICE,
			 "Internal error.\n")
		  | Exceptions.SystemCommand s => 
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			TextIO.output
			(!FxtOptions.O_ERROR_DEVICE,
			 "Could not execute: "^s^".\n")
		  | Exceptions.OtherError s => 
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			TextIO.output
			(!FxtOptions.O_ERROR_DEVICE,
			 "Error: "^s^".\n")
		  | _ =>
		      if !(FxtOptions.O_SILENT) then () 
		      else 
			let
			  val _ = 
			    TextIO.output (!(FxtOptions.O_ERROR_DEVICE),"Exception: ")
			  val _ = 
			    List.app (fn s => TextIO.output (TextIO.stdErr,s)) (SMLofNJ.exnHistory e)
			  val _ =
			    TextIO.output (!(FxtOptions.O_ERROR_DEVICE),"\n")
			in
			  ()
			end
		val _ = if isSome err then TextIO.closeOut (!FxtOptions.O_ERROR_DEVICE) else ()
	      in 
		OS.Process.failure
	      end
      end
  end
