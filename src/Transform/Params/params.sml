structure Params =
  struct
    val treeTableSize = 32
    val defaultAction = 
      let
	val current = Names.current
	val trans = Names.functionName
	val d = Names.docArgument
      in
	"if (isElement "^current^") then\n"^
	"  (vector["^
	Names.elementConstructor^
	" (getElementType "^current^") (getAttributes current) "^
	"(Doc.docMap "^trans^" (Doc.docSons "^d^"))])\n"^
	"else if (isProcessingInstruction "^current^")\n"^
	"     then (Doc.docMap "^trans^" (Doc.docSons "^d^"))\n"^ 
	"     else (vector["^current^"])\n"
      end 
  end