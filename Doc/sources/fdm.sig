signature FDM =
  sig
    type Tree
    type Forest =  Tree vector
    type Attribute

    val emptyTree : Tree
    val emptyForest : Forest

    val hasElementType : Unicode.Vector -> Tree -> bool
    val text : Unicode.Vector -> Tree
    val literateText : Unicode.Vector -> Tree
    val element : Unicode.Vector -> Attribute list -> Tree vector -> Tree
    val processingInstruction : Unicode.Vector -> Unicode.Vector -> Tree
    val isElement : Tree -> bool
    val isText : Tree -> bool
    val isProcessingInstruction : Tree -> bool
    val getElementType : Tree -> Unicode.Vector
    val getElement : Tree -> (Unicode.Vector * Attribute list * Tree vector)
    val getText : Tree -> Unicode.Vector
    val hasTextContent : Tree -> bool
    val getTextContent : Tree -> Unicode.Vector
    val getProcessingInstruction : Tree -> (Unicode.Vector*Unicode.Vector)
    val hasAttribute : Unicode.Vector -> Tree -> bool
    val getAttribute : Unicode.Vector -> Tree -> Unicode.Vector
    val Tree2UnicodeVector : Tree -> Unicode.Vector
    val sons : Tree -> Forest

    val getAttributes : Tree -> Attribute list
    val getAttributeValue : Attribute -> Unicode.Vector
    val getAttributeName : Attribute -> Unicode.Vector
    val makeAttribute : Unicode.Vector -> Unicode.Vector -> Attribute
    val replaceAttribute : Attribute list -> Unicode.Vector -> Unicode.Vector -> Attribute list
    val addAttribute : Attribute list -> Unicode.Vector -> Unicode.Vector -> Attribute list
    val addAttributes : Attribute list -> Attribute list -> Attribute list
    val deleteAttribute : Attribute list -> Unicode.Vector -> Attribute list
    val changeAttribute : Tree -> Unicode.Vector -> Unicode.Vector -> Tree
    val insertAttribute : Tree -> Unicode.Vector -> Unicode.Vector -> Tree
    val removeAttribute : Tree -> Unicode.Vector -> Tree

    val fromList : Tree list -> Forest
    val toList : Forest -> Tree list
    val length : Forest -> int
    val lengthPred : (Tree -> bool) -> Forest -> int
    val sub : Forest * int -> Tree
    val extract : Forest * int * int option -> Forest
    val concat : Forest list -> Forest
    val app : (Tree -> unit) -> Forest -> unit
    val map : (Tree -> 'a) -> Forest -> 'a vector
    val foldl : (Tree * 'a -> 'a) -> 'a -> Forest -> 'a
    val foldr : (Tree * 'a -> 'a) -> 'a -> Forest -> 'a
    val appi : (int * Tree -> unit) -> Forest * int * int option -> unit
    val mapi : (int * Tree -> 'a) -> Forest * int * int option -> 'a vector
    val foldli : (int * Tree * 'a -> 'a) -> 'a -> Forest * int * int option -> 'a
    val foldri : (int * Tree * 'a -> 'a) -> 'a -> Forest * int * int option -> 'a
    val deleteAll : (Tree -> bool) -> Forest -> Forest
    val deleteFirst : (Tree -> bool) -> Forest -> bool * Forest
    val deleteFirstN : int -> (Tree -> bool) -> Forest -> int * Forest
    val deleteLast : (Tree -> bool) -> Forest -> bool * Forest
    val deleteLastN : int -> (Tree -> bool) -> Forest -> int * Forest
    val deleteIth : int -> Forest -> Forest
    val filter : (Tree -> bool) -> Forest -> Forest
    val filterFirst : (Tree -> bool) -> Forest -> Tree
    val sort : (Tree*Tree -> bool) -> Forest -> Forest

    val concatForests : Forest vector -> Forest

    val putTree : Tree -> string -> string option -> unit
    val putForest : Forest -> string -> string option -> unit
    val printTree : Tree -> unit
    val printForest : Forest -> unit
  end