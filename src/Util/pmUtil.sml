structure PmUtil =
  struct
    fun makeDocList docTreeVector matchTreeArray = 
      let
	fun doit index res = 
	  if index >= Vector.length docTreeVector then (rev res)
	  else doit 
	    (index+1) 
	    (((Vector.sub (docTreeVector, index)),(Array.sub (matchTreeArray, index)))::res)
      in
	doit 0 []
      end
  end