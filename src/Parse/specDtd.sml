structure SpecDtd = 
   struct
      val fxtElements =
	["fxt:push","fxt:pop","fxt:isEmpty","fxt:get",
	 "fxt:global","fxt:arg","fxt:open",
	 "fxt:spec","fxt:pat","fxt:case","fxt:default",
	 "fxt:current" ,"fxt:if","fxt:switch",
	 "fxt:copyTag","fxt:copyTagDeleteAttribute",
	 "fxt:copyTagAddAttribute","fxt:copyTagReplaceAttribute",
	 "fxt:apply","fxt:copyTagApply","fxt:sml","fxt:tag",
	 "fxt:pi","fxt:text","fxt:currentText","fxt:attribute",
	 "fxt:inc","fxt:dec","fxt:add","fxt:sub",
	 "fxt:getInt","fxt:getTree","fxt:set",
	 "fxt:setForest","fxt:pushForest","fxt:copyContent",
	 "fxt:copyAttributes","fxt:addAttribute","fxt:deleteAttribute",
	 "fxt:replaceAttribute" ,"fxt:ht","fxt:cr",
	 "fxt:sp","fxt:copyType","fxt:key",
	 "fxt:copyKey","fxt:applyKey","fxt:table","fxt:getTableItems",
	 "fxt:apply2","fxt:current2"]

      val fxtElementsData = List.map UniChar.String2Data fxtElements
      
      val fxtAttributes = 
	["lt","toForest","type","sort","rel","arg",
	 "filter","test","ord","finalize","select","selectExp",
	 "code","processor","processorExp","data",
	 "dataExp","step","name","nameExp",
	 "val","valExp","fromString","toString","structure",
	 "file","command","fxt:name","table","key","keyExp","item"]

      val fxtAttributesData = List.map UniChar.String2Data fxtAttributes

      fun initDtdTables() = 
	 let 
	    val dtd = Dtd.initDtdTables()
	    val _ = List.map (Dtd.Element2Index dtd) fxtElementsData
	    val _ = List.map (Dtd.AttNot2Index dtd) fxtAttributesData
	 in 
	   dtd
	 end

      local 
	 val dtd = initDtdTables()
      in 
	val [pushIdx,popIdx,isEmptyIdx,getIdx,globalIdx,argIdx,
	     openIdx,specIdx,patIdx,
	     caseIdx,defaultIdx,currentIdx,
	     ifIdx,switchIdx,copyTagIdx,copyTagDeleteAttributeIdx,
	     copyTagAddAttributeIdx,copyTagReplaceAttributeIdx,
	     applyIdx,copyTagApplyIdx,smlIdx,tagIdx,piIdx,textIdx,
	     currentTextIdx,attributeIdx,incIdx,decIdx,addIdx,subIdx,
	     getIntIdx,getTreeIdx,setIdx,setForestIdx,pushForestIdx,
	     copyContentIdx,copyAttributesIdx,addAttributeIdx,
	     deleteAttributeIdx,replaceAttributeIdx,
	     htIdx,crIdx,spIdx,copyTypeIdx,keyIdx,
	     copyKeyIdx,applyKeyIdx,tableIdx,getTableItemsIdx,
	     apply2Idx,current2Idx] =
	  List.map (Dtd.Element2Index dtd) fxtElementsData

	val [ltIdx,toForestIdx,typeIdx,sortIdx,relationshipIdx,
	     argumentsIdx,filterIdx,testIdx,orderIdx,finalizeIdx,
	     selectIdx,selectExpIdx,codeIdx,processorIdx,processorExpIdx,
	     dataExpIdx,dataIdx,stepIdx,
	     nameIdx,nameExpIdx,valIdx,valExpIdx,fromStringIdx,
	     toStringIdx,structureIdx,fileIdx,commandIdx,fxtNameIdx,
	     tableAttIdx,keyAttIdx,keyExpAttIdx,itemAttIdx] =
	  List.map (Dtd.AttNot2Index dtd) fxtAttributesData
      end
    
   end

