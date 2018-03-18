<fxt:spec>
  <fxt:open file="/home/psi/aberlea/fxtools/fxt/XMLDoc/table.sml" structure="Table"/>
  <fxt:global name='s' type='int'/>
  <fxt:global name='ss' type='int'/>
  <fxt:global name='sss' type='int'/>
  <fxt:global name="fxtIndex" type="Forest"/>
  <fxt:global name='contents' type='Forest'/>
  <fxt:global name='aux' type='Forest'/>
  <fxt:global name="doc" type='Forest'/>
  <fxt:push name='aux' val='emptyForest'/>

  <fxt:pat>/chapter[@title]</fxt:pat>
    <fxt:push name='s' val='0'/>
    <fxt:push name='ss' val='0'/>
    <fxt:push name='sss' val='0'/>
    <fxt:push name="fxtIndex" val="emptyForest"/>
    <fxt:push name='contents' val="emptyForest"/>
    <fxt:pushForest name='doc'>
      <fxt:apply/>
    </fxt:pushForest>
    <html>
      <head>
	<title>
          <fxt:attribute name='title'/>
        </title>
      </head>
       <body bgcolor="#FFFFFF">
         <h1>
          <a href="http://www.informatik.uni-trier.de/~aberlea/Fxt/">&lt;img src="Images/fxt.jpg" alt="fxt - " align="CENTER" border="0"&gt;</a>
          The Functional XML Transformation Tool 
         </h1>
         <h1><fxt:attribute name='title'/></h1>

	 <fxt:if test='(Globals.get G.inputFile)="userGuide.xml"'>
           <p>&lt;IMG SRC="Images/shadow-big.jpg" ALT="-----------------" WIDTH="100%"&gt;</p>
           <h2>Contents:</h2>
           <fxt:get name="contents"/>
           <fxt:pop name='contents'/>
	 </fxt:if>

         <p>&lt;img SRC="Images/shadow-big.jpg" ALT="-----------------" WIDTH="100%"&gt;</p>
         <fxt:get name='doc'/>
         <fxt:pop name='doc'/>

	 <fxt:if test='(OS.Path.file (Globals.get G.inputFile))="userGuide.xml"'>
           <p>&lt;img SRC="Images/shadow-big.jpg" ALT="-----------------" WIDTH="100%"&gt;</p>
           <h2>fxt actions index:</h2>
           <table align='center' width='80%'>
             <fxt:get name="fxtIndex" 
                  sort='(fn t => 
                        ((getTextContent 
                           (filterFirst 
                              (hasElementType (String2Vector "a")) 
                              (sons t)))))' 
                  finalize='split 3 (String2Vector "tr")' 
                  lt='UnicodeVectorlt'/>
           </table>
	 </fxt:if>

         <p>&lt;img SRC="Images/shadow-big.jpg" ALT="-----------------" WIDTH="100%"&gt;</p>
         <ADDRESS>
           <A HREF="http://www.informatik.uni-trier.de/~aberlea/">A. Berlea</A>
          (<A HREF="mailto:aberlea@psi.uni-trier.de">aberlea@psi.uni-trier.de</A>)
         </ADDRESS>
       </body>
     </html>

  <fxt:pat>//section[@title]</fxt:pat>
    <fxt:push name="contents" val="emptyForest"/>
    <fxt:set name='s' val='(Globals.get G.s)+1'/>
    <fxt:set name='ss' val='0'/>
    <fxt:set name='sss' val='0'/>
    <a>
      <fxt:addAttribute 
          name='name' 
          valExp='replaceSpaces 
                  (getAttribute 
                    (String2Vector "title") 
                    current)'/>
    </a>
    <h2><fxt:get name='s'/>. <fxt:attribute name='title'/></h2>
    <p><fxt:apply/></p>
    &lt;img src="Images/shadow-med.jpg" alt="------------" width="100%"&gt;
    <fxt:set name='aux' val='(Globals.get G.contents)'/>
    <fxt:pop name="contents"/>
    <fxt:setForest name="contents">
      <fxt:get name='contents'/>
      &lt;br&gt;
      <a>
        <fxt:addAttribute 
           name='href' 
           valExp='concatVectors 
                 ((String2Vector "#"),
                  (replaceSpaces 
                    (getAttribute 
                       (String2Vector "title") 
                       current)))'/>
        <fxt:get name='s'/>. <fxt:attribute name='title'/>
      </a>
      <fxt:get name="aux"/>
    </fxt:setForest>

  <fxt:pat>//subsection[@title]</fxt:pat>
    <fxt:push name="contents" val="emptyForest"/>
    <fxt:set name='ss' val='(Globals.get G.ss)+1'/>
    <fxt:set name='sss' val='0'/>
    <a>
      <fxt:addAttribute 
          name='name' 
          valExp='replaceSpaces 
                (getAttribute 
                  (String2Vector "title") current)'/>
    </a>
    <h3><fxt:get name='s'/>.<fxt:get name='ss'/>. <fxt:attribute name='title'/></h3>
    <p><fxt:apply/></p>
    &lt;img src="Images/shadow-small.jpg" ALT="------------" width="100%"&gt;
    <fxt:set name='aux' val='(Globals.get G.contents)'/>
    <fxt:pop name="contents"/>
    <fxt:setForest name="contents">
      <fxt:get name='contents'/>
      &lt;br&gt;
      &amp;nbsp;
      &amp;nbsp;
      <a>
        <fxt:addAttribute 
             name='href' 
             valExp='concatVectors 
                  ((String2Vector "#"),
                   (replaceSpaces 
                     (getAttribute 
                        (String2Vector "title") 
                         current)))'/>
        <fxt:get name='s'/>.<fxt:get name='ss'/>. <fxt:attribute name='title'/>
      </a>
      <fxt:get name="aux"/>
    </fxt:setForest>

  <fxt:pat>//subsubsection[@title]</fxt:pat>
    <fxt:set name='sss' val='(Globals.get G.sss)+1'/>
    <a>
      <fxt:addAttribute 
          name='name' 
          valExp='replaceSpaces 
                (getAttribute 
                   (String2Vector "title") current)'/>
    </a>
    <h4><fxt:get name='s'/>.<fxt:get name='ss'/>.<fxt:get name='sss'/> <fxt:attribute name='title'/></h4>
    <p><fxt:apply/></p>
    <fxt:setForest name="contents">
      <fxt:get name="contents"/>
      &lt;br&gt;
      &amp;nbsp;
      &amp;nbsp;
      &amp;nbsp;
      &amp;nbsp;
    <a>
      <fxt:addAttribute 
         name='href' 
         valExp='concatVectors 
                ((String2Vector "#"),
                 (replaceSpaces 
                    (getAttribute 
                        (String2Vector "title") 
                        current)))'/>
      <fxt:get name='s'/>.<fxt:get name='ss'/>.<fxt:get name='sss'/> <fxt:attribute name='title'/>
    </a>
      </fxt:setForest>

  <fxt:pat>//pre</fxt:pat>
    <fxt:copyTagApply/>

  <fxt:pat>//link[@url][""]</fxt:pat>
    <a><fxt:addAttribute 
          name='href' 
          valExp='getAttribute (String2Vector "url") current'/><b><fxt:text code='getTextContent current'/></b></a>
  <fxt:pat>//notice</fxt:pat>
    <p>Notice: <fxt:apply/></p>
  <fxt:pat>//source</fxt:pat>
    <blockquote><pre><fxt:apply/></pre></blockquote>
  <fxt:pat>//p</fxt:pat>
    <p><fxt:apply/></p>
  <fxt:pat>//bold</fxt:pat>
    <b><fxt:apply/></b>
  <fxt:pat>//quote</fxt:pat>
    <i><fxt:apply/></i>
  <fxt:pat>//file</fxt:pat>
    <blockquote>
      <pre><fxt:text 
             code='
let
  val f = (Vector2String (getTextContent current))
  val s = 
String.implode
(List.concat
(List.map 
(fn c => 
  if c=(#"&lt;") then [#"&amp;",#"l",#"t",#";"]
  else if c=(#"&amp;") then [#"&amp;",#"a",#"m",#"p",#";"]
  else if c=(#">") then [#"&amp;",#"g",#"t",#";"]
  else [c]) (String.explode (File2String f))))
in 
String2Vector s
end
'/></pre>
    </blockquote>
  <fxt:pat>//example</fxt:pat>
    <h3>Example:</h3>
    &lt;hr SIZE="3" WIDTH="100%"&gt;
    <fxt:apply/>
    &lt;hr SIZE="3" WIDTH="100%"&gt;
  <fxt:pat>//img</fxt:pat>
    <img vspace='20'><fxt:addAttribute name='src' valExp="(getTextContent current)"/></img>
  <fxt:pat>//comment</fxt:pat>
  <fxt:pat>//def[@name]</fxt:pat>
  <fxt:pat>//fxtdef[@name]</fxt:pat>
    <fxt:setForest name="fxtIndex">
      <fxt:get name="fxtIndex"/>
      <td><a><fxt:addAttribute 
                name='href' 
                valExp='concatVectors 
                      ((String2Vector "#"),
                       (getAttribute 
                          (String2Vector "name") 
                           current))'/><fxt:attribute name='name'/></a></td>
      </fxt:setForest>
    <a>
      <fxt:addAttribute 
         name='name' 
         valExp='getAttribute 
                (String2Vector "name") current'/>
      <fxt:apply/>
    </a>
  <fxt:pat>default</fxt:pat>
    <fxt:current/>
</fxt:spec>