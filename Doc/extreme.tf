<fxt:spec>
  <fxt:global name='s' type='int'/>
  <fxt:global name='ss' type='int'/>
  <fxt:global name='sss' type='int'/>
  <fxt:global name='footnoteref' type='int'/>
  <fxt:global name='ftnoteno' type='int'/>
  <fxt:global name="ftnotes" type="Forest"/>
  <fxt:global name="fxtIndex" type="Forest"/>
  <fxt:global name='contents' type='Forest'/>
  <fxt:global name='aux' type='Forest'/>
  <fxt:global name="doc" type='Forest'/>
  <fxt:global name="title" type='Forest'/>
  <fxt:push name='aux' val='emptyForest'/>

  <fxt:key 
         name='footnotes'
         select='//ftnote' 
         key='id'/>

  <fxt:table 
         name='references'
         select='//bibitem' 
         key='id'
         item='fn t =>
	         (getTextContent 
		   (filterFirst 	
  	             (hasElementType (String2Vector "bib"))
                     (sons t)))'/>

  <fxt:table 
         name='crossReferences'
         select='//*[@id]' 
         key='id'
         item='getAttribute (String2Vector "id")'/>

  <fxt:pat>/paper/front/title/""</fxt:pat>
    <fxt:pushForest name="title"><fxt:current/></fxt:pushForest>

  <fxt:pat>/paper</fxt:pat>
    <fxt:push name='s' val='0'/>
    <fxt:push name='ss' val='0'/>
    <fxt:push name='sss' val='0'/>
    <fxt:push name='footnoteref' val='0'/>
    <fxt:push name='ftnoteno' val='0'/>
    <fxt:push name="ftnotes" val="emptyForest"/>
    <fxt:push name="fxtIndex" val="emptyForest"/>
    <fxt:push name='contents' val="emptyForest"/>
    <fxt:pushForest name='doc'>
      <fxt:apply/>
    </fxt:pushForest>
    <html>
      <head>
	<title>
          <fxt:get name="title"/>
        </title>
      </head>
      <body bgcolor="#FFFFFF">
        <h3><fxt:get name="title"/></h3>
        <fxt:pop name="title"/>
        <fxt:get name='doc'/>
        <fxt:pop name='doc'/>
        <h3>Footnotes:</h3>
        <fxt:get name="ftnotes"/>
        <fxt:pop name='ftnotes'/>
      </body>
    </html>

  <fxt:pat>/paper/front</fxt:pat>
    <fxt:apply/>

  <fxt:pat>//keyword</fxt:pat>
    [<fxt:currentText/>]

  <fxt:pat>/paper/front/author</fxt:pat>
    <ul><h4>Author:</h4><fxt:apply/></ul>
  <fxt:pat>/paper/front/author/fname</fxt:pat>
    <li>First name:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/surname</fxt:pat>
    <li>Surname:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/jobtitle</fxt:pat>
    <li>Jobtitle:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/address</fxt:pat>
    <li>Address:<ul><fxt:apply/></ul></li>
  <fxt:pat>/paper/front/author/address/affil</fxt:pat>
    <li>Affiliation:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/address/subaffil</fxt:pat>
    <li>Subaffiliation:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/address/city</fxt:pat>
    <li>City:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/address/country</fxt:pat>
    <li>Country:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/address/email</fxt:pat>
    <li>E-mail:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/address/web</fxt:pat>
    <li>Web:<fxt:apply/></li>
  <fxt:pat>/paper/front/author/bio</fxt:pat>
    <li>Biography:<fxt:apply/></li>

  <fxt:pat>/paper/front/abstract</fxt:pat>
    <h5>Abstract:</h5><small><fxt:apply/></small>

  <fxt:pat>//&lt;subsec1|subsec2&gt;/title</fxt:pat>
    <fxt:apply/>

  <fxt:pat>//section/title</fxt:pat>
    <fxt:inc name='s'/>
    <fxt:set name='ss' val='0'/>
    <fxt:set name='sss' val='0'/>
    <h3><fxt:get name='s'/>. <fxt:apply/></h3>

  <fxt:pat>//subsec1/title/""</fxt:pat>
    <fxt:inc name='ss'/>
    <fxt:set name='sss' val='0'/>
    <h4><fxt:get name='s'/>.<fxt:get name='ss'/>. <fxt:current/></h4>
    <p><fxt:apply/></p>

  <fxt:pat>//subsec2/title/""</fxt:pat>
    <fxt:inc name='sss'/>
    <h5><fxt:get name='s'/>.<fxt:get name='ss'/>.<fxt:get name='sss'/>. <fxt:current/></h5>
    <p><fxt:apply/></p>

  <fxt:pat>//sgml.block</fxt:pat>
    <blockquote><pre><fxt:apply/></pre></blockquote>
  <fxt:pat>//sgml</fxt:pat>
    <tt><fxt:apply/></tt>

  <fxt:pat>//verbatim</fxt:pat>
    <blockquote><pre><fxt:apply/></pre></blockquote>

  <fxt:pat>//randlist[@style="simple"]</fxt:pat>
    <ul><fxt:apply/></ul>

  <fxt:pat>//randlist</fxt:pat>
    <ul type="disc"><fxt:apply/></ul>

  <fxt:pat>//seqlist</fxt:pat>
    <ol><fxt:apply/></ol>

  <fxt:pat>//para</fxt:pat>
    <p><fxt:apply/></p>

  <fxt:pat>//bibliog</fxt:pat>
    <h2>Bibliography:<table><fxt:apply/></table></h2>
  <fxt:pat>//bibitem</fxt:pat>
    <tr><fxt:apply/></tr>
  <fxt:pat>//bib</fxt:pat>
    <td valign="top">[<fxt:apply/>]</td>
  <fxt:pat>//pub</fxt:pat>
    <td valign="top"><fxt:apply/></td>

  <fxt:pat>//bibref</fxt:pat>
     [<fxt:getTableItems 
            name='references' key='refloc'/>]
  <fxt:pat>//xref</fxt:pat>
     <i>[<fxt:getTableItems 
           name='crossReferences' key='refloc'/>]</i>

  <fxt:pat>//figure</fxt:pat>
    <fxt:apply/>
  <fxt:pat>//figure/title</fxt:pat>
    <i><fxt:copyContent/></i>
  <fxt:pat>//figure/caption</fxt:pat>
    <i><fxt:apply/></i>

  <fxt:pat>//graphic[@figname="f1"]</fxt:pat>
    &lt;img src="images/f1.jpg" width="400" height="300"&gt;
  <fxt:pat>//graphic[@figname="f2"]</fxt:pat>
    &lt;img src="images/f2.jpg" width="400" height="300"&gt;
  <fxt:pat>//graphic[@figname="f5"]</fxt:pat>
    &lt;img src="images/f5.jpg" width="150" height="300"&gt;

  <fxt:pat>//graphic[@figname]</fxt:pat>
    &lt;img src="images/<fxt:text 
	code='(getAttribute 
               (String2Vector "figname") current)'/>.jpg"&gt;
  <fxt:pat>//inline.graphic[@figname]</fxt:pat>
    &lt;img src="images/<fxt:text 
	code='(getAttribute 
               (String2Vector "figname") current)'/>.jpg"&gt;

  <fxt:pat>//acronym.grp</fxt:pat>
    <fxt:apply/>
  <fxt:pat>//acronym.grp/&lt;acronym&gt;</fxt:pat>
    <fxt:apply/>
  <fxt:pat>//acronym.grp/expansion/""</fxt:pat>
    (<fxt:current/>)

  <fxt:pat>//highlight[@style="ital"]</fxt:pat>
    <i><fxt:apply/></i>
  <fxt:pat>//highlight[@style="bold"]</fxt:pat>
    <b><fxt:apply/></b>

  <fxt:pat>//deflist</fxt:pat>
    <table><fxt:apply/></table>
  <fxt:pat>//deflist/def.item</fxt:pat>
    <tr><fxt:apply/></tr>
  <fxt:pat>//deflist/def.item/def.term</fxt:pat>
    <td><fxt:apply/></td>
  <fxt:pat>//deflist/def.item/def</fxt:pat>
    <td><fxt:apply/></td>

  <fxt:pat>//super</fxt:pat>
    <sup><fxt:apply/></sup>

  <fxt:pat>//fnref</fxt:pat>
    <fxt:inc name="footnoteref"/>
    <sup>
      <a><fxt:addAttribute 
       name="href" 
       valExp='concatVectors 
                ((String2Vector "#"),
                 (getAttribute 
                  (String2Vector "refloc") current))'
         /><fxt:get name="footnoteref"/></a></sup>  

  <fxt:pat>//ftnote</fxt:pat>
    <fxt:inc name="ftnoteno"/>
    <fxt:setForest name="ftnotes">
      <fxt:get name='ftnotes'/>
      <p><a><fxt:addAttribute 
       name="name" 
       valExp='getAttribute (String2Vector "id") current'
         /><sup><fxt:get name="ftnoteno"/></sup><fxt:copyKey name="footnotes" key="id"/></a></p>
    </fxt:setForest>

  <fxt:pat>//""</fxt:pat>
    <fxt:text
code='
let
  val txt = getTextContent current
  fun replace c = 
    case c of
      0wx3A3 => String2Vector "Sigma"
     |0wx3B4 => String2Vector "delta"
     |0wx394 => String2Vector "Delta"
     |0wx3C1 => String2Vector "rho"
     |0wx3B5 => String2Vector "epsilon"
     |0wx3C6 => String2Vector "phi"
     |0wx2208 => String2Vector " is in "
     |0wx2265 => String2Vector ">="
     |0wx252 => String2Vector "ue"
     |0wx2192 => String2Vector "-->"
     |0wx2190 => String2Vector "&lt;--"
     |0wx2032 => #[0wx27]
     |0w215 => String2Vector " x "
     |0w183 => String2Vector " x "
     |0wx21D4 => String2Vector "&lt;=>"
     |0wx2203 => String2Vector "it exists "
     |0wx2264 => String2Vector "&lt;="
     |0wx2286 => String2Vector " included in or equal to "
     |0wx2282 => String2Vector " strictly included in "
     |0wx222A => String2Vector "U"
     |0wx2229 => String2Vector "/\\"
     |0wx2260 => String2Vector "&lt;>"
     |0wx2033 => #[0wx27,0wx27]
     | _ => #[c]
  val t1 = Vector.map replace txt
in
  Vector.foldr (fn (v1,v2) => concatVectors (v1,v2)) #[] t1
end
'
/>

</fxt:spec>