<!-- common settings for CLISP Implementation Notes formatting -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0" xmlns="http://www.w3.org/1999/xhtml">

<xsl:param name="html.stylesheet" select="'impnotes.css'"/>
<xsl:param name="link.mailto.url" select="'mailto:clisp-list@sf.net'"/>
<!-- xsl:param name="suppress.navigation" select="0"/-->
<xsl:param name="inherit.keywords" select="0"/>

<!-- xsl:template name="user.header.content">
 <p>CLISP Implementation Notes [user.header.content]</p><hr width="100%"/>
</xsl:template -->

<!-- http://article.gmane.org/gmane.text.docbook.apps:9779 -->
<xsl:preserve-space elements="entry"/>

<!-- http://article.gmane.org/gmane.text.docbook.apps:11014
     apply-templates is mapc on children
     apply-imports is call-next-method -->

<xsl:template match="literal[@role = 'type'
      or @role = 'method' or @role = 'data' or @role = 'byte']">
 <span class="{@role}"><xsl:apply-imports/></span>
</xsl:template>

<xsl:template match="isbn" mode="bibliography.mode">
 <xsl:text>ISBN&#160;</xsl:text>
 <xsl:apply-templates mode="bibliography.mode"/>
 <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="quote[@role = 'package']">
 <strong class="{@role}"><xsl:apply-imports/></strong>
</xsl:template>

<xsl:template match="firstterm">
 <strong class="first"><xsl:apply-imports/></strong>
</xsl:template>

<xsl:param name="generate.toc"> <!-- toc in refentry -->
refentry  toc
appendix  toc,title
article/appendix  nop
article   toc,title
book      toc,title,figure,table,example,equation
chapter   toc,title
part      toc,title
preface   toc,title
qandadiv  toc
qandaset  toc
reference toc,title
sect1     toc
sect2     toc
sect3     toc
sect4     toc
sect5     toc
section   toc
set       toc,title
</xsl:param>

<xsl:template match="varlistentry/term">
 <xsl:call-template name="anchor"/>
 <xsl:apply-templates/>
 <xsl:if test="following-sibling::term"><br/></xsl:if>
</xsl:template>

<xsl:template match="comment()">  <!-- pass through comments -->
 <xsl:text>&#10;</xsl:text>
 <xsl:comment><xsl:value-of select="normalize-space(.)"/></xsl:comment>
 <xsl:if test="not(following-sibling::comment())">
  <xsl:text>&#10;</xsl:text></xsl:if>
</xsl:template>

<xsl:param name="generate.section.toc.level" select="3"/>
<xsl:param name="generate.index" select="1"/>
<xsl:param name="refentry.generate.title" select="1"/>
<xsl:param name="use.id.as.filename" select="1"/>
<xsl:param name="section.autolabel" select="1"/>
<xsl:param name="section.label.includes.component.label" select="1"/>

<xsl:template name="user.footer.content">
 <xsl:if test="refentryinfo"><div class="refentryinfo">
   <hr width="100%"/><table width="100%">
    <th><td align="left"><xsl:value-of select="refentryinfo/title"/></td>
     <td align="center"><xsl:value-of select="refentryinfo/subtitle"/></td>
     <td align="right"><xsl:value-of select="refentryinfo/date"/></td></th>
 </table></div></xsl:if>
 <xsl:if test="bookinfo"><div class="bookinfo">
   <hr width="100%"/><table width="100%">
    <th><td align="left"><xsl:value-of select="bookinfo/subtitle"/></td>
     <td align="right"><xsl:value-of select="bookinfo/date"/></td></th>
 </table></div></xsl:if>
 <div class="custom-footer"><hr width="100%"/><table width="100%">
   <tr><td align="left"><a href="http://clisp.cons.org">
      <img src="clisp.png" width="48" height="48" alt="[CLISP home]"/></a></td>
    <td align="center"><a href="http://www.gnu.org">
      <img src="http://www.gnu.org/graphics/gnubanner.jpg" width="468"
           height="60" alt="[Come and see what GNU creates for YOU]"/></a></td>
    <td align="right"><a href="http://sourceforge.net">
      <img src="http://sourceforge.net/sflogo.php?group_id=1355&#38;amp;type=2"
           width="125" height="37" alt="[SourceForge]"/></a></td></tr>
 </table></div>
</xsl:template>

</xsl:stylesheet>
