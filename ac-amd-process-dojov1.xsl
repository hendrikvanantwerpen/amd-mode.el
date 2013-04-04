<xsl:transform version="1.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- Transform js-doc-parse dojov1 output to elisp

This transforms the dojov1 output of js-doc-parse to a simple structure for use in Emacs auto completion.

It lists module ids with either a list of properties and methods or a parameter list string if it's
a function. We don't support properties on functions at the moment.

-->
               
    <xsl:param name="name" select="'unknown'"/>
    <xsl:output method="text"/>
 
    <xsl:template match="javascript">
      <xsl:text>;; Generated with ac-amd-process-dojov1.xsl&#10;;; Don't change by hand, but regenerate from js-doc-parse documentation.</xsl:text>
      <xsl:text>&#10;(require 'ac-amd)&#10;</xsl:text>
      <xsl:text>&#10;(defun ac-amd-</xsl:text><xsl:value-of select="$name"/><xsl:text>-completions-setup ()</xsl:text>
      <xsl:text>&#10;  (setq ac-amd-properties (append (list</xsl:text>
      <xsl:apply-templates select="object[@type='object']"/>
      <xsl:text>) ac-amd-properties))</xsl:text>
      <xsl:text>&#10;  (setq ac-amd-functions (append (list</xsl:text>
      <xsl:apply-templates select="object[@type='function']"/>
      <xsl:text>) ac-amd-functions))</xsl:text>
      <xsl:text>&#10;  (setq ac-amd-constructors (append (list</xsl:text>
      <xsl:apply-templates select="object[@classlike='true']"/>
      <xsl:text>) ac-amd-constructors))</xsl:text>
      <xsl:text>)</xsl:text>
      <xsl:text>&#10;(provide 'ac-amd-</xsl:text><xsl:value-of select="$name"/><xsl:text>-completions)&#10;</xsl:text>
    </xsl:template>

    <xsl:template match="object[@type='function']">
      <xsl:text>&#10;    (cons "</xsl:text>
      <xsl:value-of select="@location"/>
      <xsl:text>" "(</xsl:text>
      <xsl:apply-templates select="parameters/parameter"/>
      <xsl:text>)")</xsl:text>
    </xsl:template>

    <xsl:template match="object[@classlike='true']">
      <xsl:text>&#10;    (cons "</xsl:text>
      <xsl:value-of select="@location"/>
      <xsl:text>" "(</xsl:text>
      <xsl:apply-templates select="methods/method[@name='constructor']/parameters/parameter"/>
      <xsl:text>)")</xsl:text>
    </xsl:template>

    <xsl:template match="object[@type='object']">
      <xsl:text>&#10;    (cons "</xsl:text>
      <xsl:value-of select="@location"/>
      <xsl:text>" (list</xsl:text>
      <xsl:apply-templates select="properties/property|methods/method"/>
      <xsl:text>))</xsl:text>
    </xsl:template>

    <xsl:template match="property[not(starts-with(@name,'_'))]">
        <xsl:text> "</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>"</xsl:text>
    </xsl:template>

    <xsl:template match="method[not(starts-with(@name,'_'))]">
        <xsl:text> "</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>(</xsl:text>
        <xsl:apply-templates select="parameters/parameter"/>
        <xsl:text>)"</xsl:text>
    </xsl:template>

    <xsl:template match="parameter">
        <xsl:if test="@usage='optional'"><xsl:text>_</xsl:text></xsl:if>
        <xsl:value-of select="@name"/>
        <xsl:if test="@usage='optional'"><xsl:text>_</xsl:text></xsl:if>
        <xsl:if test="position()!=last()"><xsl:text>, </xsl:text></xsl:if>
    </xsl:template>
    
    <xsl:template match="*">
    </xsl:template>

</xsl:transform>
