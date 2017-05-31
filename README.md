# XML output generator for Open Fortran Parser

This is an extension of Open Fortran Parser (OFP), which outputs abstract syntaxt tree (AST)
in XML format - to a file or to 'System.out'.


## dependencies

  - Open Fortran Parser 0.8.4-1

  https://github.com/mbdevpl/open-fortran-parser/releases/tag/v0.8.4-1

  This is a patched version of OFP. Specifically, 'FortranParserActionPrint' class in OFP
  could not be properly subclassed due to access levels of members of that class, so for example
  writing my own printer would introduce a lot of code duplication. Patch resolves this,
  without affecting any functionality of the Parser.

  The patch also resolves some issue when compiling with recent GCC versions.

  - ANTRL 3.3 (dependency of Open Fortran Parser)

  http://www.antlr3.org/download/

  - Apache Commons CLI

  https://commons.apache.org/proper/commons-cli/download_cli.cgi


## how to run


## AST specification

In progress.

Root node is '<ofp>', it has one subnode '<file>'.
