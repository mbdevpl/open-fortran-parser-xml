# XML output generator for Open Fortran Parser

This is an extension of Open Fortran Parser (OFP), which outputs abstract syntaxt tree (AST)
in XML format of parsed Fortran file - to a file or to `System.out`.


## dependencies

  - Open Fortran Parser 0.8.4-1

    https://github.com/mbdevpl/open-fortran-parser/releases/tag/v0.8.4-1

    This is a patched version of OFP. Specifically, `FortranParserActionPrint` class in OFP
    could not be properly subclassed due to access levels of members of that class, so for example
    writing my own printer would introduce a lot of code duplication. Patch resolves this,
    without affecting any functionality.

    The patch also resolves an issue when compiling with recent GCC versions.

  - ANTRL 3.3 (dependency of Open Fortran Parser)

    http://www.antlr3.org/download/

  - Apache Commons CLI 1.4 (or later)

    https://commons.apache.org/proper/commons-cli/download_cli.cgi

## how to build

Put dependencies in `lib` directory, and run:

```
ant
```

This will create a `.jar` file in `dist` directory.

## how to run

```bash
java -cp "/path/to/dependencies/*" fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
  --output output.xml --verbosity 0~100 input.f
```

where:

  - The `--verbosity` flag controls verbosity of the parse tree. Defaluts to `100` when omitted.

      * Maximum, `100`, means that all details picked up by Open Fortran Parser will be preserved.

      * Minimum, `0`, means that tree will contain only what is needed to reconstruct the program
        without changing it's meaning.

  - The `--output` flag controls where the XML should be written. Defaults to standard output
    when omitted.

... and remaining command-line options are exactly as defined in OFP 0.8.4.

To parse `some_fortran_file.f` and save XML output in `tree.xml` with minimum verbosity:

```bash
java -cp "./lib/*:./dist/*" fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
  --output tree.xml --verbosity 0 some_fortran_file.f
```

And to dump XML with maximum verbosity to console:

```bash
java -cp "./lib/*:./dist/*" fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
  --verbosity 100 some_fortran_file.f
```


## AST specification

In progress.

Root node is `<ofp>`, it has one subnode `<file>`.
