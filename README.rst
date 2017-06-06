.. role:: bash(code)
    :language: bash

.. role:: java(code)
    :language: java

.. role:: python(code)
    :language: python

.. role:: xml(code)
    :language: xml


============================================
XML output generator for Open Fortran Parser
============================================

.. image:: https://travis-ci.org/mbdevpl/open-fortran-parser-xml.svg?branch=master
    :target: https://travis-ci.org/mbdevpl/open-fortran-parser-xml
    :alt: build status from Travis CI

.. image:: https://img.shields.io/pypi/l/open-fortran-parser.svg
    :target: https://travis-ci.org/mbdevpl/open-fortran-parser-xml/blob/master/NOTICE
    :alt: license

Implementation has 2 parts: the XML generator written in Java, and Python wrapper for the generator.

In this file, first the Java implementation is described and then the Python wrapper.


==========================
Java XML generator for OFP
==========================

This is an extension of Open Fortran Parser (OFP), which outputs abstract syntaxt tree (AST)
of parsed Fortran file in XML format - to a file or to `System.out`.


------------
dependencies
------------

*   Java 1.8

*   Open Fortran Parser 0.8.4-1

    https://github.com/mbdevpl/open-fortran-parser/releases/tag/v0.8.4-1

    This is a patched version of OFP. Specifically, `FortranParserActionPrint` class in OFP
    could not be properly subclassed due to access levels of members of that class, so for example
    writing my own printer would introduce a lot of code duplication. Patch resolves this,
    without affecting any functionality.

    The patch also resolves an issue when compiling with recent GCC versions.

*   ANTRL 3.3 (dependency of Open Fortran Parser)

    http://www.antlr3.org/download/

*   Apache Commons CLI 1.4 (or later)

    https://commons.apache.org/proper/commons-cli/download_cli.cgi


------------
how to build
------------

Get dependencies, either manually, or using the provided script:

.. code:: bash

    python3 open_fortran_parser/dev_dependencies.py
    export CLASSPATH="${CLASSPATH}:$(pwd)/lib/*"

Build:

.. code:: bash

    ant
    export CLASSPATH="${CLASSPATH}:$(pwd)/dist/*"

This will create a `.jar` file in `dist` directory.


----------
how to run
----------

.. code:: bash

    java fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
      --output output.xml --verbosity 0~100 input.f

where:

*   The :bash:`--verbosity` flag controls verbosity of the parse tree. Defaluts to :bash:`100`
    when omitted.

    *   Maximum, :bash:`100`, means that all details picked up by Open Fortran Parser
        will be preserved.

    *   Minimum, :bash:`0`, means that tree will contain only what is needed to reconstruct
        the program without changing it's meaning.

*   The :bash:`--output` flag controls where the XML should be written. Defaults to standard output
    when omitted.

and remaining command-line options are exactly as defined in OFP 0.8.4.

To parse :bash:`some_fortran_file.f` and save XML output in :bash:`tree.xml` with minimum verbosity:

.. code:: bash

    java fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
      --output tree.xml --verbosity 0 some_fortran_file.f

And to dump XML with maximum verbosity to console:

.. code:: bash

    java fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
      --verbosity 100 some_fortran_file.f


-----------------
AST specification
-----------------

In progress.

Root node is :xml:`<ofp>`, it has one subnode :xml:`<file>`.


================================
Python wrapper for the generator
================================

.. image:: https://img.shields.io/pypi/v/open-fortran-parser.svg
    :target: https://pypi.python.org/pypi/open-fortran-parser
    :alt: package version from PyPI

.. image:: https://coveralls.io/repos/github/mbdevpl/open-fortran-parser-xml/badge.svg?branch=master
    :target: https://coveralls.io/github/mbdevpl/open-fortran-parser-xml
    :alt: test coverage from Coveralls

Using the wrapper should not require any special knowledge about the generator itself, other than
knowing the abstract syntax tree (AST) specification.


------------
dependencies
------------

Java 1.8.

Python version >= 3.6.

Python libraries as specified in `<requirements.txt>`_.

Building and running tests additionally requires packages listed in `<dev_requirements.txt>`_.


------------
how to build
------------

.. code:: bash

  - pip3 install -U -r dev_requirements.txt
    python3 setup.py sdist --formats=gztar,zip
    python3 setup.py bdist_wheel


----------
how to run
----------

The wrapper can be used as a script, or as a library.

Before running, however, please make sure that dependencies are configured correctly.
You can do that by either following the "how to build" section for Java implementation above,
or by executing this:

.. code:: bash

    python3 open_fortran_parser/dependencies.py
    export CLASSPATH="${CLASSPATH}:$(pwd)/lib/*"

as script
~~~~~~~~~

.. code:: bash

    $ python3 -m open_fortran_parser -h
    usage: open_fortran_parser [-h] [-v VERBOSITY] input [output]

    Python wrapper around XML generator for Open Fortran Parser 0.8.4

    positional arguments:
      input                 path to Fortran source code file
      output                writable path for where to store resulting XML,
                            defaults to stdout if no path provided (default: None)

    optional arguments:
      -h, --help            show this help message and exit
      -v VERBOSITY, --verbosity VERBOSITY
                            level of verbosity, from 0 to 100 (default: 100)

    Copyright 2017 Mateusz Bysiek https://mbdevpl.github.io/, Apache License 2.0


as library
~~~~~~~~~~

.. code:: python

    from open_fortran_parser import parse

    xml = parse('my_legacy_code.f', verbosity=0)


-------
testing
-------

.. code:: bash

    python3 -m pylint --load-plugins=pylint.extensions.mccabe --docstring-min-length 5 \
      --no-docstring-rgx "^(test)?_|.*Tests$" --unsafe-load-any-extension y \
      --output-format colorized  --reports y $(find . -name "*.py")
    python3 -m coverage run --branch -m unittest discover --verbose
    python3 -m coverage report --show-missing
    python3 -m coverage html
