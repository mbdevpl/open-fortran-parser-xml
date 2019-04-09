.. role:: bash(code)
    :language: bash

.. role:: fortran(code)
    :language: fortran

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

.. image:: https://ci.appveyor.com/api/projects/status/github/mbdevpl/open-fortran-parser-xml?branch=master&svg=true
    :target: https://ci.appveyor.com/project/mbdevpl/open-fortran-parser-xml
    :alt: build status from AppVeyor

.. image:: https://api.codacy.com/project/badge/Grade/1e5602a9efed41998eca0437d84cc1db
    :target: https://www.codacy.com/app/mbdevpl/open-fortran-parser-xml
    :alt: grade from Codacy

.. image:: https://codecov.io/gh/mbdevpl/open-fortran-parser-xml/branch/master/graph/badge.svg
    :target: https://codecov.io/gh/mbdevpl/open-fortran-parser-xml
    :alt: test coverage from Codecov

.. image:: https://img.shields.io/pypi/l/open-fortran-parser.svg
    :target: https://github.com/mbdevpl/open-fortran-parser-xml/blob/master/NOTICE
    :alt: license

Implementation has 2 parts: the XML generator written in Java, and Python wrapper for the generator.

The implementation is tested on Linux, OS X and Windows.

In this file, first the AST specification is described, then the Java implementation,
and then the Python wrapper.

.. contents::
    :backlinks: none


AST specification
=================

For any Fortran file, the resulting XML file has the following structure:

.. code:: xml

    <?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <ofp version="0.8.5-1"> <!-- version of the Open Fortran Parser used -->
      <file path="/path/to/parsed/file.f90">
        <!-- Fortran syntax goes here -->
      </file>
    </ofp>

Root node is :xml:`<ofp>`, it has one subnode :xml:`<file>`.

Inside the :xml:`<file>`, there might be one or many of the following nodes:

*   :xml:`<program>`
*   :xml:`<subroutine>`
*   :xml:`<function>`
*   :xml:`<module>`
*   :xml:`<interface>`
*   ...

Additionally, every XML node that was built using tokens from the source code
(which means almost any XML node) has its source code location described in the following way:

.. code:: xml

    <node col_begin="..." col_end="..." line_begin="..." line_end="..." />

For simplicity, the above XML file boilerplate as well as locations are stripped
from the examples that follow.

For each presented construct, Fortran code snippet and corresponding XML AST is given.


Comments and directives
-----------------------

Comment:

.. code:: fortran

    ! my comment
    !$omp parallel do

.. code:: xml

    <comment text="! my comment"/>
    <comment text="!$omp parallel do"/>

Directive:

.. code:: fortran

    #define NDIMS 3

.. code:: xml

    <directive text="#define NDIMS 3"/>


Nodes :xml:`<comment>` and :xml:`<directive>`
exist to carry comments and preprocessor directives, respectively.
These nodes might be in principle inserted before, after or within any of other nodes,
however, in practice they are either surrounding the top-level nodes (e.g. program or subroutine)
or are placed in-between non-compound declarations and/or statements within them.

Note: compiler directives are comments in Fortran.


Program
-------

.. code:: fortran

    program empty
      ...
    end program empty

.. code:: xml

    <program name="empty">
      <body>
        ...
      </body>
    </program>

In the body, `Declarations`_ followed by any number of statements can be found.

And each of the statements listed after the declarations,
can be one of `Simple statements`_ or `Compound statements`_.


Declarations
------------

A special node :xml:`<specification>` wraps all declarations:

.. code:: xml

    <specification declarations="0" implicits="0" imports="0" uses="0">
      ...
    </specification>

It provides counts for each of the declaration type and contains a collection of declarations,
which can any of the following:

*   :xml:`<use>`
*   :xml:`<declaraion>`
*   ...

The :xml:`<declaraion>` node is special in a sense that it has type attribute that specifies
what kind of declaration it is.


Implicit declaration
~~~~~~~~~~~~~~~~~~~~

.. code:: fortran

    implicit none
    implicit real (A-H,O-Z)

.. code:: xml

    <declaration subtype="none" type="implicit" />
    <declaration subtype="some" type="implicit">
      <type name="real" type="intrinsic" />
      <letter-ranges>
        <letter-range begin="A" end="H" />
        <letter-range begin="O" end="Z" />
      </letter-ranges>
    </declaration>


Variable declaration
~~~~~~~~~~~~~~~~~~~~

.. code:: fortran

    integer i, j

.. code:: xml

    <declaration type="variable">
      <type name="integer" type="intrinsic"/>
      <variables count="2">
        <variable name="i"/>
        <variable name="j"/>
      </variables>
    </declaration>


External declaration
~~~~~~~~~~~~~~~~~~~~

.. code:: fortran

    external omp_get_num_procs


Save declaration
~~~~~~~~~~~~~~~~

.. code:: fortran

    save n


Use
~~~

.. code:: fortran

    use mpi
    use my_interface, only: subroutine1, subroutine2
    use, non_intrinsic :: my_module
    use, intrinsic :: iso_c_binding, only: c_int, c_float

.. code:: xml

    <use name="mpi" />
    <use name="my_interface">
      <only>
        <name id="subroutine1" />
        <name id="subroutine2" />
      </only>
    </use>
    <use name="my_module">
      <nature name="non_intrinsic" />
    </use>
    <use name="iso_c_binding">
      <nature name="intrinsic" />
      <only>
        <name id="c_int" />
        <name id="c_float" />
      </only>
    </use>


Compound statements
-------------------

Compound statements, e.g.:

*   :xml:`<if>`
*   :xml:`<loop>`
*   :xml:`<select>`
*   ...

each have :xml:`<header>` and :xml:`<body>`.


If
~~~

In the header of :xml:`<if>`, an expression is present.

See `Expressions`_ for a definition.


Loop
~~~~

In the header of the :xml:`<loop>`, at least one :xml:`<index-variable>` is present.
It has :xml:`<lower-bound>`, :xml:`<upper-bound>`  and :xml:`<step>`.


Select
~~~~~~

In the body of :xml:`<select>` there multiple :xml:`<case>` nodes.
These are also compound (i.e. each of them has :xml:`<header>` and :xml:`<body>`),
however they exist only within the body of select statement.


Simple statements
-----------------

.. code:: xml

    <statement>
      ...
    </statement>

All simple statements are using :xml:`<statement>` node, which wraps around nodes like:

*   :xml:`<assignment>`
*   :xml:`<pointer-assignment>`
*   :xml:`<call>`
*   :xml:`<open>`
*   :xml:`<close>`
*   :xml:`<write>`
*   :xml:`<format>`
*   :xml:`<print>`
*   :xml:`<allocate>`
*   :xml:`<deallocate>`
*   :xml:`<return>`
*   :xml:`<stop>`
*   :xml:`<continue>`
*   :xml:`<cycle>`
*   :xml:`<arithmetic-if>`
*   ...


Assignment
~~~~~~~~~~

.. code:: fortran

    x = 1

.. code:: xml

    <assignment>
      <target>
        <name id="i" />
      </target>
      <value>
        <literal type="int" value="1" />
      </value>
    </assignment>


Call
~~~~

.. code:: fortran

    call configure
    call initialize()
    call calculate(1, 2)
    call something(thing=my_value)

.. code:: xml

    <call>
      <name hasSubscripts="false" id="configure" type="procedure" />
    </call>
    <call>
      <name hasSubscripts="true" id="initialize" type="procedure">
        <subscripts count="0" />
      </name>
    </call>
    <call>
      <name hasSubscripts="true" id="calculate" type="procedure">
        <subscripts count="2">
          <subscript type="simple">
            <literal  type="int" value="1" />
          </subscript>
          <subscript type="simple">
            <literal type="int" value="2" />
          </subscript>
        </subscripts>
      </name>
    </call>
    <call >
      <name hasSubscripts="true" id="something" type="procedure">
        <subscripts count="1">
          <argument name="thing">
            <name id="my_value" />
          </argument>
        </subscripts>
      </name>
    </call>


Expressions
-----------

Expression might be a single node like:

*   :xml:`<name>`
*   :xml:`<literal>`
*   ...

More complex expressions are built from the :xml:`<operation>` nodes, each of which contains
a collection of :xml:`<operand>` and :xml:`<operator>` nodes. Each operand contains an expression.


Unary operation
~~~~~~~~~~~~~~~

.. code:: fortran

    .not. flag

.. code:: xml

    <operation type="unary">
      <operator operator=".not." />
      <operand>
        <name id="flag" />
      </operand>
    </operation>


Multiary operation
~~~~~~~~~~~~~~~~~~

.. code:: fortran

    'Hello' // ' world'
    5 + x

.. code:: xml

    <operation type="multiary">
      <operand >
        <literal type="char" value="'Hello'" />
      </operand>
      <operator operator="//" />
      <operand>
        <literal type="char" value="' world'" />
      </operand>
    </operation>
    <operation type="multiary">
      <operand>
        <literal type="int" value="5" />
      </operand>
      <operator operator="+" />
      <operand>
        <name id="x" />
      </operand>
    </operation>


Subroutine
----------

Many complex nodes contain :xml:`<header>` and :xml:`<body>`.

The contents of the header depend on the type of the node. For example, in case of subroutines,
it contains list of parameters.


Function
--------

.. code:: fortran

    function foo
      ...
    end function foo

.. code:: xml

    <function name="foo">
      <header>
        ...
      </header>
      <body>
        ...
      </body>
    </function>


Module
------

.. code:: fortran

    module abc
      integer i
      ...
    contains
      subroutine sub()
        ...
      end subroutine sub
      ...
    end module abc

.. code:: xml

    <module name="abc">
      <body>
        <specification declarations="1" implicits="0" imports="0" uses="0">
          <declaration type="variable">
            <type name="integer" type="intrinsic"/>
            <variables count="1">
              <variable name="i"/>
            </variables>
          </declaration>
        </specification>
        ...
      </body>
      <members>
        <subroutine name="sub">
          <header/>
          <body>
            ...
          </body>
        </subroutine>
        ...
      </members>
    </module>


Work in progress
----------------

Remaining details of AST are not decided yet. For the time being, to see implementation details,
please take a look into `<src/fortran/ofp/XMLPrinter.java>`_.


Unhandled corner cases
----------------------

in certain corner cases, the parse tree might deviate from the above description.

This might be due to two main reasons:

1)   Some feature is not yet implemented in this XML output generator
2)   The events provided by OFP are not sufficient to generate a correct tree.

In case 1, all contributions to this project are very welcome. The implementation of any one
of the missing features might not be very troublesome. The main reason why many of those features
are not implemented yet is because the Fortran codes the current contributors work with
do not use them.

In case 2, there is a need to dynamically reorder/modify/delete nodes, or otherwise manipulate
existing parse tree while adding new nodes. Contributions are also very welcome,
but implementation might be much more challenging in this case.


Java XML generator for OFP
==========================

.. image:: https://img.shields.io/github/release/mbdevpl/open-fortran-parser-xml.svg
    :target: https://github.com/mbdevpl/open-fortran-parser-xml/releases
    :alt: latest GitHub release

This is an extension of Open Fortran Parser (OFP), which outputs abstract syntaxt tree (AST)
of parsed Fortran file in XML format - to a file or to :java:`System.out`.


dependencies
------------

*   Java 1.7 or later

*   Open Fortran Parser 0.8.5-1

    https://github.com/mbdevpl/open-fortran-parser/releases

    This is a patched version of OFP. The list of changes is available at the above link.

*   ANTRL 3.5.2 (dependency of Open Fortran Parser)

    http://www.antlr3.org/download/

*   Apache Commons CLI 1.4 or later

    https://commons.apache.org/proper/commons-cli/download_cli.cgi


how to build
------------

Get dependencies, either manually, or using the provided script:

.. code:: bash

    pip3 install -U -r requirements.txt
    python3 -m open_fortran_parser --deps
    export CLASSPATH="${CLASSPATH}:$(pwd)/lib/*"

Build:

.. code:: bash

    ant
    export CLASSPATH="${CLASSPATH}:$(pwd)/dist/*"

This will create a `.jar` file in `dist` directory, and add it to the Java classpath.

If you use a different python executable to install requirements, please provide it to ant too:

.. code:: bash

    ant -Dpython=/custom/python

Because the build script by default relies on "python3" executable.


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

and remaining command-line options are exactly as defined in OFP 0.8.5.

To parse :bash:`some_fortran_file.f` and save XML output in :bash:`tree.xml` with minimum verbosity:

.. code:: bash

    java fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
      --output tree.xml --verbosity 0 some_fortran_file.f

And to dump XML with maximum verbosity to console:

.. code:: bash

    java fortran.ofp.FrontEnd --class fortran.ofp.XMLPrinter \
      --verbosity 100 some_fortran_file.f


Python wrapper for the generator
================================

.. image:: https://img.shields.io/pypi/v/open-fortran-parser.svg
    :target: https://pypi.python.org/pypi/open-fortran-parser
    :alt: package version from PyPI

Using the wrapper should not require any special knowledge about the generator itself, other than
knowing the abstract syntax tree (AST) specification.


dependencies
------------

Java XML generator for OFP and all of its dependencies.

Python version 3.5 or later.

Python libraries as specified in `<requirements.txt>`_.

Building and running tests additionally requires packages listed in `<test_requirements.txt>`_.


how to build
------------

.. code:: bash

    pip3 install -U -r test_requirements.txt
    python3 setup.py sdist --formats=gztar,zip
    python3 setup.py bdist_wheel

how to install
--------------

You can simply install from PyPI:

.. code:: bash

    pip3 install open-fortran-parser

Or using any of below commands, when installing from source:

.. code:: bash

    pip3 install .
    pip3 install dist/<filename>.whl
    pip3 install dist/<filename>.tar.gz
    pip3 install dist/<filename>.zip


how to run
----------

The wrapper can be used as a script, or as a library.

When running any installed version, even if installed from source, dependencies are automatically
installed together with the wrapper.

Before running from source (without installation), however, please follow "how to build" section
for Java implementation above.
You can make sure that dependencies are configured correctly by running:

.. code:: bash

    python3 -m open_fortran_parser --check-deps

If the depenencies changed since you first ran the wrapper from the source tree, you can cleanup
outdated dependencies by executing:

.. code:: bash

    python3 -m open_fortran_parser --cleanup-deps


as script
~~~~~~~~~

.. code::

    $ python3 -m open_fortran_parser -h
    usage: open_fortran_parser [-h] [--version] [-v VERBOSITY]
                               [--check-dependencies]
                               [input] [output]

    Python wrapper around XML generator for Open Fortran Parser

    positional arguments:
      input                 path to Fortran source code file (default: None)
      output                writable path for where to store resulting XML,
                            defaults to stdout if no path provided (default: None)

    optional arguments:
      -h, --help            show this help message and exit
      --version             show program's version number and exit
      -v VERBOSITY, --verbosity VERBOSITY
                            level of verbosity, from 0 to 100 (default: 100)
      --check-dependencies, --check-deps
                            check if all required dependencies are present and
                            exit (default: False)

    Copyright 2017-2019 by the contributors, Apache License 2.0,
    https://github.com/mbdevpl/open-fortran-parser-xml


as library
~~~~~~~~~~

.. code:: python

    from open_fortran_parser import parse

    xml = parse('my_legacy_code.f', verbosity=0)

More examples available in `<examples.ipynb>`_.


testing
-------

Run basic tests:

.. code:: bash

    python3 -m unittest -v
    TEST_LONG=1 python3 -m unittest -v  # this might take a long time...


code coverage
~~~~~~~~~~~~~

Getting code coverage results for Java requires JaCoCo agent, and JaCoCo CLI,
and both are dowonloaded automatically along with other development dependencies.

Currently, test setup relies on JaCoCo 0.8.3:

*   JaCoCo agent 0.8.3 (runtime)
*   JaCoCo CLI 0.8.3 (nodeps)

Run all test and gather code coverage:

.. code:: bash

    TEST_LONG=1 TEST_COVERAGE=1 python3 -m coverage run --branch --source . -m unittest -v

This will take a long while.

Then, generate results for Python code:

.. code:: bash

    python3 -m coverage report --show-missing
    python3 -m coverage html

Finally, generate results for Java code:

.. code:: bash

    java -jar "lib/org.jacoco.cli-0.8.3-nodeps.jar" report "jacoco.exec" --classfiles "bin/" --sourcefiles "src/" --html jacoco
