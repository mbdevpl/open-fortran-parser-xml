"""Default configuration for open_fortran_parser package."""

import os
import pathlib
import urllib

from ._version import VERSION

DEV_DEPENDENCIES_PATH = pathlib.Path(os.getcwd(), 'lib')

DEV_DEPENDENCIES = {
    'ANTLR 3.3': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser/releases/download/v0.8.4-1/'),
        pathlib.Path('antlr-3.3-complete.jar')),
    'Open Fortran Parser 0.8.4-4': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser/releases/download/v0.8.4-4/'),
        pathlib.Path('OpenFortranParser-0.8.4-4.jar')),
    'Apache Commons CLI 1.4': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser-xml/releases/download/v0.1.0/'),
        pathlib.Path('commons-cli-1.4.jar'))}

DEPENDENCIES_PATH = pathlib.Path(__file__).resolve().parent

DEPENDENCIES = DEV_DEPENDENCIES.copy()

DEPENDENCIES.update({
    'Open Fortran Parser XML {}'.format(VERSION): (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser-xml/releases/download/v{}/'
            .format(VERSION)),
        pathlib.Path('OpenFortranParserXML-{}.jar'.format(VERSION)))})

OUTDATED_DEPENDENCIES = {
    'Open Fortran Parser 0.8.4-1': pathlib.Path('OpenFortranParser-0.8.4-1.jar'),
    'Open Fortran Parser 0.8.4-2': pathlib.Path('OpenFortranParser-0.8.4-2.jar'),
    'Open Fortran Parser 0.8.4-3': pathlib.Path('OpenFortranParser-0.8.4-3.jar')}

JAVA = {
    'executable': pathlib.Path('java'),
    'classpath': pathlib.Path(DEPENDENCIES_PATH, '*'),
    'options': None,
    'ofp_class': 'fortran.ofp.FrontEnd',
    'ofp_xml_class': 'fortran.ofp.XMLPrinter'}

OFC = {
    'executable': pathlib.Path('ofc'),
    'path': None}
