"""Default configuration for open_fortran_parser package."""

import pathlib

from .dependencies import DEPENDENCIES_PATH

JAVA = {
    'executable': pathlib.Path('java'),
    'classpath': pathlib.Path(DEPENDENCIES_PATH, '*'),
    'options': None,
    'ofp_class': 'fortran.ofp.FrontEnd',
    'ofp_xml_class': 'fortran.ofp.XMLPrinter'}

OFC = {
    'executable': pathlib.Path('ofc'),
    'path': None}
