"""Default configuration for open_fortran_parser package."""

import pathlib

JAVA = {
    'executable': pathlib.Path('java'),
    'classpath': None,
    'options': None,
    'ofp_class': 'fortran.ofp.FrontEnd',
    'ofp_xml_class': 'fortran.ofp.XMLPrinter'}

OFC = {
    'executable': pathlib.Path('ofc'),
    'path': None}
