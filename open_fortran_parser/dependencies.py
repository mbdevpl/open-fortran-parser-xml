#!/usr/bin/env python3

"""Dependency downloader for open_fortran_parser."""

import pathlib
import urllib

if __name__ == '__main__':
    from dev_dependencies import DEV_DEPENDENCIES, ensure_dependencies
else:
    from .dev_dependencies import DEV_DEPENDENCIES, ensure_dependencies

DEPENENCIES = DEV_DEPENDENCIES.copy()

DEPENENCIES.update({
    'Open Fortran Parser XML 0.1.0': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser-xml/releases/download/v0.1.0/'),
        pathlib.Path('OpenFortranParserXML-0.1.0.jar'))})


if __name__ == '__main__':
    ensure_dependencies(DEPENENCIES)
