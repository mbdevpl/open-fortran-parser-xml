"""Tests for ofc_wrapper module."""

import logging
import pathlib
import unittest

from open_fortran_parser.ofc_wrapper import execute_compiler, transpile

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = list(_HERE.joinpath('examples').glob('**/*.*'))
OUTPUT_PATHS = ['/tmp/out.xml', None]
VERBOSITIES = (0, 20, 80, 100)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_execute_parser(self):
        output_path = OUTPUT_PATHS[0]
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    execute_parser(input_path, output_path, verbosity)
