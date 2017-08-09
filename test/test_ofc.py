"""Tests for ofc_wrapper module."""

import logging
import pathlib
import unittest

from open_fortran_parser.ofc_wrapper import CodeForm, execute_compiler, transpile

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = list(_HERE.joinpath('examples').glob('**/*.*'))
OUTPUT_PATHS = ['/tmp/out.f', None]
INDENTS = (0, 4, 8)
FORMS = (CodeForm.Fixed, CodeForm.Free, None)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_execute_compiler(self):
        output_path = OUTPUT_PATHS[0]
        for input_path in INPUT_PATHS:
            for indent in INDENTS:
                for form in FORMS:
                    with self.subTest(input_path=input_path):
                        execute_compiler(input_path, output_path, indent, form)
