"""Tests for ofc_wrapper module."""

import logging
import pathlib
import unittest

from open_fortran_parser.ofc_wrapper import CodeForm, execute_compiler, transpile

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = [_HERE.joinpath('examples', _) for _ in ['empty.f']]
OUTPUT_PATHS = [None] # [pathlib.Path('/tmp/out.f'), None]
INDENTS = (4,) # (0, 4, 8)
FORMS = (None,) # (CodeForm.Fixed, CodeForm.Free, None)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_execute_compiler(self):
        for input_path in INPUT_PATHS:
            for output_path in OUTPUT_PATHS:
                for indent in INDENTS:
                    for form in FORMS:
                        with self.subTest(input_path=input_path, output_path=output_path,
                                          indent=indent, form=form):
                            execute_compiler(input_path, output_path, indent, form)

    def test_transpile(self):
        for input_path in INPUT_PATHS:
            for indent in INDENTS:
                for form in FORMS:
                    with self.subTest(input_path=input_path, indent=indent, form=form):
                        code = transpile(input_path, indent, form)
                        self.assertIsNotNone(code)
                        self.assertIsInstance(code, str)
                        self.assertGreater(len(code), 0)
