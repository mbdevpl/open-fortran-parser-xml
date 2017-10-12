"""Tests for ofc_wrapper module."""

import logging
import pathlib
import platform
import tempfile
import unittest

from open_fortran_parser.ofc_wrapper import CodeForm, execute_compiler, transpile

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = [_HERE.joinpath('examples', _) for _ in ['empty.f']]
INDENTS = (None, 2, 4, 8)
FORMS = (None, CodeForm.Fixed, CodeForm.Free)


class Tests(unittest.TestCase):

    maxDiff = None

    @unittest.skipIf(platform.system() == 'Windows', 'OFC not available on Windows')
    def test_execute_compiler(self):
        for input_path in INPUT_PATHS:
            for indent in INDENTS:
                for form in FORMS:
                    output_file = tempfile.NamedTemporaryFile(delete=False)
                    output_file_path = pathlib.Path(output_file.name)
                    for output_path in (None, output_file_path):
                        with self.subTest(input_path=input_path, output_path=output_path,
                                          indent=indent, form=form):
                            result = execute_compiler(input_path, output_path, indent, form)
                            self.assertEqual(result.returncode, 0, msg=result)

    @unittest.skipIf(platform.system() == 'Windows', 'OFC not available on Windows')
    def test_transpile(self):
        for input_path in INPUT_PATHS:
            for indent in INDENTS:
                for form in FORMS:
                    with self.subTest(input_path=input_path, indent=indent, form=form):
                        code = transpile(input_path, indent, form, raise_on_error=True)
                        self.assertIsNotNone(code)
                        self.assertIsInstance(code, str)
                        self.assertGreater(len(code), 0)
