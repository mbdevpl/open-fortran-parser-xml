"""Tests for main script."""

import contextlib
import io
import os
import pathlib
import tempfile
import unittest

from open_fortran_parser.config import DEV_DEPENDENCIES_PATH, DEPENDENCIES_PATH
from .test_setup import run_module

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATH = _HERE.joinpath('examples', 'empty.f')


def normalize_newlines(text: str) -> str:
    return text.replace('\r\n', '\n').replace('\r', '\n')


class Tests(unittest.TestCase):

    maxDiff = None

    def test_run_not_main(self):
        sio = io.StringIO()
        with contextlib.redirect_stderr(sio):
            run_module('open_fortran_parser', 'some', 'bad', 'args', run_name='not_main')
        self.assertEqual(len(sio.getvalue()), 0)

    def test_help(self):
        sio = io.StringIO()
        with contextlib.redirect_stderr(sio):
            with self.assertRaises(SystemExit):
                run_module('open_fortran_parser')
        text = sio.getvalue()
        self.assertIn('usage', text)
        self.assertIn('open_fortran_parser', text)

    @unittest.skipUnless(os.environ.get('TEST_DEPENDENCIES'), 'skipping dependency test')
    def test_deps_flag(self):
        sio = io.StringIO()
        with contextlib.redirect_stderr(sio):
            run_module('open_fortran_parser', '--deps')
        self.assertGreater(len(sio.getvalue()), 0)
        self.assertGreater(len(os.listdir(str(DEPENDENCIES_PATH))), 0)

        run_module('open_fortran_parser', '--dev-deps')
        self.assertGreater(len(os.listdir(str(DEV_DEPENDENCIES_PATH))), 0)

    def test_verbosity_flag(self):
        verbosities = (0, 20, 40, 60, 80, 100)
        for verbosity in verbosities:
            sio = io.StringIO()
            with contextlib.redirect_stdout(sio):
                run_module('open_fortran_parser', '-v', str(verbosity), str(INPUT_PATH))
            self.assertGreater(len(sio.getvalue()), 0)

    def test_output_flag(self):
        output_file = tempfile.NamedTemporaryFile(delete=False)
        output_file.close()
        sio = io.StringIO()
        with contextlib.redirect_stdout(sio):
            run_module('open_fortran_parser', str(INPUT_PATH))
        run_module('open_fortran_parser', str(INPUT_PATH), output_file.name)
        with open(output_file.name) as output_file:
            self.assertEqual(normalize_newlines(sio.getvalue()),
                             normalize_newlines(output_file.read()))
        os.remove(output_file.name)
