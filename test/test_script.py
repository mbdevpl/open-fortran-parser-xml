"""Tests for main script."""

import contextlib
import io
import os
import pathlib
import runpy
import sys
import tempfile
import unittest

INPUT_PATH = pathlib.Path('test', 'examples', 'empty.f')


def normalize_newlines(text: str) -> str:
    return text.replace('\r\n', '\n').replace('\r', '\n')


class Tests(unittest.TestCase):

    maxDiff = None

    def run_script(self, *args):
        sys.argv = ['open_fortran_parser'] + list(args)
        runpy.run_module('open_fortran_parser', run_name='__main__')

    def test_help(self):
        f = io.StringIO()
        with contextlib.redirect_stderr(f):
            with self.assertRaises(SystemExit):
                self.run_script()
        text = f.getvalue()
        self.assertIn('usage', text)
        self.assertIn('open_fortran_parser', text)

    def test_verbosity_flag(self):
        verbosities = (0, 20, 40, 60, 80, 100)
        for verbosity in verbosities:
            f = io.StringIO()
            with contextlib.redirect_stdout(f):
                self.run_script('-v', str(verbosity), str(INPUT_PATH))
            self.assertGreater(len(f.getvalue()), 0)

    def test_output_flag(self):
        output_file = tempfile.NamedTemporaryFile(delete=False)
        output_file.close()
        f = io.StringIO()
        with contextlib.redirect_stdout(f):
            self.run_script(str(INPUT_PATH))
        self.run_script(str(INPUT_PATH), output_file.name)
        with open(output_file.name) as output_file:
            self.assertEqual(normalize_newlines(f.getvalue()),
                             normalize_newlines(output_file.read()))
        os.remove(output_file.name)
