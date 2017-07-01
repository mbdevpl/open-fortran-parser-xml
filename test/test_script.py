"""Tests for main script."""

import contextlib
import io
import logging
import os
import pathlib
import runpy
import sys
import tempfile
import unittest

_LOG = logging.getLogger(__name__)


class Tests(unittest.TestCase):

    def run_script(self, *args):
        sys.argv = ['open_fortran_parser'] + list(args)
        runpy.run_module('open_fortran_parser', run_name='__main__')

    def test_help(self):
        f = io.StringIO()
        with contextlib.redirect_stderr(f):
            with self.assertRaises(SystemExit):
                self.run_script()
        _LOG.warning('%s', f.getvalue())

    def test_verbosity_flag(self):
        input_path = pathlib.Path('test', 'examples', 'miranda_io.f90')
        verbosities = (0, 20, 40, 60, 80, 100)
        for verbosity in verbosities:
            f = io.StringIO()
            with contextlib.redirect_stdout(f):
                self.run_script(str(input_path))

    def test_output_flag(self):
        input_path = pathlib.Path('test', 'examples', 'miranda_io.f90')
        output_file = tempfile.NamedTemporaryFile(delete=False)
        output_file.close()
        f = io.StringIO()
        with contextlib.redirect_stdout(f):
            self.run_script(str(input_path))
        self.run_script(str(input_path), output_file.name)
        with open(output_file.name) as output_file:
            self.assertEqual(f.getvalue(), output_file.read())
        os.remove(output_file.name)
