"""Tests for dependencies downloader scripts."""

import runpy
import sys
import unittest
import urllib.error


class Tests(unittest.TestCase):

    def execute_module(self, module, *args):
        sys.argv = ['{}.py'.format(module.replace('.', '/'))] + list(args)
        runpy.run_module(module, run_name='__main__')

    def test_dev_deps(self):
        self.execute_module('open_fortran_parser.dev_dependencies')

    def test_deps(self):
        with self.assertRaises(urllib.error.HTTPError):
            self.execute_module('open_fortran_parser.dependencies')
