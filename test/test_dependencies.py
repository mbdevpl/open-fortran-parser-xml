"""Tests for dependencies downloader scripts."""

import pathlib
import runpy
import shutil
import sys
import unittest
import urllib.error


class Tests(unittest.TestCase):

    test_root_path = pathlib.Path('/tmp', 'ofp_deps_tmp')

    @classmethod
    def tearDownClass(cls):
        super().tearDownClass()
        shutil.rmtree(str(cls.test_root_path), ignore_errors=True)

    def execute_module(self, module, *args):
        sys.argv = ['{}.py'.format(module.replace('.', '/'))] + list(args)
        runpy.run_module(module, run_name='__main__')

    def test_dev_deps(self):
        # as script
        self.execute_module('open_fortran_parser.dev_dependencies')

        # as module
        from open_fortran_parser.dev_dependencies import DEV_DEPENDENCIES, ensure_dependencies
        for silent in (True, False):
            ensure_dependencies(DEV_DEPENDENCIES, silent=silent)

        # create folder
        shutil.rmtree(str(self.test_root_path), ignore_errors=True)
        ensure_dependencies(DEV_DEPENDENCIES, self.test_root_path)

    def test_deps(self):
        # as script
        with self.assertRaises(urllib.error.HTTPError):
            self.execute_module('open_fortran_parser.dependencies')

        # as module
        from open_fortran_parser.dependencies import DEPENDENCIES, ensure_dependencies
        for silent in (True, False):
            with self.assertRaises(urllib.error.HTTPError):
                ensure_dependencies(DEPENDENCIES, silent=silent)

        # create folder
        shutil.rmtree(str(self.test_root_path), ignore_errors=True)
        with self.assertRaises(urllib.error.HTTPError):
            ensure_dependencies(DEPENDENCIES, self.test_root_path)
