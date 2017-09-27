"""Tests for dependencies downloader scripts."""

import os
import pathlib
import runpy
import shutil
import sys
import unittest


class Tests(unittest.TestCase):

    test_root_path = pathlib.Path('/tmp', 'ofp_deps_tmp')

    @classmethod
    def tearDownClass(cls):
        super().tearDownClass()
        shutil.rmtree(str(cls.test_root_path), ignore_errors=True)

    def execute_module(self, module, *args):
        sys.argv = ['{}.py'.format(module.replace('.', '/'))] + list(args)
        runpy.run_module(module, run_name='__main__')

    @unittest.skipUnless(os.environ.get('TEST_DEPENDENCIES'), 'skipping dependency test')
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

        shutil.rmtree(str(self.test_root_path), ignore_errors=True)

    @unittest.skipUnless(os.environ.get('TEST_DEPENDENCIES'), 'skipping dependency test')
    def test_deps(self):
        # as script
        self.execute_module('open_fortran_parser.dependencies')

        # as module
        from open_fortran_parser.dependencies import DEPENDENCIES, ensure_dependencies
        for silent in (True, False):
            ensure_dependencies(DEPENDENCIES, silent=silent)
        from open_fortran_parser.dev_dependencies import ROOT_PATH, DEV_DEPENDENCIES
        new_deps = {key: DEPENDENCIES[key] for key in set(DEPENDENCIES) ^ set(DEV_DEPENDENCIES)}
        for _, new_dep in new_deps.items():
            pathlib.Path(ROOT_PATH, new_dep[1]).unlink()

        # create folder
        shutil.rmtree(str(self.test_root_path), ignore_errors=True)
        ensure_dependencies(DEPENDENCIES, self.test_root_path)

        shutil.rmtree(str(self.test_root_path), ignore_errors=True)
