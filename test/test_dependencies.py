"""Tests for dependencies downloader scripts."""

import os
import pathlib
import tempfile
import unittest

from .test_setup import run_module


class Tests(unittest.TestCase):

    def test_deps_script(self):
        run_module('open_fortran_parser.dependencies')
        from open_fortran_parser.dependencies import DEPENDENCIES_PATH
        self.assertGreater(len(os.listdir(str(DEPENDENCIES_PATH))), 0)

    @unittest.skipUnless(os.environ.get('TEST_DEPENDENCIES'), 'skipping dependency test')
    def test_deps(self):
        from open_fortran_parser.dependencies import DEV_DEPENDENCIES, ensure_dependencies
        for silent in (False, True):
            with tempfile.TemporaryDirectory() as temp_dir:
                ensure_dependencies(DEV_DEPENDENCIES, pathlib.Path(temp_dir), silent=silent)
                self.assertGreater(len(os.listdir(temp_dir)), 0)

        return
