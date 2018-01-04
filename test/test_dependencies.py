"""Tests for dependencies downloader scripts."""

import os
import pathlib
import tempfile
import unittest

from open_fortran_parser.config import DEV_DEPENDENCIES
from open_fortran_parser.dependencies import ensure_dependencies


class Tests(unittest.TestCase):

    @unittest.skipUnless(os.environ.get('TEST_DEPENDENCIES'), 'skipping dependency test')
    def test_deps(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            ensure_dependencies(DEV_DEPENDENCIES, pathlib.Path(temp_dir), silent=False)
            self.assertGreater(len(os.listdir(temp_dir)), 0)
        with tempfile.TemporaryDirectory() as temp_dir:
            os.rmdir(temp_dir)
            ensure_dependencies(DEV_DEPENDENCIES, pathlib.Path(temp_dir), silent=True)
            self.assertGreater(len(os.listdir(temp_dir)), 0)
