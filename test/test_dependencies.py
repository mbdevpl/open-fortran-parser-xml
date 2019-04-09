"""Tests for dependencies downloader scripts."""

import os
import pathlib
import tempfile
import unittest

from open_fortran_parser.config import DEV_DEPENDENCIES
from open_fortran_parser.dependencies import ensure_dependencies, cleanup_old_dependencies

EXAMPLE_DEPENDENCY = 'Apache Commons CLI 1.4'

TESTED_DEPENDENCIES = {EXAMPLE_DEPENDENCY: DEV_DEPENDENCIES[EXAMPLE_DEPENDENCY]}


class Tests(unittest.TestCase):

    @unittest.skipUnless(os.environ.get('TEST_DEPENDENCIES'), 'skipping dependency test')
    def test_deps(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            self.assertEqual(len(os.listdir(temp_dir)), 0)
            ensure_dependencies(TESTED_DEPENDENCIES, pathlib.Path(temp_dir), download=False)
            self.assertEqual(len(os.listdir(temp_dir)), 0)
            ensure_dependencies(TESTED_DEPENDENCIES, pathlib.Path(temp_dir), silent=False)
            self.assertGreater(len(os.listdir(temp_dir)), 0)
            ensure_dependencies(TESTED_DEPENDENCIES, pathlib.Path(temp_dir), silent=False)
        with tempfile.TemporaryDirectory() as temp_dir:
            os.rmdir(temp_dir)
            ensure_dependencies(TESTED_DEPENDENCIES, pathlib.Path(temp_dir), silent=True)
            self.assertGreater(len(os.listdir(temp_dir)), 0)

    @unittest.skipUnless(os.environ.get('TEST_DEPENDENCIES'), 'skipping dependency test')
    def test_cleanup_deps(self):
        to_clean = {k: v[1] for k, v in TESTED_DEPENDENCIES.items()}
        with tempfile.TemporaryDirectory() as temp_dir:
            ensure_dependencies(TESTED_DEPENDENCIES, pathlib.Path(temp_dir), silent=True)
            self.assertGreater(len(os.listdir(temp_dir)), 0)
            cleanup_old_dependencies(to_clean, pathlib.Path(temp_dir))
            self.assertEqual(len(os.listdir(temp_dir)), 0)
            cleanup_old_dependencies(to_clean, pathlib.Path(temp_dir))
        with tempfile.TemporaryDirectory() as temp_dir:
            ensure_dependencies(TESTED_DEPENDENCIES, pathlib.Path(temp_dir), silent=True)
            count = len(os.listdir(temp_dir))
            self.assertGreater(count, 0)
            with tempfile.TemporaryDirectory() as temp_backup_dir:
                os.rmdir(temp_backup_dir)
                cleanup_old_dependencies(to_clean, pathlib.Path(temp_dir),
                                         pathlib.Path(temp_backup_dir))
                self.assertEqual(len(os.listdir(temp_backup_dir)), count)
            self.assertEqual(len(os.listdir(temp_dir)), 0)
