"""Tests for parser_wrapper module."""

import logging
import pathlib
import subprocess
import unittest

from open_fortran_parser.parser_wrapper import execute_parser, parse

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = list(_HERE.joinpath('examples').glob('**/*.*'))
OUTPUT_PATHS = ['/tmp/out.xml', None]
VERBOSITIES = (0, 20, 80, 100)


def all_ofp_tests_paths():
    """Find all Fortran files in "tests" subfolder of Open Fortran Parser repository."""
    ofp_relative_repo_path = pathlib.Path('..', 'open-fortran-parser')
    ofp_repo_path = _HERE.parent.joinpath(ofp_relative_repo_path)
    if not ofp_repo_path.exists():
        return []
    all_input_paths = []
    for extension in ('.f', '.f90', '.f03', '.f08', '.h'):
        input_paths = ofp_repo_path.joinpath('tests').glob(
            f'**/*{extension}')
        for input_path in input_paths:
            input_path = input_path.resolve()
            all_input_paths.append(input_path)
    return all_input_paths

ALL_OFP_TEST_PATHS = all_ofp_tests_paths()


class Tests(unittest.TestCase):

    maxDiff = None

    def test_execute_parser(self):
        output_path = OUTPUT_PATHS[0]
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    execute_parser(input_path, output_path, verbosity)

    def test_execute_parser_stdout(self):
        for input_path in INPUT_PATHS:
            for output_path in OUTPUT_PATHS:
                for verbosity in VERBOSITIES:
                    with self.subTest(input_path=input_path,
                                      output_path=output_path, verbosity=verbosity):
                        execute_parser(input_path, output_path, verbosity)

    def test_parse(self):
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    root_node = parse(input_path, verbosity)
                    self.assertIsNotNone(root_node)
                    #root_node = tree.getroot()
                    self.assertEqual(root_node.tag, 'ofp')
                    self.assertEqual(len(root_node), 1)
                    file_node = root_node[0]
                    self.assertEqual(file_node.tag, 'file')
                    self.assertGreaterEqual(len(file_node), 1)

    @unittest.skipIf(not ALL_OFP_TEST_PATHS, 'no Open Fortran Parser test files found')
    def test_ofp_test_files(self):
        failed_test_cases = []
        for input_path in ALL_OFP_TEST_PATHS:
            with self.subTest(input_path=input_path):
                try:
                    root_node = parse(input_path, verbosity=100, raise_on_error=True)
                    self.assertIsNotNone(root_node)
                except subprocess.CalledProcessError:
                    failed_test_cases.append(input_path)
                    continue

        self.assertLessEqual(len(failed_test_cases), 57, msg=failed_test_cases)
        _LOG.warning("failed test cases (%i): %s", len(failed_test_cases), failed_test_cases)
