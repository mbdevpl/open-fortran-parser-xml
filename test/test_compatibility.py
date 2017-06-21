"""Tests using the test cases from Open Fortran Parser."""

import logging
import pathlib
import subprocess
import unittest

from open_fortran_parser.parser_wrapper import parse

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent


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

    def test_ofp_test_file(self):
        input_path = "../open-fortran-parser/tests/annex_c/c_5_3_7.f03"
        root_node = parse(input_path, verbosity=100, raise_on_error=True)
        self.assertIsNotNone(root_node)

    @unittest.skipIf(not ALL_OFP_TEST_PATHS, 'no Open Fortran Parser test files found')
    def test_ofp_test_files(self):
        passed_test_cases = []
        failed_test_cases = []
        for input_path in ALL_OFP_TEST_PATHS:
            with self.subTest(input_path=input_path):
                try:
                    root_node = parse(input_path, verbosity=100, raise_on_error=True)
                    self.assertIsNotNone(root_node)
                except subprocess.CalledProcessError:
                    failed_test_cases.append(input_path)
                    continue
                passed_test_cases.append(input_path)

        passed_count = len(passed_test_cases)
        failed_count = len(failed_test_cases)
        self.assertEqual(passed_count + failed_count, len(ALL_OFP_TEST_PATHS))
        _LOG.warning(
            "OFP test case pass rate is %i of %i = %f", passed_count, len(ALL_OFP_TEST_PATHS),
            passed_count / (passed_count + failed_count))
        _LOG.debug("passed OFP test cases (%i): %s", passed_count, passed_test_cases)
        _LOG.warning("failed OFP test cases (%i): %s", failed_count, failed_test_cases)
        self.assertGreaterEqual(passed_count, 366, msg=failed_test_cases)
        self.assertLessEqual(failed_count, 55, msg=failed_test_cases)
