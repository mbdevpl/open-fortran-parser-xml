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
        input_paths = [pathlib.Path(_) for _ in [
            '../open-fortran-parser/tests/annex_c/c_5_3_7.f03',
            '../open-fortran-parser/tests/rule-tests/R802.f03',
            '../open-fortran-parser/tests/bug-reports/bug-1759956.f90']]
        for input_path in input_paths:
            try:
                root_node = parse(input_path, verbosity=100, raise_on_error=True)
                self.assertIsNotNone(root_node)
            except subprocess.CalledProcessError as err:
                _LOG.exception(err.stdout.decode().rstrip())
                self.fail('failed to parse "{}"'.format(input_path))

    def test_ofp_test_files(self):
        reports_path = _HERE.joinpath('compatibility_reports')
        reports_path.mkdir(exist_ok=True)
        passed_test_cases = []
        failed_test_cases = []
        for input_path in ALL_OFP_TEST_PATHS:
            #with self.subTest(input_path=input_path):
            report_path = reports_path.joinpath(input_path.name + '.xml')
            try:
                root_node = parse(input_path, verbosity=100, raise_on_error=True)
                self.assertIsNotNone(root_node)
            except subprocess.CalledProcessError as err:
                if err.stdout and b'XMLPrinter' in err.stderr:
                    with open(report_path, 'w') as report_file:
                        print('<stderr>', file=report_file)
                        print(err.stderr.decode().rstrip(), file=report_file)
                        print('</stderr>', file=report_file)
                        print('<code>', file=report_file)
                        with open(input_path) as fortran_file:
                            print(fortran_file.read(), file=report_file)
                        print('</code>', file=report_file)
                        print(err.stdout.decode().rstrip(), file=report_file)
                elif report_path.exists():
                    report_path.unlink()
                failed_test_cases.append(input_path)
                continue
            if report_path.exists():
                report_path.unlink()
            passed_test_cases.append(input_path)

        failed_count = len(failed_test_cases)
        passed_count = len(passed_test_cases)
        self.assertEqual(passed_count + failed_count, len(ALL_OFP_TEST_PATHS))
        _LOG.warning(
            "OFP test case pass rate is %i of %i = %f", passed_count, len(ALL_OFP_TEST_PATHS),
            passed_count / (passed_count + failed_count))
        _LOG.warning("failed OFP test cases (%i): %s", failed_count, failed_test_cases)
        _LOG.debug("passed OFP test cases (%i): %s", passed_count, passed_test_cases)
        self.assertLessEqual(failed_count, 55, msg=failed_test_cases)
        self.assertGreaterEqual(passed_count, 366, msg=passed_test_cases)
