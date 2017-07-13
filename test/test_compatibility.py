"""Tests using the test cases from Open Fortran Parser."""

import logging
import os
import pathlib
import subprocess
import unittest
import xml.etree.ElementTree as ET

from open_fortran_parser.parser_wrapper import parse

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent


def all_fortran_paths(root_path: pathlib.Path):
    """Recursively find all Fortran files in a given directory."""
    if not root_path.exists():
        return []
    all_input_paths = []
    for extension in ('.f', '.f90', '.f03', '.f08', '.h'):
        input_paths = root_path.glob(
            f'**/*{extension}')
        for input_path in input_paths:
            input_path = input_path.resolve()
            all_input_paths.append(input_path)
    return all_input_paths

_OFP_RELATIVE_REPO_PATH = pathlib.Path('..', 'open-fortran-parser')

_OFP_TESTS_DIR = _HERE.parent.joinpath(_OFP_RELATIVE_REPO_PATH, 'tests')

ALL_OFP_TEST_PATHS = all_fortran_paths(_OFP_TESTS_DIR)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_ofp_required_cases(self):
        input_paths = [pathlib.Path(_) for _ in [
            'annex_c/c_5_3_7.f03',
            'rule-tests/R802.f03',
            'bug-reports/bug-1759956.f90',
            'rule-tests/R510.f03']]
        for input_path in input_paths:
            input_path = _OFP_TESTS_DIR.joinpath(input_path).resolve()
            try:
                root_node = parse(input_path, verbosity=100, raise_on_error=True)
                self.assertIsNotNone(root_node)
            except subprocess.CalledProcessError as err:
                _LOG.exception(err.stdout.decode().rstrip())
                self.fail('failed to parse "{}"'.format(input_path))

    def test_ofp_all_cases(self):
        tests_absolute_path = _OFP_TESTS_DIR.resolve()
        failure_reports_path = _HERE.joinpath('compatibility_failure')
        failure_reports_path.mkdir(exist_ok=True)
        failure_reports_path.joinpath('filtered').mkdir(exist_ok=True)
        success_reports_path = _HERE.joinpath('compatibility_success')
        success_reports_path.mkdir(exist_ok=True)
        passed_test_cases = []
        new_passed_cases = []
        failed_test_cases = []
        new_failed_cases = []

        logger_level = logging.getLogger('open_fortran_parser.parser_wrapper').level
        logging.getLogger('open_fortran_parser.parser_wrapper').setLevel(logging.CRITICAL)

        for input_path in ALL_OFP_TEST_PATHS:
            #with self.subTest(input_path=input_path):
            result = None
            try:
                result = parse(input_path, verbosity=100, raise_on_error=True)
                self.assertIsNotNone(result)
            except subprocess.CalledProcessError as err:
                result = err

            relative_input_path = input_path.relative_to(tests_absolute_path)
            report_filename = str(relative_input_path).replace(os.sep, '_') + '.xml'
            failure_report_path = failure_reports_path.joinpath(report_filename)
            filtered_report_path = failure_reports_path.joinpath('filtered', report_filename)
            success_report_path = success_reports_path.joinpath(report_filename)
            old_success_report_path = success_reports_path.joinpath('old_' + report_filename)

            if isinstance(result, ET.Element):
                passed_test_cases.append(input_path)
                if not success_report_path.exists():
                    new_passed_cases.append(input_path)
                if old_success_report_path.exists():
                    old_success_report_path.unlink()
                if failure_report_path.exists():
                    failure_report_path.unlink()
                if filtered_report_path.exists():
                    filtered_report_path.unlink()
                report_path = success_report_path
            elif isinstance(result, subprocess.CalledProcessError):
                failed_test_cases.append(input_path)
                if not failure_report_path.exists() and not filtered_report_path.exists():
                    new_failed_cases.append(input_path)
                if success_report_path.exists():
                    success_report_path.rename(old_success_report_path)
                if b'XMLPrinter' in result.stderr:
                    if filtered_report_path.exists():
                        filtered_report_path.unlink()
                    report_path = failure_report_path
                else:
                    if failure_report_path.exists():
                        failure_report_path.unlink()
                    report_path = filtered_report_path
            else:
                self.fail('{} {}'.format(type(result), result))

            with open(report_path, 'w') as report_file:
                print('<path>{}</path>'.format(input_path), file=report_file)
                if hasattr(result, 'stderr') and result.stderr:
                    print('<stderr>', file=report_file)
                    print(result.stderr.decode().rstrip(), file=report_file)
                    print('</stderr>', file=report_file)
                print('<code>', file=report_file)
                with open(input_path) as fortran_file:
                    print(fortran_file.read(), file=report_file)
                print('</code>', file=report_file)
                if isinstance(result, ET.Element):
                    print(ET.tostring(result).decode(), file=report_file)
                if hasattr(result, 'stdout') and result.stdout:
                    print(result.stdout.decode().rstrip(), file=report_file)

        logging.getLogger('open_fortran_parser.parser_wrapper').setLevel(logger_level)

        failed_count = len(failed_test_cases)
        passed_count = len(passed_test_cases)
        self.assertEqual(passed_count + failed_count, len(ALL_OFP_TEST_PATHS))
        _LOG.warning(
            "OFP test case pass rate is %i of %i = %f", passed_count, len(ALL_OFP_TEST_PATHS),
            passed_count / (passed_count + failed_count))
        _LOG.info("failed OFP test cases (%i): %s", failed_count, failed_test_cases)
        _LOG.debug("passed OFP test cases (%i): %s", passed_count, passed_test_cases)
        if new_failed_cases:
            _LOG.warning(
                "new failed OFP test cases (%i): %s", len(new_failed_cases), new_failed_cases)
        if new_passed_cases:
            _LOG.warning(
                "new passed OFP test cases (%i): %s", len(new_passed_cases), new_passed_cases)
        self.assertLessEqual(failed_count, 48, msg=failed_test_cases)
        self.assertGreaterEqual(passed_count, 373, msg=passed_test_cases)
