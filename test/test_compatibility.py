"""Tests using the test cases from Open Fortran Parser."""

import itertools
import logging
import os
import pathlib
import subprocess
import typing as t
import unittest
import xml.etree.ElementTree as ET

from open_fortran_parser.parser_wrapper import parse
from open_fortran_parser.ofc_wrapper import transpile

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent


def all_fortran_paths(root_path: pathlib.Path):
    """Recursively find all Fortran files in a given directory."""
    if not root_path.exists():
        return []
    all_input_paths = []
    for extension in itertools.chain(*[(_, _.upper()) for _ in ('.f', '.f90', '.f03', '.f08', '.h')]):
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

    def check_cases(self, input_paths):
        for input_path in input_paths:
            input_path = _OFP_TESTS_DIR.joinpath(input_path).resolve()
            try:
                root_node = parse(input_path, verbosity=100, raise_on_error=True)
                self.assertIsNotNone(root_node)
            except subprocess.CalledProcessError as err:
                _LOG.exception(err.stdout.decode().rstrip())
                self.fail('failed to parse "{}"'.format(input_path))

    def check_cases_and_report(
            self, scenario_name: str, failure_reports_path: pathlib.Path,
            success_reports_path: pathlib.Path, input_paths_root: pathlib.Path,
            input_paths: t.Sequence[pathlib.Path], minimum_passed_cases: int = None,
            fall_back_to_ofc: bool = False):
        all_count = len(input_paths)
        if minimum_passed_cases is None:
            minimum_passed_cases = all_count
        else:
            self.assertGreaterEqual(all_count, minimum_passed_cases, 'not enough cases to pass')

        failure_reports_path.mkdir(parents=True, exist_ok=True)
        failure_reports_path.joinpath('filtered').mkdir(parents=True, exist_ok=True)
        success_reports_path.mkdir(parents=True, exist_ok=True)
        passed_test_cases = []
        new_passed_cases = []
        failed_test_cases = []
        new_failed_cases = []

        for input_path in input_paths:
            #with self.subTest(input_path=input_path):

            relative_input_path = input_path.relative_to(input_paths_root)
            flat_relative_input_path = str(relative_input_path).replace(os.sep, '_')

            logger_level = logging.getLogger('open_fortran_parser.parser_wrapper').level
            logging.getLogger('open_fortran_parser.parser_wrapper').setLevel(logging.CRITICAL)
            ofc_logger_level = logging.getLogger('open_fortran_parser.ofc_wrapper').level
            logging.getLogger('open_fortran_parser.ofc_wrapper').setLevel(logging.CRITICAL)
            result = None
            try:
                try:
                    result = parse(input_path, verbosity=100, raise_on_error=True)
                    self.assertIsNotNone(result)
                except subprocess.CalledProcessError as parser_err:
                    if not fall_back_to_ofc:
                        raise parser_err
                    code = None
                    try:
                        code = transpile(input_path, raise_on_error=True)
                        self.assertIsInstance(code, str)
                        transpiled_path = pathlib.Path('/tmp', flat_relative_input_path)
                        with open(str(transpiled_path), 'w') as transpiled_file:
                            transpiled_file.write(code)
                        result = parse(transpiled_path, verbosity=100, raise_on_error=True)
                        self.assertIsNotNone(result)
                        _LOG.warning('OFC definitely fixed something, see %s', transpiled_path)
                    except subprocess.CalledProcessError as err3:
                        if code is not None:
                            _LOG.warning('OFC succeeded but parser failed %s', transpiled_path)
                        raise parser_err from err3
            except subprocess.CalledProcessError as err:
                result = err
            logging.getLogger('open_fortran_parser.parser_wrapper').setLevel(logger_level)
            logging.getLogger('open_fortran_parser.ofc_wrapper').setLevel(ofc_logger_level)

            report_filename = flat_relative_input_path + '.xml'
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

        failed_count = len(failed_test_cases)
        passed_count = len(passed_test_cases)
        self.assertEqual(passed_count + failed_count, all_count)
        _LOG.warning(
            '%s test case pass rate is %i of %i = %f', scenario_name, passed_count,
            all_count, passed_count / (passed_count + failed_count))
        _LOG.info('failed %s test cases (%i): %s', scenario_name, failed_count, failed_test_cases)
        _LOG.debug('passed %s test cases (%i): %s', scenario_name, passed_count, passed_test_cases)
        if new_failed_cases:
            _LOG.warning(
                'new failed %s test cases (%i): %s', scenario_name, len(new_failed_cases),
                new_failed_cases)
        if new_passed_cases:
            _LOG.warning(
                'new passed %s test cases (%i): %s', scenario_name, len(new_passed_cases),
                new_passed_cases)
        self.assertLessEqual(failed_count, all_count - minimum_passed_cases, msg=failed_test_cases)
        self.assertGreaterEqual(passed_count, minimum_passed_cases, msg=passed_test_cases)

        return passed_test_cases, new_passed_cases, failed_test_cases, new_failed_cases

    def test_ofp_required_cases(self):
        input_paths = [pathlib.Path(_) for _ in [
            'annex_c/c_5_3_7.f03',
            'rule-tests/R802.f03',
            'bug-reports/bug-1759956.f90',
            'rule-tests/R510.f03']]
        self.check_cases(input_paths)

    def test_ofp_module_contains(self):
        input_paths = [pathlib.Path(_) for _ in [
            'rule-f08-tests/R1101.f90',
            'rule-f08-tests/R1104.f90',
            'f08-tests/R1237-F08.f08',
            'rule-tests/R455.f90',
            'rule-tests/R1104.f90',
            'bug-reports/bug-3053141.f90']]
        self.check_cases(input_paths)

    def test_ofp_select_case(self):
        input_paths = [pathlib.Path(_) for _ in [
            'rule-tests/R808.f03',
            'rule-tests/R814.f03']]
        self.check_cases(input_paths)

    def test_ofp_all_cases(self):
        tests_absolute_path = _OFP_TESTS_DIR.resolve()
        failure_reports_path = _HERE.joinpath('compatibility_failure')
        success_reports_path = _HERE.joinpath('compatibility_success')

        self.check_cases_and_report(
            'OFP', failure_reports_path, success_reports_path, tests_absolute_path,
            ALL_OFP_TEST_PATHS, 385)
