"""Testing ast_transformer module on FFB-MINI application."""

import logging
import pathlib
import unittest

from .test_compatibility import all_fortran_paths

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

_FFBMINI_RELATIVE_REPO_PATH = pathlib.Path('..', 'ffb-mini')
_FFBMINI_SRC_DIR = _HERE.parent.joinpath(_FFBMINI_RELATIVE_REPO_PATH, 'src').resolve()

ALL_FFBMINI_SRC_PATHS = all_fortran_paths(_FFBMINI_SRC_DIR)
VERBOSITIES = (100,)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_ffb_mini(self):
        failure_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'failure')
        success_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'success')

        from .test_compatibility import Tests as CompTests
        CompTests.check_cases_and_report(
            self, 'FFB-MINI', failure_reports_path, success_reports_path, _FFBMINI_SRC_DIR,
            ALL_FFBMINI_SRC_PATHS, 25)

    def test_ffb_mini_with_ofc(self):
        failure_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'failure_ofc')
        success_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'success_ofc')

        from .test_compatibility import Tests as CompTests
        CompTests.check_cases_and_report(
            self, 'FFB-MINI+OFC', failure_reports_path, success_reports_path, _FFBMINI_SRC_DIR,
            ALL_FFBMINI_SRC_PATHS, 35, True)
