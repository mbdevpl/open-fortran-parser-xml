"""Testing ast_transformer module on FFB-MINI application."""

import logging
import pathlib
import platform
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

    def test_miranda_io(self):
        miranda_io_relative_repo_path = pathlib.Path('..', 'miranda_io')
        miranda_io_src_dir = _HERE.parent.joinpath(miranda_io_relative_repo_path).resolve()
        all_miranda_io_src_paths = all_fortran_paths(miranda_io_src_dir)

        failure_reports_path = _HERE.joinpath('results', 'apps', 'mirnada_io', 'failure')
        success_reports_path = _HERE.joinpath('results', 'apps', 'mirnada_io', 'success')

        from .test_compatibility import Tests as CompTests
        CompTests.check_cases_and_report(
            self, 'miranda_io', failure_reports_path, success_reports_path, miranda_io_src_dir,
            all_miranda_io_src_paths)

    def test_flash(self):
        flash_relative_repo_path = pathlib.Path('..', 'flash-subset', 'FLASH4.4')
        try:
            flash_src_dir = _HERE.parent.joinpath(flash_relative_repo_path, 'source').resolve()
        except FileNotFoundError:
            self.skipTest('FLASH directory not found')  # in Python 3.5
        if not flash_src_dir.is_dir():
            self.skipTest('FLASH directory not found')
        tested_flash_kernel_paths = [
            pathlib.Path(flash_src_dir, pathlib.Path(input_path)) for input_path in [
                'physics/Hydro/HydroMain/simpleUnsplit/HLL/hy_hllUnsplit.F90']]

        failure_reports_path = _HERE.joinpath('results', 'apps', 'flash', 'failure')
        success_reports_path = _HERE.joinpath('results', 'apps', 'flash', 'success')

        from .test_compatibility import Tests as CompTests
        CompTests.check_cases_and_report(
            self, 'FLASH', failure_reports_path, success_reports_path, flash_src_dir,
            tested_flash_kernel_paths)

    def test_ffb_mini(self):
        failure_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'failure')
        success_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'success')

        from .test_compatibility import Tests as CompTests
        CompTests.check_cases_and_report(
            self, 'FFB-MINI', failure_reports_path, success_reports_path, _FFBMINI_SRC_DIR,
            ALL_FFBMINI_SRC_PATHS, 25)

    @unittest.skipIf(platform.system() == 'Windows', 'OFC not available on Windows')
    def test_ffb_mini_with_ofc(self):
        failure_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'failure_ofc')
        success_reports_path = _HERE.joinpath('results', 'apps', 'ffb-mini', 'success_ofc')

        from .test_compatibility import Tests as CompTests
        CompTests.check_cases_and_report(
            self, 'FFB-MINI+OFC', failure_reports_path, success_reports_path, _FFBMINI_SRC_DIR,
            ALL_FFBMINI_SRC_PATHS, 35, True)
