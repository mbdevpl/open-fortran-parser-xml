"""Testing ast_transformer module on FFB-MINI application."""

import logging
import pathlib
import platform
import unittest

from .test_compatibility import all_fortran_paths

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

_APPS_ROOT_PATHS = {
    'miranda_io': pathlib.Path('..', 'miranda_io'),
    'FLASH': pathlib.Path('..', 'flash-subset', 'FLASH4.4'),
    'FFB-MINI': pathlib.Path('..', 'ffb-mini')}

_APPS_OPTIONAL = {'FLASH'}

_APPS_ROOT_PATHS = {
    app: _HERE.parent.joinpath(path).resolve() for app, path in _APPS_ROOT_PATHS.items()
    if app not in _APPS_OPTIONAL or _HERE.parent.joinpath(path).is_dir()}

_APPS_CODE_FILEPATHS = {
    'miranda_io': all_fortran_paths(_APPS_ROOT_PATHS['miranda_io']),
    'FLASH': [pathlib.Path(_APPS_ROOT_PATHS['FLASH'], 'source', pathlib.Path(input_path))
              for input_path in [
                  'physics/Hydro/HydroMain/simpleUnsplit/HLL/hy_hllUnsplit.F90'
                  ]] if 'FLASH' in _APPS_ROOT_PATHS else [],
    'FFB-MINI': all_fortran_paths(_APPS_ROOT_PATHS['FFB-MINI'].joinpath('src'))}


class Tests(unittest.TestCase):

    maxDiff = None

    def _run_app_test(
            self, app_name: str, app_dirname: str = None, minimum_passed_cases: int = None,
            fall_back_to_ofc: bool = False):
        if app_dirname is None:
            app_dirname = app_name

        _suffix = '_ofc' if fall_back_to_ofc else ''

        failure_reports_path = _HERE.joinpath('results', 'apps', app_dirname, 'failure' + _suffix)
        success_reports_path = _HERE.joinpath('results', 'apps', app_dirname, 'success' + _suffix)

        from .test_compatibility import Tests as CompatibilityTests
        CompatibilityTests.check_cases_and_report(
            self, app_name, failure_reports_path, success_reports_path,
            _APPS_ROOT_PATHS[app_name], _APPS_CODE_FILEPATHS[app_name],
            minimum_passed_cases, fall_back_to_ofc)

    def test_miranda_io(self):
        self._run_app_test('miranda_io')

    @unittest.skipIf('FLASH' not in _APPS_ROOT_PATHS, 'FLASH directory not found')
    def test_flash(self):
        self._run_app_test('FLASH', 'flash')

    def test_ffb_mini(self):
        self._run_app_test('FFB-MINI', 'ffb-mini', 25)

    @unittest.skipIf(platform.system() == 'Windows', 'OFC not available on Windows')
    def test_ffb_mini_with_ofc(self):
        self._run_app_test('FFB-MINI', 'ffb-mini', 35, True)
