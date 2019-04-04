"""Testing ast_transformer module on FFB-MINI application."""

import logging
import os
import pathlib
import platform
import unittest

from .test_compatibility import all_fortran_paths, TestsBase

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

_ROOT = _HERE.parent

_APPS_ROOT = pathlib.Path(os.environ.get('TEST_APPS_ROOT', _ROOT.parent)).resolve()

_APPS_ROOT_PATHS = {
    'miranda_io': pathlib.Path('..', 'miranda_io'),
    'FLASH-4.5': pathlib.Path('..', 'flash-4.5'),
    'FLASH-SUBSET': pathlib.Path('..', 'flash-subset', 'FLASH4.4'),
    'FFB-MINI': pathlib.Path('..', 'ffb-mini'),
    'flash5': _APPS_ROOT.joinpath('flash5')}

_APPS_OPTIONAL = {'FLASH-4.5', 'FLASH-SUBSET'}

_APPS_ROOT_PATHS = {
    app: _HERE.parent.joinpath(path).resolve() for app, path in _APPS_ROOT_PATHS.items()
    if app not in _APPS_OPTIONAL or _HERE.parent.joinpath(path).is_dir()}

_FLASH_COMMON_PATHS = [
    'physics/Hydro/HydroMain/split/MHD_8Wave/hy_8wv_interpolate.F90',
    'physics/Hydro/HydroMain/split/MHD_8Wave/hy_8wv_fluxes.F90',
    'physics/Eos/EosMain/Gamma/eos_idealGamma.F90',
    'physics/Hydro/HydroMain/split/MHD_8Wave/hy_8wv_sweep.F90']

_APPS_CODE_FILEPATHS = {
    'miranda_io': all_fortran_paths(_APPS_ROOT_PATHS['miranda_io']),
    'FLASH-4.5': [
        pathlib.Path(_APPS_ROOT_PATHS['FLASH-4.5'], 'source', pathlib.Path(input_path))
        for input_path in [
            'physics/Hydro/HydroMain/unsplit/hy_uhd_getFaceFlux.F90',
            'physics/Hydro/HydroMain/unsplit/hy_uhd_DataReconstructNormalDir_MH.F90',
            'physics/Hydro/HydroMain/unsplit/hy_uhd_upwindTransverseFlux.F90',
            'physics/Hydro/HydroMain/unsplit/hy_uhd_TVDslope.F90',
            'physics/Hydro/HydroMain/unsplit/hy_uhd_Roe.F90'
            ] + _FLASH_COMMON_PATHS] if 'FLASH-4.5' in _APPS_ROOT_PATHS else [],
    'FLASH-SUBSET': [
        pathlib.Path(_APPS_ROOT_PATHS['FLASH-SUBSET'], 'source', pathlib.Path(input_path))
        for input_path in [
            'physics/Hydro/HydroMain/simpleUnsplit/HLL/hy_hllUnsplit.F90',
            'physics/Hydro/HydroMain/unsplit/hy_getFaceFlux.F90',
            'physics/Hydro/HydroMain/unsplit/hy_DataReconstructNormalDir_MH.F90',
            'physics/Hydro/HydroMain/unsplit/hy_upwindTransverseFlux.F90',
            'physics/Hydro/HydroMain/unsplit/hy_TVDslope.F90',
            'physics/Hydro/HydroMain/unsplit/hy_Roe.F90'
            ] + _FLASH_COMMON_PATHS] if 'FLASH-SUBSET' in _APPS_ROOT_PATHS else [],
    'FFB-MINI': [path for path in all_fortran_paths(_APPS_ROOT_PATHS['FFB-MINI'].joinpath('src'))
                 if path.name not in ('gfc.h', 'gfrd_c.h', 'gfutil_c.h', 'gfutil_f.h', 'gfwrt_c.h',
                                      'maprof.h', 'maprof_proc.h', 'maprof_yaml.h')],
    'flash5': [_APPS_ROOT_PATHS['flash5'].joinpath('source', _) for _ in {
        pathlib.Path('physics', 'Hydro', 'HydroMain', 'unsplit', 'hy_getFaceFlux.F90'),
        pathlib.Path('physics', 'Hydro', 'HydroMain', 'unsplit', 'hy_getRiemannState.F90'),
        pathlib.Path('physics', 'Hydro', 'HydroMain', 'unsplit', 'hy_TVDslope.F90'),
        pathlib.Path('physics', 'Hydro', 'HydroMain', 'unsplit', 'hy_upwindTransverseFlux.F90'),
        pathlib.Path('physics', 'Hydro', 'HydroMain', 'unsplit', 'MHD', 'hy_eigenVector.F90'),
        pathlib.Path('physics', 'Eos', 'EosMain', 'Helmholtz_starkiller', 'SpeciesBased',
                     'actual_eos.F90'),
        pathlib.Path('physics', 'sourceTerms', 'Burn', 'BurnMain', 'nuclearBurn', 'Aprox13',
                     'bn_mapNetworkToSpecies.F90'),
        pathlib.Path('physics', 'sourceTerms', 'Burn', 'BurnMain', 'nuclearBurn', 'bn_burner.F90'),
        pathlib.Path('physics', 'sourceTerms', 'Burn', 'BurnMain', 'nuclearBurn', 'Burn.F90'),
        pathlib.Path('Simulation', 'Simulation_init.F90')}]}


class Tests(TestsBase):

    maxDiff = None

    def _run_app_test(
            self, app_name: str, app_dirname: str = None, minimum_passed_cases: int = None,
            fall_back_to_ofc: bool = False):
        if app_name not in _APPS_ROOT_PATHS and app_name in _APPS_OPTIONAL:
            self.skipTest('{} directory not found'.format(app_name))
        if app_dirname is None:
            app_dirname = app_name.lower()

        _suffix = '_ofc' if fall_back_to_ofc else ''

        failure_reports_path = _HERE.joinpath('results', 'apps', app_dirname, 'failure' + _suffix)
        success_reports_path = _HERE.joinpath('results', 'apps', app_dirname, 'success' + _suffix)

        self.check_cases_and_report(
            app_name, failure_reports_path, success_reports_path,
            _APPS_ROOT_PATHS[app_name], _APPS_CODE_FILEPATHS[app_name],
            minimum_passed_cases, fall_back_to_ofc)

    def test_miranda_io(self):
        self._run_app_test('miranda_io')

    @unittest.skipUnless(os.environ.get('TEST_LONG'), 'skipping long test')
    def test_flash_45(self):
        self._run_app_test('FLASH-4.5')

    @unittest.skipUnless(os.environ.get('TEST_LONG'), 'skipping long test')
    def test_flash_subset(self):
        self._run_app_test('FLASH-SUBSET')

    @unittest.skipUnless(os.environ.get('TEST_LONG'), 'skipping long test')
    def test_ffb_mini(self):
        self._run_app_test('FFB-MINI', None, 24)

    @unittest.skipUnless(os.environ.get('TEST_LONG'), 'skipping long test')
    @unittest.skipIf(platform.system() == 'Windows', 'OFC not available on Windows')
    def test_ffb_mini_with_ofc(self):
        self._run_app_test('FFB-MINI', None, 35, True)

    def test_flash5(self):
        self._run_app_test('flash5', None, 9)
