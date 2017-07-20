"""Testing ast_transformer module on FFB-MINI application."""

import logging
import pathlib
import unittest

import typed_astunparse

from open_fortran_parser.parser_wrapper import parse
from open_fortran_parser.ast_transformer import transform

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
        failure_reports_path = _HERE.joinpath('ffbmini_failure')
        success_reports_path = _HERE.joinpath('ffbmini_success')

        from .test_compatibility import Tests as CompTests
        CompTests.check_cases_and_report(
            self, 'FFB-MINI', failure_reports_path, success_reports_path, _FFBMINI_SRC_DIR,
            ALL_FFBMINI_SRC_PATHS, 25)

    @unittest.skip('not ready')
    def test_transform(self):
        transformations_path = _HERE.joinpath('transformations', 'ffbmini')
        transformations_path.mkdir(exist_ok=True)
        for input_path in ALL_FFBMINI_SRC_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    root_node = parse(input_path, verbosity)
                    self.assertIsNotNone(root_node)
                    typed_tree = transform(root_node)
                    code = typed_astunparse.unparse(typed_tree)
                    self.assertGreater(len(code), 0)
                    result_path = transformations_path.joinpath(input_path.stem + '.py')
                    with open(result_path, 'w') as result_file:
                        result_file.write(code)
                    _LOG.debug('```%s```', code)
