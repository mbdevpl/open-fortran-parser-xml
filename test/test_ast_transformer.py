"""Tests for ast_transformer module."""

import pathlib
import unittest

import typed_astunparse

from open_fortran_parser.parser_wrapper import parse
from open_fortran_parser.ast_transformer import transform

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = list(_HERE.joinpath('examples').glob('**/*.*'))
VERBOSITIES = (100,)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_transform(self):
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    root_node = parse(input_path, verbosity)
                    self.assertIsNotNone(root_node)
                    typed_tree = transform(root_node)
                    code = typed_astunparse.unparse(typed_tree)
                    print('```', code, '```')
