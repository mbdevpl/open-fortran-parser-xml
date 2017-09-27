"""Tests for parser_wrapper module."""

import logging
import pathlib
import unittest
import xml.etree.ElementTree as ET

from open_fortran_parser.parser_wrapper import execute_parser, parse

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = list(pathlib.Path(_HERE, 'examples').glob('**/*.*'))
VERBOSITIES = (0, 20, 80, 100)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_execute_parser_stdout(self):
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    process = execute_parser(input_path, None, verbosity)
                    self.assertEqual(process.returncode, 0)
                    fortran_ast = ET.fromstring(process.stdout)
                    self.assertIsInstance(fortran_ast, ET.Element)

    def test_generate_xml(self):
        results_path = pathlib.Path(_HERE, 'results', 'examples')
        results_path.mkdir(exist_ok=True)
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    output_path = pathlib.Path(results_path, input_path.name + '.xml')
                    process = execute_parser(input_path, output_path, verbosity)
                    self.assertEqual(process.returncode, 0)
                    self.assertTrue(output_path.exists())

    def test_parse(self):
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    root_node = parse(input_path, verbosity)
                    self.assertIsNotNone(root_node)
                    self.assertEqual(root_node.tag, 'ofp')
                    self.assertEqual(len(root_node), 1)
                    file_node = root_node[0]
                    self.assertEqual(file_node.tag, 'file')
                    self.assertGreater(len(file_node), 0)
