"""Tests for parser_wrapper module."""

import itertools
import logging
import os
import pathlib
import tempfile
import unittest
import xml.etree.ElementTree as ET

from open_fortran_parser.config import JAVA as java_config
from open_fortran_parser.parser_wrapper import execute_parser, parse
from .test_setup import run_program

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATH = _HERE.joinpath('examples', 'empty.f')

INPUT_PATHS = list(pathlib.Path(_HERE, 'examples').glob('**/*.*'))
INPUT_PATHS_LARGE = list(pathlib.Path(_HERE, 'examples_large').glob('**/*.*'))
VERBOSITIES = (0, 20, 80, 100)

SOME_INPUT_PATHS = [_HERE.joinpath('examples', _) for _ in ('comments.f', 'strings.f90')]
SOME_INPUT_PATHS_LARGE = [_HERE.joinpath('examples_large', 'ORCHIDEE_grid.f90')]
SOME_VERBOSITIES = (0, 100)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_config_variants(self):
        executable = java_config['executable']
        options = java_config['options']
        ofp_class = java_config['ofp_class']
        ofp_xml_class = java_config['ofp_xml_class']
        for classpath, verbosity, invalid_options in itertools.product(
                (None, java_config['classpath']), (0, 100, 'invalid'),
                ([], ['--invalid', 'option'], ['--help', 'true'])):
            output_file = tempfile.NamedTemporaryFile(delete=False)
            output_file_name = pathlib.Path(output_file.name)
            output_file.close()
            for output_path in (None, output_file_name):
                with self.subTest(classpath=classpath, verbosity=verbosity,
                                  invalid_options=invalid_options, output_path=output_path):
                    command = [str(executable)]
                    if classpath is not None:
                        command += ['-cp', str(classpath)]
                    if options is not None:
                        command += options
                    command.append(ofp_class)
                    command += ['--class', ofp_xml_class, '--verbosity', str(verbosity)]
                    if output_path is not None:
                        command += ['--output', str(output_path)]
                    command += invalid_options
                    command.append(str(INPUT_PATH))

                    if invalid_options or verbosity == 'invalid' \
                            or classpath is None and 'CLASSPATH' not in os.environ:
                        with self.assertRaises(AssertionError):
                            run_program(*command)
                        continue
                    run_program(*command)
            output_file_name.unlink()

    def test_unset_config(self):
        classpath = java_config['classpath']
        options = java_config['options']

        java_config['classpath'] = None
        with tempfile.NamedTemporaryFile() as output_file:
            execute_parser(INPUT_PATH, pathlib.Path(output_file.name))
        self.assertIsNone(java_config['classpath'])

        java_config['classpath'] = classpath
        java_config['options'] = options

    def test_execute_parser_stdout(self):
        for input_path in SOME_INPUT_PATHS:
            for verbosity in SOME_VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    process = execute_parser(input_path, None, verbosity)
                    self.assertEqual(process.returncode, 0)
                    fortran_ast = ET.fromstring(process.stdout)
                    self._validate_tree(fortran_ast)

    def test_generate_xml(self):
        results_path = pathlib.Path(_HERE, 'results', 'examples')
        results_path.mkdir(exist_ok=True)
        for input_path in INPUT_PATHS:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    output_path = results_path.joinpath(
                        '{}.{}.xml'.format(input_path.name, verbosity))
                    process = execute_parser(input_path, output_path, verbosity)
                    self.assertEqual(process.returncode, 0, process)
                    self.assertTrue(output_path.exists())

    @unittest.skipUnless(os.environ.get('TEST_LONG'), 'skipping long test')
    def test_generate_xml_large(self):
        results_path = pathlib.Path(_HERE, 'results', 'examples')
        results_path.mkdir(exist_ok=True)
        for input_path in INPUT_PATHS_LARGE:
            for verbosity in VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    output_path = results_path.joinpath(
                        '{}.{}.xml'.format(input_path.name, verbosity))
                    process = execute_parser(input_path, output_path, verbosity)
                    self.assertEqual(process.returncode, 0)
                    self.assertTrue(output_path.exists())

    def _validate_tree(self, tree):
        self.assertIsNotNone(tree)
        self.assertIsInstance(tree, ET.Element)
        self.assertEqual(tree.tag, 'ofp')
        self.assertEqual(len(tree), 1)
        file_node = tree[0]
        self.assertEqual(file_node.tag, 'file')
        self.assertGreater(len(file_node), 0)

    def test_parse(self):
        for input_path in SOME_INPUT_PATHS:
            for verbosity in SOME_VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    root_node = parse(input_path, verbosity)
                    self._validate_tree(root_node)

    @unittest.skipUnless(os.environ.get('TEST_LONG'), 'skipping long test')
    def test_parse_large(self):
        for input_path in SOME_INPUT_PATHS_LARGE:
            for verbosity in SOME_VERBOSITIES:
                with self.subTest(input_path=input_path, verbosity=verbosity):
                    root_node = parse(input_path, verbosity)
                    self._validate_tree(root_node)
