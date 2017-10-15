"""Tests for parser_wrapper module."""

import itertools
import logging
import pathlib
import tempfile
import unittest
import xml.etree.ElementTree as ET

from open_fortran_parser.config import JAVA as java_config
from open_fortran_parser.parser_wrapper import execute_parser, parse
from .test_setup import run_program

_LOG = logging.getLogger(__name__)

_HERE = pathlib.Path(__file__).resolve().parent

INPUT_PATHS = list(pathlib.Path(_HERE, 'examples').glob('**/*.*'))
VERBOSITIES = (0, 20, 80, 100)


class Tests(unittest.TestCase):

    maxDiff = None

    def test_config_variants(self):
        executable = java_config['executable']
        options = java_config['options']
        ofp_class = java_config['ofp_class']
        ofp_xml_class = java_config['ofp_xml_class']
        input_path = pathlib.Path('test', 'examples', 'empty.f')
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
                    command.append(str(input_path))

                    if invalid_options or verbosity == 'invalid':
                        with self.assertRaises(AssertionError):
                            run_program(*command)
                        continue
                    run_program(*command)
            output_file_name.unlink()

    def test_unset_config(self):
        classpath = java_config['classpath']
        options = java_config['options']

        input_path = pathlib.Path('test', 'examples', 'empty.f')
        java_config['classpath'] = None
        with tempfile.NamedTemporaryFile() as output_file:
            execute_parser(input_path, pathlib.Path(output_file.name))

        java_config['classpath'] = classpath
        java_config['options'] = options

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
