"""Implementation of Python wrapper for OpenFortranParserXML."""

import logging
import pathlib
import subprocess
import typing as t
import xml.etree.ElementTree as ET

from .config import JAVA as java_config

_LOG = logging.getLogger(__name__)


def execute_parser(
        input_path: pathlib.Path, output_path: t.Optional[pathlib.Path],
        verbosity: int = 100, tokenize_instead: bool = False, *args) -> subprocess.CompletedProcess:
    """Execute Open Fortran Parser according to current configuration and function parameters.

    If tokenize_instead is True, given file will not be parsed, but just tokenized instead.
    """

    command = [str(java_config['executable'])]
    if java_config['classpath'] is not None:
        command += ['-cp', str(java_config['classpath'])]
    if java_config['options'] is not None:
        command += java_config['options']
    command.append(java_config['ofp_class'])
    if tokenize_instead:
        command.append('--tokens')
    command += list(args)
    command += ['--class', java_config['ofp_xml_class'], '--verbosity', str(verbosity)]
    if output_path is not None:
        command += ['--output', str(output_path)]
    command.append(str(input_path))

    _LOG.debug('Executing %s...', command)
    return subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def parse(
        input_path: pathlib.Path, verbosity: int = 100, raise_on_error: bool = False) -> ET.Element:
    """Parse given Fortran file and return parse tree as XML."""

    process = execute_parser(input_path, None, verbosity)
    if process.returncode != 0:
        _LOG.warning('%s', process.stdout.decode())
        _LOG.error('Open Fortran Parser returned %i', process.returncode)
    if process.stderr:
        _LOG.warning(process.stderr.decode())
    if raise_on_error:
        process.check_returncode()

    return ET.fromstring(process.stdout)
