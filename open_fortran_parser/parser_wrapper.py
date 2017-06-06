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
        verbosity: int = 100) -> subprocess.CompletedProcess:
    """Execute Open Fortran Parser according to current configuration and function parameters."""

    command = [str(java_config['executable'])]
    if java_config['classpath'] is not None:
        command += ['-cp', str(java_config['classpath'])]
    command.append(java_config['ofp_class'])
    command += ['--class', java_config['ofp_xml_class'], '--verbosity', str(verbosity)]
    if output_path is not None:
        command += ['--output', str(output_path)]
    command.append(str(input_path))

    _LOG.debug('Executing %s...', command)
    return subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

def parse(input_path: pathlib.Path, verbosity: int = 100) -> ET.Element:
    """Parse given Fortran file and return parse tree as XML."""

    process = execute_parser(input_path, None, verbosity)
    if process.stderr:
        _LOG.info(process.stderr.decode())
    process.check_returncode()

    return ET.fromstring(process.stdout)
