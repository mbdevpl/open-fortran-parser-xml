"""Tests for open_fortran_parser package."""

import os
import pathlib

from open_fortran_parser.config import JAVA as java_config


if java_config['options'] is None:
    java_config['options'] = []
java_config['options'].append('-Djava.util.logging.config.file=logging.properties')

os.makedirs(str(pathlib.Path('test', 'results', 'logs')), exist_ok=True)

if os.environ.get('TEST_COVERAGE'):
    JACOCO_PATH = pathlib.Path('lib', 'org.jacoco.agent-0.8.1-runtime.jar').resolve()
    JACOCO_EXCLUDES = ('fortran.ofp.parser.java.FortranParserExtras_FortranParser08',)
    if JACOCO_PATH.is_file():
        java_config['options'].append(
            '-javaagent:{}=excludes={}'.format(str(JACOCO_PATH), ':'.join(JACOCO_EXCLUDES)))
