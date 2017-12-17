"""Tests for open_fortran_parser package."""

import os
import pathlib

from open_fortran_parser.config import JAVA as java_config


if os.environ.get('TEST_COVERAGE'):
    JACOCO_PATH = pathlib.Path('lib', 'org.jacoco.agent-0.7.9-runtime.jar').resolve()
    JACOCO_EXCLUDES = ('fortran.ofp.parser.java.FortranParserExtras_FortranParser08',)
    if JACOCO_PATH.is_file():
        if java_config['options'] is None:
            java_config['options'] = []
        java_config['options'].append(
            '-javaagent:{}=excludes={}'.format(str(JACOCO_PATH), ':'.join(JACOCO_EXCLUDES)))
