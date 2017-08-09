"""Implementation of Python wrapper for Open Fortran Compiler."""

import enum
import logging
import pathlib
import subprocess
import typing as t

from .config import OFC as ofc_config

_LOG = logging.getLogger(__name__)


@enum.unique
class CodeForm(enum.IntEnum):
    Fixed = 1
    Free = 2


def execute_compiler(
        input_path: pathlib.Path, output_path: t.Optional[pathlib.Path],
        indent: int = 4, form: t.Optional[CodeForm] = None) -> subprocess.CompletedProcess:

    if ofc_config['path'] is not None:
        command = [str(ofc_config['path'].joinpath(ofc_config['executable']))]
    else:
        command = [str(ofc_config['executable'])]
    command.append('--sema-tree')
    if indent is not None:
        command += ['--indent-width', str(indent)]
    if form is not None:
        command.append('--{}-form'.format(form.name.lower()))
    command.append(str(input_path))

    _LOG.debug('Executing %s...', command)

    if output_path is not None:
        raise NotImplementedError()


def transpile(input_path: pathlib.Path, output_path: t.Optional[pathlib.Path]):
    pass
