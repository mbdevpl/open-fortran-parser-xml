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
    """Run Open Fortran Compiler with given parameters."""
    assert isinstance(input_path, pathlib.Path), type(input_path)
    assert output_path is None or isinstance(output_path, pathlib.Path), type(output_path)
    assert indent is None or isinstance(indent, int), type(indent)
    assert indent is None or indent > 0, indent
    assert form is None or isinstance(form, CodeForm), type(form)

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
    result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    if output_path is not None:
        with open(str(output_path), 'wb') as output_file:
            output_file.write(result.stdout)

    return result


def transpile(
        input_path: pathlib.Path, indent: int = 4, form: t.Optional[CodeForm] = None,
        raise_on_error: bool = False) -> str:
    """Parse given (possibly non-standard) Fortran code and return standard-compliant code."""
    assert isinstance(input_path, pathlib.Path), type(input_path)
    assert indent is None or isinstance(indent, int), type(indent)
    assert indent is None or indent > 0, indent
    assert form is None or isinstance(form, CodeForm), type(form)

    process = execute_compiler(input_path, None, indent, form)
    process_stdout = process.stdout.decode()
    if process.returncode != 0:
        _LOG.warning('%s', process_stdout)
        _LOG.error('Open Fortran Compiler returned %i', process.returncode)
    if process.stderr:
        _LOG.warning(process.stderr.decode())
    if raise_on_error:
        process.check_returncode()

    return process_stdout
