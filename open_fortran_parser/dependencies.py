#!/usr/bin/env python3

"""Dependency downloader for open_fortran_parser."""

import logging
import os
import pathlib
import platform
import typing as t
import urllib

import wget

try:
    from ._version import VERSION
except SystemError:
    from _version import VERSION

_LOG = logging.getLogger(__name__)

DEV_DEPENDENCIES = {
    'ANTLR 3.3': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser/releases/download/v0.8.4-1/'),
        pathlib.Path('antlr-3.3-complete.jar')),
    'Open Fortran Parser 0.8.4-2': (
        urllib.parse.urlparse(
            # 'https://github.com/mbdevpl/open-fortran-parser/releases/download/v0.8.4-2/'),
            'https://dl.bintray.com/mbdevpl/pkgs/open-fortran-parser-v0.8.5.dev4+8f933cfc/linux-openjdk7/'),
        pathlib.Path('OpenFortranParser-0.8.4-3.jar')),
    'Apache Commons CLI 1.4': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser-xml/releases/download/v0.1.0/'),
        pathlib.Path('commons-cli-1.4.jar'))}

DEV_DEPENDENCIES_PATH = pathlib.Path(os.getcwd(), 'lib')

DEPENDENCIES = DEV_DEPENDENCIES.copy()

DEPENDENCIES.update({
    'Open Fortran Parser XML {}'.format(VERSION): (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser-xml/releases/download/v{}/'
            .format(VERSION)),
        pathlib.Path('OpenFortranParserXML-{}.jar'.format(VERSION)))})

DEPENDENCIES_PATH = pathlib.Path(__file__).resolve().parent


def ensure_dependencies(
        dependencies: t.Mapping[str, t.Tuple[urllib.parse.ParseResult, pathlib.Path]],
        target_dir: pathlib.Path, silent: bool = False) -> None:
    """Download missing depenedencies."""
    if not target_dir.exists():
        _LOG.warning('Creating directory "%s"...', target_dir)
        os.makedirs(str(target_dir), exist_ok=True)
    for dependency, (url_root, filename) in dependencies.items():
        path = target_dir.joinpath(filename)
        if path.is_file():
            _LOG.warning('%s is present already.', dependency)
            continue
        url = urllib.parse.urlunparse(url_root) + str(filename)
        _LOG.warning('Downloading %s from URL "%s" to path "%s"...', dependency, url, path)
        wget.download(url, str(path), bar=None if silent else wget.bar_adaptive)
        if not silent:
            print()
    if not silent:
        classpath = target_dir.joinpath('*')
        _LOG.warning('If you wish to use the Open Fortran Parser XML generator directly,'
                     ' please add "%s" to your Java classpath:', classpath)
        if platform.system() != 'Windows':
            _LOG.warning('export CLASSPATH="${CLASSPATH}:%s"', classpath)


OUTDATED_DEPENDENCIES = {
    'Open Fortran Parser 0.8.4-1': pathlib.Path('OpenFortranParser-0.8.4-1.jar'),
    'Open Fortran Parser 0.8.4-2': pathlib.Path('OpenFortranParser-0.8.4-2.jar')}


def cleanup_old_dependencies(
        outdated_dependencies, current_dir: pathlib.Path,
        backup_dir: t.Optional[pathlib.Path] = None):
    if backup_dir is not None and not backup_dir.exists():
        _LOG.warning('Creating directory "%s"...', backup_dir)
        os.makedirs(str(backup_dir), exist_ok=True)
    for dependency, filename in outdated_dependencies.items():
        path = current_dir.joinpath(filename)
        if not path.is_file():
            _LOG.debug('%s already does not exist.', dependency)
            continue
        if backup_dir is None:
            _LOG.warning('Deleting %s in path "%s"...', dependency, current_dir)
            path.unlink()
        else:
            _LOG.warning('Moving %s from path "%s" to path "%s"...',
                         dependency, current_dir, backup_dir)
            path.move()
