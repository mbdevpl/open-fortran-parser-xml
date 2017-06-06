#!/usr/bin/env python3

"""Development dependency downloader for open_fortran_parser."""

import logging
import os
import pathlib
import typing as t
import urllib

import wget

_LOG = logging.getLogger(__name__)

ROOT_PATH = pathlib.Path(os.getcwd(), 'lib')

DEV_DEPENDENCIES = {
    'ANTLR 3.3': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser/releases/download/v0.8.4-1/'),
        pathlib.Path('antlr-3.3-complete.jar')),
    'Open Fortran Parser 0.8.4.1': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser/releases/download/v0.8.4-1/'),
        pathlib.Path('OpenFortranParser-0.8.4-1.jar')),
    'Apache Commons CLI 1.4': (
        urllib.parse.urlparse(
            'https://github.com/mbdevpl/open-fortran-parser-xml/releases/download/v0.1.0/'),
        pathlib.Path('commons-cli-1.4.jar'))}


def ensure_dependencies(
        dependencies: t.Mapping[str, t.Tuple[urllib.parse.ParseResult, str]] = None,
        target_dir: pathlib.Path = None, silent: bool = False) -> None:
    """Download missing depenedencies."""
    if dependencies is None:
        dependencies = DEV_DEPENDENCIES
    if target_dir is None:
        target_dir = ROOT_PATH
    for dependency, (url_root, filename) in dependencies.items():
        path = target_dir.joinpath(filename)
        if path.is_file():
            _LOG.debug('%s is present already.', dependency)
            continue
        url = urllib.parse.urlunparse(url_root) + str(filename)
        if not target_dir.exists():
            _LOG.warning('Creating directory "%s"...', target_dir)
            os.makedirs(str(target_dir), exist_ok=True)
        _LOG.warning('Downloading %s from URL "%s" to path "%s"...', dependency, url, path)
        wget.download(url, str(path), bar=None if silent else wget.bar_adaptive)
        if not silent:
            print()
    if not silent:
        _LOG.warning('Please add "%s/*" to your Java classpath:', target_dir.joinpath('*'))
        _LOG.warning('export CLASSPATH="${CLASSPATH}:%s"', target_dir.joinpath('*'))


if __name__ == '__main__':
    ensure_dependencies()
