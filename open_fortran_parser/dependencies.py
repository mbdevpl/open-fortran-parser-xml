"""Dependency downloader for open_fortran_parser."""

import logging
import os
import pathlib
import platform
import typing as t
import urllib

import wget

_LOG = logging.getLogger(__name__)


def ensure_dependencies(
        dependencies: t.Mapping[str, t.Tuple[urllib.parse.ParseResult, pathlib.Path]],
        target_dir: pathlib.Path, download: bool = True, silent: bool = False) -> None:
    """Download missing depenedencies."""
    if not target_dir.exists():
        _LOG.warning('Creating directory "%s"...', target_dir)
        os.makedirs(str(target_dir), exist_ok=True)
    for dependency, (url_root, filename) in dependencies.items():
        path = target_dir.joinpath(filename)
        if path.is_file() and not silent:
            _LOG.warning('%s is present already.', dependency)
            continue
        if not download:
            _LOG.warning('%s is not present!', dependency)
            continue
        url = urllib.parse.urlunparse(url_root) + str(filename)
        _LOG.warning('Downloading %s from URL "%s" to path "%s"...', dependency, url, path)
        wget.download(url, str(path), bar=None if silent else wget.bar_adaptive)
        if not silent:
            print()

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
            path.rename(backup_dir.joinpath(filename))
