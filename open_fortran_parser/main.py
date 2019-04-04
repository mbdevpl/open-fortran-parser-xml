"""Main function for open_fortran_parser package."""

import argparse
import logging
import pathlib
import sys

from ._version import VERSION
from .config import \
    DEV_DEPENDENCIES_PATH, DEV_DEPENDENCIES, DEPENDENCIES_PATH, DEPENDENCIES, OUTDATED_DEPENDENCIES
from .parser_wrapper import execute_parser
from .dependencies import ensure_dependencies, cleanup_old_dependencies

logging.basicConfig()

_LOG = logging.getLogger(__name__)


def main(args=None, namespace=None):
    """Launch Open Fortran Parser."""
    parser = argparse.ArgumentParser(
        prog='open_fortran_parser',
        description='''Python wrapper around XML generator for Open Fortran Parser''',
        epilog='''Copyright 2017-2019 by the contributors, Apache License 2.0,
            https://github.com/mbdevpl/open-fortran-parser-xml''',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.version = VERSION
    parser.add_argument('--version', action='version')

    parser.add_argument(
        'input', nargs='?', type=pathlib.Path, help='''path to Fortran source code file''')
    parser.add_argument(
        'output', nargs='?', type=pathlib.Path, default=None,
        help='''writable path for where to store resulting XML, defaults to stdout
            if no path provided''')
    parser.add_argument(
        '-v', '--verbosity', type=int, default=100, help='''level of verbosity, from 0 to 100''')
    parser.add_argument(
        '--get-dependencies', '--deps', action='store_true',
        help='''download dependencies and exit''')
    parser.add_argument(
        '--get-development-dependencies', '--dev-deps', action='store_true',
        help=argparse.SUPPRESS)
    parser.add_argument(
        '--cleanup-dependencies', '--cleanup-deps', action='store_true',
        help=argparse.SUPPRESS)

    args = parser.parse_args(args, namespace)

    if args.get_development_dependencies:
        ensure_dependencies(DEV_DEPENDENCIES, DEV_DEPENDENCIES_PATH)
        return

    if args.get_dependencies:
        ensure_dependencies(DEPENDENCIES, DEPENDENCIES_PATH)
        return

    if args.cleanup_dependencies:
        cleanup_old_dependencies(OUTDATED_DEPENDENCIES, DEV_DEPENDENCIES_PATH)
        return

    if not args.input:
        parser.print_help(sys.stderr)
        parser.exit(2)

    process = execute_parser(args.input, args.output, args.verbosity)
    if process.stderr:
        _LOG.warning(process.stderr.decode().rstrip())
    process.check_returncode()
    if args.output is None:
        print(process.stdout.decode().rstrip())
