"""Main function for open_fortran_parser package."""

import argparse
import logging
import pathlib

from .parser_wrapper import execute_parser

logging.basicConfig()

_LOG = logging.getLogger(__name__)


def main(args=None, namespace=None):
    """Launch Open Fortran Parser."""
    parser = argparse.ArgumentParser(
        prog='open_fortran_parser',
        description='''Python wrapper around XML generator for Open Fortran Parser 0.8.4''',
        epilog='''Copyright 2017 Mateusz Bysiek https://mbdevpl.github.io/, Apache License 2.0''',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('input', type=pathlib.Path, help='''path to Fortran source code file''')
    parser.add_argument(
        'output', nargs='?', type=pathlib.Path, default=None,
        help='''writable path for where to store resulting XML, defaults to stdout
            if no path provided''')
    parser.add_argument(
        '-v', '--verbosity', type=int, default=100, help='''level of verbosity, from 0 to 100''')

    args = parser.parse_args(args, namespace)

    process = execute_parser(args.input, args.output, args.verbosity)
    if process.stderr:
        _LOG.warning(process.stderr.decode().rstrip())
    process.check_returncode()
    if args.output is None:
        print(process.stdout.decode().rstrip())
