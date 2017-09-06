"""Build script for open_fortran_parser package."""

import setup_boilerplate


class Package(setup_boilerplate.Package):

    """Package metadata."""

    name = 'open-fortran-parser'
    description = 'Python wrapper for XML output generator for Open Fortran Parser'
    download_url = 'https://github.com/mbdevpl/open-fortran-parser-xml'
    classifiers = [
        'Development Status :: 1 - Planning',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: Apache Software License',
        'Natural Language :: English',
        'Operating System :: POSIX',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3 :: Only',
        'Topic :: Education',
        'Topic :: Scientific/Engineering',
        'Topic :: Utilities']
    keywords = ['abstract syntax tree', 'ast', 'parser', 'xml']


if __name__ == '__main__':
    Package.setup()
