"""Build script for open_fortran_parser package."""

import importlib
import pathlib
import shutil
import sys
import typing as t

import setuptools

_SRC_DIR = '.'
"""Directory with source code, relative to the setup.py file location."""


def setup() -> None:
    """Run setuptools.setup() with correct arguments.

    List of valid project classifiers: https://pypi.python.org/pypi?:action=list_classifiers

    The extras_require is a dictionary which might have the following key-value pairs:
    'some_feature': ['requirement1', 'requirement2'],

    The entry_points is a dictionary which might have the following key-value pair:
    'console_scripts': ['script_name = package.subpackage:function']
    """
    name = 'open-fortran-parser'
    version = find_version(name.replace('-', '_'))
    description = 'Python wrapper for XML output generator for Open Fortran Parser'
    url = 'https://mbdevpl.github.io/'
    download_url = 'https://github.com/mbdevpl/open-fortran-parser-xml'
    author = 'Mateusz Bysiek'
    author_email = 'mb@mbdev.pl'
    license_str = 'Apache License 2.0'
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
        'Topic :: Utilities'
        ]
    keywords = ['abstract syntax tree', 'ast', 'parser', 'xml']
    package_data = {}
    exclude_package_data = {}
    extras_require = {}
    entry_points = {}
    test_suite = 'test'

    setuptools.setup(
        name=name, version=version, description=description,
        long_description=parse_readme(), url=url, download_url=download_url,
        author=author, author_email=author_email,
        maintainer=author, maintainer_email=author_email,
        license=license_str, classifiers=classifiers, keywords=keywords,
        packages=find_packages(), package_dir={'': _SRC_DIR}, include_package_data=True,
        package_data=package_data, exclude_package_data=exclude_package_data,
        install_requires=parse_requirements(), extras_require=extras_require,
        python_requires=find_required_python_version(classifiers),
        entry_points=entry_points, test_suite=test_suite)


# below code is generic boilerplate and normally should not be changed
# last update: 2017-06-01

_HERE = pathlib.Path(__file__).resolve().parent


def clean(build_directory_name: str = 'build') -> None:
    """Recursively delete build directory (by default "build") if it exists."""
    build_directory_path = _HERE.joinpath(build_directory_name)
    if build_directory_path.is_dir():
        shutil.rmtree(str(build_directory_path))


def find_version(
        package_name: str, version_module_name: str = '_version',
        version_variable_name: str = 'VERSION') -> str:
    """Simulate behaviour of "from package_name._version import VERSION", and return VERSION."""
    version_module = importlib.import_module('{}.{}'.format(package_name, version_module_name))
    return getattr(version_module, version_variable_name)


def find_packages() -> t.List[str]:
    """Find packages to pack."""
    exclude = ['test', 'test.*'] if 'bdist_wheel' in sys.argv else []
    packages_list = setuptools.find_packages(_SRC_DIR, exclude=exclude)
    return packages_list


def parse_readme(readme_path: str = 'README.rst', encoding: str = 'utf-8') -> str:
    """Read contents of readme file (by default "README.rst") and return them."""
    with open(_HERE.joinpath(readme_path), encoding=encoding) as readme_file:
        desc = readme_file.read()
    return desc


def parse_requirements(
        requirements_path: str = 'requirements.txt') -> t.List[str]:
    """Read contents of requirements.txt file and return data from its relevant lines.

    Only non-empty and non-comment lines are relevant.
    """
    requirements = []
    with open(_HERE.joinpath(requirements_path)) as reqs_file:
        for requirement in [line.strip() for line in reqs_file.read().splitlines()]:
            if not requirement or requirement.startswith('#'):
                continue
            requirements.append(requirement)
    return requirements


def find_required_python_version(
        classifiers: t.Sequence[str], ver_prefix: str = 'Programming Language :: Python :: ',
        only_suffix: str = ' :: Only') -> str:
    """Determine the minimum required Python version."""
    versions = [ver.replace(ver_prefix, '') for ver in classifiers if ver.startswith(ver_prefix)]
    versions_min = [ver for ver in versions if not ver.endswith(only_suffix)]
    versions_only = [ver.replace(only_suffix, '') for ver in versions if ver.endswith(only_suffix)]
    versions_min = [tuple([int(_) for _ in ver.split('.')]) for ver in versions_min]
    versions_only = [tuple([int(_) for _ in ver.split('.')]) for ver in versions_only]
    if len(versions_only) > 1:
        raise ValueError(
            'more than one "{}" version encountered in {}'.format(only_suffix, versions_only))
    only_version = None
    if len(versions_only) == 1:
        only_version = versions_only[0]
        for version in versions_min:
            if version[:len(only_version)] != only_version:
                raise ValueError(
                    'the "{}" version {} is inconsistent with version {}'
                    .format(only_suffix, only_version, version))
    min_supported_version = None
    for version in versions_min:
        if min_supported_version is None or \
                (len(version) >= len(min_supported_version) and version < min_supported_version):
            min_supported_version = version
    if min_supported_version is None:
        if only_version is not None:
            return '.'.join([str(_) for _ in only_version])
    else:
        return '>=' + '.'.join([str(_) for _ in min_supported_version])
    return None


def main() -> None:
    clean()
    setup()


if __name__ == '__main__':
    main()
