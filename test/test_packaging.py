"""Tests of packaging."""

import os
import runpy
import sys
import unittest


class Tests(unittest.TestCase):

    def run_setup(self, *args):
        sys.argv = ['setup.py'] + list(args)
        runpy.run_module('setup', run_name='__main__')

    @unittest.skip('failing in this context')
    def test_bdist(self):
        self.run_setup('bdist')

    @unittest.skipUnless(os.environ.get('TEST_PACKAGING'), 'skipping packaging test')
    def test_bdist_wheel(self):
        self.run_setup('bdist_wheel')

    @unittest.skipUnless(os.environ.get('TEST_PACKAGING'), 'skipping packaging test')
    def test_sdist(self):
        self.run_setup('sdist', '--formats=gztar,zip')
