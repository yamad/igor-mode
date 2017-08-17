#!/usr/bin/env python

# License: GPL-3
#
# Copyright (C) 2011  Jason Yamada-Hanff
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import win32com.client as w32c
import pywintypes

import contextlib
import sys

@contextlib.contextmanager
def nostderr():
    savestderr = sys.stderr
    class Devnull(object):
        def write(self, _): pass
    sys.stderr = Devnull()
    yield
    sys.stderr = savestderr


def isAppRunning(com_string):
    """ Return boolean whether an application is running or not"""
    try:
        igorapp = w32c.GetActiveObject(com_string)
        return True
    except pywintypes.com_error:
        return False

if __name__ == '__main__':
    with nostderr():
        isRunning = isAppRunning("IgorPro.Application")
        sys.stdout.write(str(isRunning))
