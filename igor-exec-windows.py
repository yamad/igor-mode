#!/usr/bin/env python

""" Execute Igor commands externally from Windows

Call this script with one or more commands passed as individual
arguments. The passed commands will be run, in order, by Igor.

There is no simple scripting language in Windows that will get
information back from Igor. In VBScript, it is not possible to call
the Execute2 function from the Igor COM object because Execute2
returns pointers to type BSTR. VBScript can't handle this.

Requires: pywin32
"""

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


class WindowsIgorCommunicator(object):
    def __init__(self):
        import win32com.client

        self.igorapp = win32com.client.gencache.EnsureDispatch("IgorPro.Application")
        self.constants = win32com.client.constants

    def execute(self, cmd):
        flag_nolog = 1
        code_page = 0

        err_code = 0
        err_msg = ""
        history = ""
        results = ""

        result_tuple = self.igorapp.Execute2(flag_nolog, code_page, cmd,
                                             err_code, err_msg,
                                             history, results)
        err_code, err_msg, history, results = result_tuple
        return results

def main(argv):
    """ Call with one or more Igor commands as arguments. Each command
    will be run in Igor. No other arguments are accepted.
    """
    commands = argv

    # no commands given, so quit
    if len(commands) < 1:
        return ""

    igorapp = WindowsIgorCommunicator()
    results = []
    for cmd in commands:
        results.append(igorapp.execute(cmd))

    return '\n'.join(results)

if __name__ == '__main__':
    with nostderr():
        sys.stdout.write(main(sys.argv[1:]))
