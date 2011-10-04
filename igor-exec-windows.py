#!/usr/bin/env python

""" Execute Igor commands externally from Windows

Call this script with one or more commands passed as individual
arguments. The passed commands will be run, in order, by Igor.

There is no simple scripting language in Windows that will get
information back from Igor. In VBScript, it is not possible to call
the Execute2 function from the Igor COM object because Execute2
returns pointers to type BSTR. VBScript can't handle this.
"""

class WindowsIgorCommunicator(object):
    def __init__(self):
        import win32com.client

        self.igorapp = win32com.client.gencache.EnsureDispatch("IgorPro.Application")
        self.constants = win32com.client.constants

    def execute(self, cmd):
        flag_nolog = 0
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
    import sys
    print main(sys.argv[1:])
