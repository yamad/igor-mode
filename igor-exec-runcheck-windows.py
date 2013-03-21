#!/usr/bin/env python

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
