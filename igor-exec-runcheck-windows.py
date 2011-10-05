#!/usr/bin/env python

import win32com.client
import pywintypes

def isAppRunning(com_string):
    """ Return boolean whether an application is running or not"""
    try:
        igorapp = win32com.client.GetActiveObject(com_string)
        return True
    except pywintypes.com_error:
        return False

if __name__ == '__main__':
    import sys
    isRunning = isAppRunning("IgorPro.Application")
    sys.stdout.write(str(isRunning))
