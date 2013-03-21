;;; igor-exec.el --- Igor Pro interaction (execution) through emacs

;; Copyright (C) 2011  Jason Yamada-Hanff

;; Author: Jason Yamada-Hanff <jason@jason.macbook>
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; igor-exec (igor execution) provides an emacs interface for sending
;; and receiving commands from WaveMetrics' Igor Pro.
;;
;; Interaction with Igor Pro is platform-specfic, so a major goal of
;; this extension is to encapsulate platform differences and present a
;; single interface that works on all platforms.
;;
;; On Mac OS X, this module should work without any extra work since
;; the interaction depends on Applescript. This code has been tested
;; to work with OS X 10.6
;;
;; On Windows, a python script is used to interact with Igor. There is
;; no good built-in scripting language that will work easily for this
;; purpose. Therefore, a native Windows Python distribution and the
;; win32com module need to be present.

;;; Code:

(defun igor-exec-execute (&rest cmd-list)
  "Executes the given Igor commands

One or more commands should be passed as separate string
arguments. Commands will be escaped, collected into the CMD-LIST,
and issued to Igor Pro in order.

Delegates to the appropriate script based on platform. Code
should always favor using this function over the
platform-specific functions. Intended to work on Windows, Mac OS
X, and Linux with WINE installed."
  (cond ((eq system-type 'windows-nt)
         (apply 'igor-exec-execute-windows nil cmd-list))
        ((eq system-type 'darwin)
         (apply 'igor-exec-execute-mac nil cmd-list))
        ((eq system-type 'gnu/linux)
         (apply 'igor-exec-execute-wine nil cmd-list))))

(defun igor-exec-execute-unquoted (&rest cmd-list)
  "Executes the given Igor commands without filtering through a quoting function

Delegates to the appropriate script based on platform. Code
should always favor using this function over the
platform-specific functions. Intended to work on Windows, Mac OS
X, and Linux with WINE installed."
  (cond ((eq system-type 'windows-nt)
         (apply 'igor-exec-execute-windows t cmd-list))
        ((eq system-type 'darwin)
         (apply 'igor-exec-execute-mac t cmd-list))
        ((eq system-type 'gnu/linux)
         (apply 'igor-exec-execute-wine t cmd-list))))

(defun igor-exec-is-igor-running ()
  "Returns t if Igor Pro is running, nil if not"
  (cond ((eq system-type 'windows-nt)
         (igor-exec-is-igor-running-windows))
        ((eq system-type 'darwin)
         (igor-exec-is-igor-running-mac))
        ((eq system-type 'gnu/linux)
         (igor-exec-is-igor-running-wine))))

(defun igor-exec-execute-and-return (&rest cmd-list)
  "Executes the given Igor commands and returns their results"
  (apply 'igor-exec-execute
         (mapcar 'igor-exec-format-for-return cmd-list)))

(defun igor-exec-format-for-return (cmd)
  "Wrap Igor command CMD so that its result is returned to the caller"
  (format
   "fprintf 0, \"%%s\", %s" cmd))

(defun igor-exec-format-add-to-op-queue (cmd)
  "Wrap Igor command CMD for adding to the operation queue"
  (format
   "Execute/P/Q \"%s\"" cmd))

(defun igor-exec-cmd-close-procedure (include-name)
  "Return Igor command to close INCLUDE-NAME procedure window"
  (igor-exec-format-add-to-op-queue
   (format
    "CloseProc/NAME=\\\"%s\\\"" include-name)))

(defun igor-exec-cmd-insert-include (include-name)
  "Return Igor command to include INCLUDE-NAME from the experiment"
  (igor-exec-format-add-to-op-queue
   (format
    "INSERTINCLUDE \\\"%s\\\"" include-name)))

(defun igor-exec-cmd-delete-include (include-name)
  "Return Igor command to delete INCLUDE-NAME from the experiment"
  (igor-exec-format-add-to-op-queue
   (format
    "DELETEINCLUDE \\\"%s\\\"" include-name)))

(defun igor-exec-cmd-compileprocedures ()
  "Return Igor command to compile procedures

The space at the end of the string COMPILEPROCEDURES is
required (go figure)
"
  (igor-exec-format-add-to-op-queue "COMPILEPROCEDURES "))

(defun igor-exec-is-procs-compiled ()
  "Return t if the procedures are compiled, nil if not

A call to FunctionList() when the procedures are not compiled
returns the value \"Procedures Not Compiled\" as the first member
in the list. This behavior is undocumented, so it may change."
  (not (equal
   "Procedures Not Compiled"
   (car
    (igor-exec-igor-to-emacs-list
     (igor-exec-execute-and-return
      igor-exec-cmd-compile-status-probe))))))
(defvar igor-exec-cmd-compile-status-probe
  "FunctionList(\"xa6dEzCP_funcWontExist\", \";\", \"\")")

(defun igor-exec-clean-not-compiled-flag (func-list)
  "Return a *cleaned* version of the function list"
  (remove "Procecures Not Compiled" list))

(defun igor-exec-is-proc-included (include-name)
  "Return t if the procedure file INCLUDE-NAME is included in the
  current experiment, nil if not"
  (if (member include-name (igor-exec-include-list))
      t nil))

(defun igor-exec-include-list ()
  "Return list of all included files (*.ipf extensions are
stripped) in the current experiment"
  (mapcar
   'igor-exec-strip-ipf-extension
   (igor-exec-igor-to-emacs-list
    (igor-exec-full-include-list))))
(defun igor-exec-strip-ipf-extension (arg)
  (replace-regexp-in-string "\.ipf" "" arg))

(defun igor-exec-full-include-list ()
  "Execute Igor command to get all (including hidden) include
files

To make hidden include files visible, the IndependentModuleDev
setting must be on.
"
  (igor-exec-trim-whitespace
   (igor-exec-execute
    igor-exec-cmd-set-module-dev-on
    (igor-exec-format-for-return
     igor-exec-cmd-include-list)
    igor-exec-cmd-set-module-dev-off)))

(defun igor-exec-open-proc-window-list ()
  "Execute Igor command to get all open procedure windows

To make hidden include files visible, the IndependentModuleDev
setting must be on.
"
  (mapcar
   'igor-exec-strip-ipf-extension
   (igor-exec-igor-to-emacs-list
    (igor-exec-trim-whitespace
     (igor-exec-execute
      igor-exec-cmd-set-module-dev-on
      (igor-exec-format-for-return
       igor-exec-cmd-open-proc-window-list)
      igor-exec-cmd-set-module-dev-off)))))

(defun igor-exec-cmd-set-module-dev (flag)
  (format
   "SetIgorOption IndependentModuleDev=%d" flag))
(defvar igor-exec-cmd-set-module-dev-on
  (igor-exec-cmd-set-module-dev 1))
(defvar igor-exec-cmd-set-module-dev-off
  (igor-exec-cmd-set-module-dev 0))
(defun igor-exec-cmd-proc-list (visible)
  (format
  "WinList(\"*\", \";\", \"WIN:128,VISIBLE:%d\")" visible))
(defvar igor-exec-cmd-include-list
  (igor-exec-cmd-proc-list 0))
(defvar igor-exec-cmd-open-proc-window-list
  (igor-exec-cmd-proc-list 1))

(defun igor-exec-get-include-name (arg)
  (replace-regexp-in-string
   "^#include\\s-+\\\"\\([^\\\"]+\\)\\\"" "\\1" arg))
(defun igor-exec-local-includes-list (window)
  (mapcar 'igor-exec-get-include-name
   (igor-exec-igor-to-emacs-list
    (igor-exec-trim-whitespace
     (igor-exec-local-includes window))
    "\r")))

(defun igor-exec-local-includes (window)
  (let ((res))
        (progn
          (apply 'igor-exec-execute (igor-exec-cmd-includedprocs-begin window))
          (cond ((eq system-type 'windows-nt)
                 (apply 'igor-exec-execute-unquoted
                        igor-exec-cmd-includedprocs-windows-middle))
                ((eq system-type 'darwin)
                 (apply 'igor-exec-execute igor-exec-cmd-includedprocs-mac-middle))
                ((eq system-type 'gnu/linux)
                 (apply 'igor-exec-execute igor-exec-cmd-includedprocs-mac-middle)))
          (setq res (igor-exec-execute-and-return igor-exec-cmd-includedprocs-result))
          (apply 'igor-exec-execute igor-exec-cmd-includedprocs-end))
        res))

(defun igor-exec-cmd-includedprocs-begin (window)
  (split-string
   (format
    "NewDataFolder/O/S :procinclude_Y9sXbbaM
    String cline, inc_txt
    String proc_txt = ProcedureText(\"\", 0, \"%s\")" window) "\n+" t))

(defvar igor-exec-cmd-includedprocs-windows-middle
  '("\"String r_sep = \"\"\\r\"\"\""
    "\"String inc_re = \"\"^\\s*#include\\s*\\\\\"\"([^\\\\\"\"]+)\\\\\"\"\"\"")"On Windows, the carriage-return \r needs special escaping to get passed to Igor")

(defvar igor-exec-cmd-includedprocs-mac-middle
  '("String r_sep = \"\\r\""
    "String inc_re = \"^\\\s*#include\\\s*\\\"([^\\\"]+)\\\"\""))

(defvar igor-exec-cmd-includedprocs-result
   "GrepList(proc_txt, inc_re, 0, r_sep)")

(defvar igor-exec-cmd-includedprocs-end
  (split-string "KillDataFolder/Z :" "\n+" t))

(defun igor-exec-full-path-from-relative (file-relative-path)
  "Returns the full path to a file, given its path relative to
  this file.

FILE-RELATIVE-PATH must be a string holding the path to the
desired file relative to the current file."
  (expand-file-name
   file-relative-path
   (file-name-directory
    (or load-file-name
        buffer-file-name))))

;; Mac OS X specific functions
;; ===========================
(defun igor-exec-execute-mac (no-quote &rest cmd-list)
  "Executes the given Igor commands (Mac OS X)
See the function `igor-exec-execute'"
  (let (results)
     (dolist (cmd cmd-list results)
       (setq results
             (concat
              (shell-command-to-string
               (format
                "osascript %s %s"
                igor-exec-scriptname-mac
                (igor-exec-compose-cmds (cons cmd nil) no-quote))) results)))))
(defvar igor-exec-scriptname-mac
  (igor-exec-full-path-from-relative "igor-exec-mac.applescript")
  "Full path to the Mac OS X execution script")

(defun igor-exec-is-igor-running-mac ()
  "Returns t if Igor Pro is running, nil if not (Mac OS X)
See the function `igor-exec-is-igor-running'"
  (let ((run-probe
         (igor-exec-trim-whitespace
          (shell-command-to-string
           (format
            "osascript %s"
            igor-exec-scriptname-runcheck-mac)))))
    (if (equal run-probe "True")
        t nil)))
(defvar igor-exec-scriptname-runcheck-mac
  (igor-exec-full-path-from-relative "igor-exec-runcheck-mac.applescript")
  "Full path to the Windows (python) run test script")


;; Windows specific functions
;; ==========================
(defcustom igor-exec-path-to-python-windows
  "python.exe"
  "Command to run Python on Windows OS"
  :type 'string
  :group 'igor)

(defun igor-exec-format-python-run (path-to-python script-to-run no-quote &rest args)
  "Format a command to run the python script at the path string
SCRIPT-TO-RUN using the python interpreter at the path string
PATH-TO-PYTHON. ARGS will be passed to the script as if invoked
from a shell."
  (format "%s %s %s"
          path-to-python
          script-to-run
          (igor-exec-compose-cmds args no-quote)))

(defun igor-exec-format-python-execute (path-to-python no-quote &rest cmd-list)
"Format a command to execute the Igor commands CMD-LIST through
the python interpreter PATH-TO-PYTHON"
  (apply 'igor-exec-format-python-run
   path-to-python
   igor-exec-scriptname-windows
   no-quote
   cmd-list))

(defun igor-exec-format-python-is-igor-running (path-to-python)
"Format a command to test whether an Igor instance is running
using the python interpreter PATH-TO-PYTHON"
  (igor-exec-format-python-run
   path-to-python
   igor-exec-scriptname-runcheck-windows nil))

(defun igor-exec-execute-windows (no-quote &rest cmd-list)
  "Executes the given Igor commands (Windows)
See the function `igor-exec-execute'"
  (shell-command-to-string
   (apply 'igor-exec-format-python-execute
          igor-exec-path-to-python-windows no-quote cmd-list)))

(defvar igor-exec-scriptname-windows
  (igor-exec-full-path-from-relative "igor-exec-windows.py")
  "Full path to the Windows (python) execution script")

(defun igor-exec-is-igor-running-windows ()
  "Returns t if Igor Pro is running, nil if not (Windows)
See the function `igor-exec-is-igor-running'"
  (let ((run-probe
         (shell-command-to-string
          (igor-exec-format-python-is-igor-running
           igor-exec-path-to-python-windows))))
    (if (equal run-probe "True")
        t nil)))
(defvar igor-exec-scriptname-runcheck-windows
  (igor-exec-full-path-from-relative "igor-exec-runcheck-windows.py")
  "Full path to the Windows (python) run test script")

;; Linux Wine specific functions
;; =============================
(defcustom igor-exec-path-to-python-wine
  "wine ~/.wine/drive_c/Python27/python.exe"
  "Command to run Python through the Linux Wine Windows emulator"
  :type 'string
  :group 'igor)

(defun igor-exec-execute-wine (no-quote &rest cmd-list)
  "Executes the given Igor commands (Wine)
See the function `igor-exec-execute'"
  (shell-command-to-string
   (apply 'igor-exec-format-python-execute
    igor-exec-path-to-python-wine no-quote cmd-list)))

(defun igor-exec-is-igor-running-wine ()
  "Returns t if Igor Pro is running, nil if not (Wine)
See the function `igor-exec-is-igor-running'"
  (let ((run-probe
         (shell-command-to-string
          (igor-exec-format-python-is-igor-running
           igor-exec-path-to-python-wine))))
    (if (equal (substring run-probe -4 nil)
               "True")
        t nil)))

;; Helper functions
;; ================
(defun igor-exec-compose-cmds (cmd-list &optional no-quote)
  "Collect Igor commands for execution scripts

CMD-LIST must be a list of one or more commands held in
strings. Each argument is escaped/quoted appropriately. Returns a
string holding all commands properly prepared for passing to a
script."
  (if (not no-quote)
      (combine-and-quote-strings cmd-list)
    (mapconcat 'concat cmd-list " ")))

(defun igor-exec-igor-to-emacs-list (igor-list &optional sep)
  "Convert an igor list into a lisp list

IGOR-LIST is a string which holds list members bounded by the SEP
separator. The SEP strings are removed and members are collected
as a list.

If SEP is non-nil, it should be a string (usually a single
character) that separates the list members. If SEP is nil, the
separator defaults to \";\" (a semi-colon)."
  (if (not sep)
      (split-string igor-list ";" t)
    (split-string igor-list sep t)))

(defun igor-exec-trim-whitespace (str)
  "Remove leading and trailing whitespace from STR"
  (replace-regexp-in-string
   "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" str))

(provide 'igor-exec)
;;; igor-exec.el ends here
