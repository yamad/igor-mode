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
platform-specific functions. Currently is a no-op on platforms
that are not Windows or Mac OS X."
  (cond ((eq system-type 'windows-nt)
         (apply 'igor-exec-execute-windows cmd-list))
        ((eq system-type 'darwin)
         (apply 'igor-exec-execute-mac cmd-list))))

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
    (igor-exec-execute-and-return
     igor-exec-cmd-include-list))))
(defun igor-exec-strip-ipf-extension (arg)
  (replace-regexp-in-string "\.ipf" "" arg))
(defvar igor-exec-cmd-include-list
  "WinList(\"*\", \";\", \"WIN:128\")")

;; Mac OS X specific functions
;; ===========================
(defun igor-exec-execute-mac (&rest cmd-list)
  "Executes the given Igor commands (Mac OS X)
See the function `igor-exec-execute'"
  (shell-command-to-string
   (format
    "osascript %s %s"
    igor-exec-scriptname-mac
    (igor-exec-compose-cmds cmd-list))))

(defvar igor-exec-scriptname-mac
  (igor-exec-full-path-from-relative "igor-exec-mac.applescript")
  "Full path to the Mac OS X execution script")

;; Windows specific functions
;; ==========================
(defun igor-exec-execute-windows (&rest cmd-list)
  "Executes the given Igor commands (Windows)
See the function `igor-exec-execute'"
  (shell-command-to-string
   (format
    "python.exe %s %s"
    igor-exec-scriptname-windows
    (igor-exec-compose-cmds cmd-list))))
(defvar igor-exec-scriptname-windows
  (igor-exec-full-path-from-relative "igor-exec-windows.py")
  "Full path to the Windows (python) execution script")

(defun igor-exec-compose-cmds (cmd-list)
  "Collect Igor commands for execution scripts

CMD-LIST must be a list of one or more commands held in
strings. Each argument is escaped/quoted appropriately. Returns a
string holding all commands properly prepared for passing to a
script."
  (combine-and-quote-strings cmd-list))

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

(provide 'igor-exec)
;;; igor-exec.el ends here
