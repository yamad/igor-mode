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
   script-relative-path
   (file-name-directory
    (or load-file-name
        buffer-file-name))))

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
         (igor-exec-execute-windows cmd-list))
        ((eq system-type 'darwin)
         (igor-exec-execute-mac cmd-list))))

(defvar igor-exec-scriptname-mac
  (igor-exec-full-path-from-relative "igor-exec-mac.applescript")
  "Full path to the Mac OS X execution script")

(defun igor-exec-execute-mac (&rest cmd-list)
  "Executes the given Igor commands (Mac OS X)
See the function `igor-exec-execute'"
  (shell-command-to-string
   (format
    "osascript %s %s"
    igor-exec-scriptname-mac
    (igor-exec-compose-cmds cmd-list))))

(defvar igor-exec-scriptname-windows
  (igor-exec-full-path-from-relative "igor-exec-windows.py")
  "Full path to the Windows (python) execution script")

(defun igor-exec-execute-windows (&rest cmd-list)
  "Executes the given Igor commands (Windows)
See the function `igor-exec-execute'"
  (shell-command-to-string
   (format
    "python.exe %s %s"
    igor-exec-scriptname-windows
    (igor-exec-compose-cmds cmd-list))))

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
