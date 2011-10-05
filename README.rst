===========
 igor-mode
===========

An emacs_ major mode for editing `Igor Pro`_ procedure files (*.ipf)

This mode currently provides syntax highlighting and indentation for
the Igor programming language syntax.

It has been tested with GNU emacs 23.2 and Igor Pro 6.22A, but it may
work with other versions.

Install
=======

Install like any other emacs package, by adding the following lines to
your emacs initialization file (e.g. .emacs)::

  (add-to-list 'load-path "<path/to/igor-mode>")
  (require 'igor-mode)

Auto-reloading
==============

There is also an experimental feature that automatically reloads *.ipf
files from the running Igor experiment when the file is saved in
emacs. This is necessary because Igor write-protects any loaded *.ipf
files. The auto-reloading works well as long as the file is not edited
in emacs and Igor at the same time--that is, you should do all of the
file editing in emacs.

The Windows version of the auto-reload feature requires a native
Windows python_ distribution and the pywin32_ package. Both
ActivePython_ and EPD_ bundle pywin32 in their distribution, so this
is an easy way to get up and running quickly.


.. _emacs: http://www.gnu.org/s/emacs
.. _`Igor Pro`: http://www.wavemetrics.com

.. _python: http://www.python.org
.. _pywin32: http://sourceforge.net/projects/pywin32/
.. _ActivePython: http://www.activestate.com/activepython
.. _EPD: http://www.enthought.com/products/epd.php
