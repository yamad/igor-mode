===========
 igor-mode
===========

An emacs_ major mode for editing `Igor Pro`_ procedure files (\*.ipf)

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

There is also an experimental feature that automatically reloads \*.ipf
files from the running Igor experiment when the file is saved in
emacs. This is necessary because Igor write-protects any loaded \*.ipf
files. The auto-reloading works well as long as the file is not edited
in emacs and Igor at the same time--that is, you should do all of the
file editing in emacs.

Auto-reload interacts with Igor through the underlying operating
system. Windows, Mac OS X, and Linux (running Wine_) are supported.

The Windows and Linux/Wine versions of the auto-reload feature require
a native Windows python_ distribution and the pywin32_ package. Both
ActivePython_ and EPD_ bundle pywin32 in their distribution.

The official Python_ distribution has been tested to work with
Linux/Wine. If you are running `Gentoo Linux`_ make sure that Wine
(app-emulation/wine) is installed with the the *gnutls* and the
*samba* USE flags on. The command to run Wine python is held in the
variable `igor-exec-path-to-python-wine`, which will likely need to be
customized for your particular setup.

Auto-reload can be turned off by setting the `igor-use-autoreload`
customization variable to `nil`. It is activated by default.


.. _emacs: http://www.gnu.org/s/emacs
.. _`Igor Pro`: http://www.wavemetrics.com

.. _python: http://www.python.org
.. _pywin32: http://sourceforge.net/projects/pywin32/
.. _ActivePython: http://www.activestate.com/activepython
.. _EPD: http://www.enthought.com/products/epd.php
.. _Wine: http://www.winehq.org
.. _`Gentoo Linux`: http://www.gentoo.org
