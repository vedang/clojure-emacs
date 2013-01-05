-*- mode: org; -*-

* Clojure-Emacs

This repository contains all the third party code required to make
Emacs an awesome clojure IDE. Namely, it contains the latest versions
of clojure-mode, nrepl.el, and ac-nrepl.

I've strived to make this configuration super simple to install and
use, if you face any problems please open an issue and let me know
about it.

* INSTALL
** Get the code:
   $ git clone --recursive git://github.com/vedang/clojure-emacs.git /path/to/clojure-emacs
** Add the following lines to your .emacs

   (add-to-list 'load-path "/path/to/clojure-emacs")
   (require 'clojure-emacs-init)

* Caveats
This configuration is ONLY known to work with Emacs 24. You are on
your own with older versions of Emacs.

* Slime support
Support for slime was dropped with clojure-mode 2.0.0. Hence,
clojure-emacs has also dropped support for slime. If you want a fully
working configuration for slime, move back to the tag 0.2.0

   $ cd /path/to/clojure-emacs
   $ git checkout 0.2.0
