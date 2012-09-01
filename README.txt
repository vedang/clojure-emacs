-*- mode: org; -*-

* Clojure-Emacs

This repository contains all the third party code required to make
Emacs an awesome clojure IDE. Namely, it contains the latest versions
of clojure-mode, clojure-compatible Slime, and ac-slime.

I've strived to make this configuration super simple to install and
use, if you face any problems please open an issue and let me know
about it.

* INSTALL
** Get the code:
   - git clone --recursive git://github.com/vedang/clojure-emacs.git ~/.emacs.d/path/to/clojure-emacs
** Add the following lines to your .emacs

   (add-to-list 'load-path "/path/to/clojure-emacs")
   (require 'clojure-emacs-init)

* Caveats
 - This configuration is ONLY known to work with Emacs 24. You are on your own with older versions of Emacs.
