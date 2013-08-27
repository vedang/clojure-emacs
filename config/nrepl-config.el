;;; nrepl-config.el --- Configuration for nRepl
;;; Author: Vedang Manerikar
;;; Created on: 05 Jan 2013
;;; Time-stamp: "2013-08-27 16:03:08 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(setq nrepl-popup-stacktraces-in-repl t
      nrepl-history-file (concat tempfiles-dirname "nrepl-history.txt")
      nrepl-buffer-name-separator "-"
      nrepl-buffer-name-show-port t)


(defun turn-on-nrepl-paredit ()
  "Paredit for the win"
  (require 'paredit)
  (paredit-mode t))


(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'turn-on-nrepl-paredit)
(add-hook 'nrepl-mode-hook 'subword-mode)


(eval-after-load "auto-complete"
  '(progn
     (require 'ac-nrepl)
     (defun activate-ac-nrepl? ()
       "ac-nrepl doesn't work well in production environments. Don't activate it
       there."
       (when (y-or-n-p "Activate AC-nREPL?")
         (ac-nrepl-setup)))
     (add-hook 'nrepl-mode-hook 'activate-ac-nrepl?)
     (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
     (add-to-list 'ac-modes 'nrepl-mode)))


(eval-after-load "clojure-mode"
  '(progn
    (defun fix-nrepl-indentation ()
      (setq-local lisp-indent-function 'clojure-indent-function))
    (add-hook 'nrepl-mode-hook 'fix-nrepl-indentation)))


(global-set-key (kbd "C-c z") 'nrepl-selector)


(require 'nrepl-decompile)
(provide 'nrepl-config)
