;;; nrepl-config.el --- Configuration for nRepl
;;; Author: Vedang Manerikar
;;; Created on: 05 Jan 2013
;;; Time-stamp: "2013-01-05 21:34:54 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)


(defun turn-on-nrepl-paredit ()
  "Paredit for the win"
  (require 'paredit)
  (paredit-mode t))


(add-hook 'nrepl-mode-hook 'turn-on-nrepl-paredit)


(eval-after-load "auto-complete"
  '(progn
     (require 'ac-nrepl)
     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
     (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
     (add-to-list 'ac-modes 'nrepl-mode)))


(provide 'nrepl-config)
