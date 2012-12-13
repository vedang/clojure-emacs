;;; nrepl-config.el --- configuration for nrepl
;;; Author: Vedang Manerikar
;;; Created on: 23 Nov 2012
;;; Time-stamp: "2012-11-23 16:39:17 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode 'paredit-mode)


;;; Auto complete integration with nrepl
(eval-after-load "auto-complete"
  '(progn
     (require 'ac-nrepl)
     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
     (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
     (add-to-list 'ac-modes 'nrepl-mode)
     (defun set-auto-complete-as-completion-at-point-function ()
       (add-to-list 'completion-at-point-functions 'auto-complete))
     (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)))


(provide 'nrepl-config)
