;;; slime-config.el --- Configuration for Slime and lisp modes in general.
;;; Author: Vedang Manerikar
;;; Created on: 08 Jan 2012
;;; Time-stamp: "2012-05-28 13:54:23 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:

(require 'cl)


(defun turn-on-slime ()
  (slime-mode t))


(defun turn-on-paredit ()
  (paredit-mode t))


(defun turn-on-slime-paredit ()
  "Redefining paredit-space-for-delimiter function so that paredit behaves
well in slime."
  (require 'paredit)
  (defun paredit-space-for-delimiter-p (endp delimiter)
    (and (not (if endp (eobp) (bobp)))
         (memq (char-syntax (if endp (char-after) (char-before)))
               (list ?\" ;; REMOVED ?w ?_
                     (let ((matching (matching-paren delimiter)))
                       (and matching (char-syntax matching)))))))
  (define-key slime-repl-mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key slime-repl-mode-map
    (kbd "}") 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")
  (modify-syntax-entry ?~ "'   ")
  (modify-syntax-entry ?, "    ")
  (modify-syntax-entry ?^ "'")
  (modify-syntax-entry ?= "'")
  (paredit-mode t))


(progn
  (condition-case ex
      (def-slime-selector-method ?j
        "Go to the most recently visited clojure-mode buffer."
        (slime-recently-visited-buffer 'clojure-mode))
    ('error (message (format "Caught Exception: [%s]" ex))))
  (global-set-key (kbd "C-c z") 'slime-selector))


(setq slime-net-coding-system 'utf-8-unix
      slime-protocol-version 'ignore)
(slime-setup '(slime-repl
               slime-scratch
               slime-editing-commands))

(add-hook 'lisp-mode-hook 'turn-on-slime)
(add-hook 'lisp-mode-hook 'turn-on-paredit)
(add-hook 'inferior-lisp-mode-hook 'turn-on-slime)
(add-hook 'slime-mode-hook 'turn-on-slime-paredit)
(add-hook 'slime-repl-mode-hook 'turn-on-slime-paredit)
(add-hook 'slime-connected-hook 'turn-on-slime-paredit)


(defadvice slime-repl-emit (after sr-emit-ad activate)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after sr-prompt-ad activate)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))


;; Translate filenames accessed over tramp so slime works on remote machines
;; https://groups.google.com/forum/#!msg/swank-clojure/av0vE-z54ZQ/O80OA-Vt8TsJ
;; Hat-tip: Sidhant (http://github.com/grinnbearit/dot-emacs/)
(defun slime-tramp-local-filename (f)
  (interactive)
  (if (file-remote-p f)
      (tramp-file-name-localname
       (tramp-dissect-file-name f))
    f))


(defun slime-tramp-remote-filename (f)
  (interactive)
  (if (file-remote-p default-directory)
      (tramp-make-tramp-file-name
       (tramp-file-name-method
        (tramp-dissect-file-name default-directory))
       (tramp-file-name-user
        (tramp-dissect-file-name default-directory))
       (tramp-file-name-host
        (tramp-dissect-file-name default-directory))
       f)
    f))


(defun slime-remote-file-name-hook ()
  (interactive)
  (setq slime-from-lisp-filename-function
        'slime-tramp-remote-filename)
  (setq slime-to-lisp-filename-function
        'slime-tramp-local-filename))


(add-hook 'slime-connected-hook 'slime-remote-file-name-hook)


;;; Auto complete integration with slime
(eval-after-load "auto-complete"
  '(progn
     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
     (add-to-list 'ac-modes 'slime-repl-mode)))


(provide 'slime-config)
