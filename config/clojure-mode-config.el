;;; clojure-mode-config.el --- configuration for clojure
;;; Author: Vedang Manerikar
;;; Created on: 10 Jan 2012
;;; Time-stamp: "2012-07-10 22:11:34 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


(defun pretty-fns ()
  (font-lock-add-keywords
   nil `(("(\\(fn\\)[\[[:space:]]"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    "ƒ")
                    nil))))))


(defun pretty-reader-macros ()
  (font-lock-add-keywords
   nil `(("\\(#\\)("
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    "ƒ")
                    nil))))))


(defun pretty-sets ()
  (font-lock-add-keywords
   nil `(("\\(#\\){"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    "∈")
                    nil))))))


(defun turn-on-paredit-clojure ()
  (require 'paredit)
  (paredit-mode t))


(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (describe 'defun)
     (testing 'defun)
     (given 'defun)
     (using 'defun)
     (with 'defun)
     (it 'defun)
     (do-it 'defun)))


;;; http://stackoverflow.com/questions/2474804/is-there-a-colored-repl-for-clojure
(defun turn-on-clojure-font-lock-setup ()
  (interactive)
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (set (make-local-variable 'font-lock-multiline) t)

  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)

  (when clojure-mode-font-lock-comment-sexp
    (add-to-list 'font-lock-extend-region-functions
                 'clojure-font-lock-extend-region-comment t)
    (make-local-variable 'clojure-font-lock-keywords)
    (add-to-list 'clojure-font-lock-keywords
                 'clojure-font-lock-mark-comment t)
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil))

  (setq font-lock-defaults
        '(clojure-font-lock-keywords ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))


(eval-after-load "slime-config"
  '(progn
     (add-hook 'slime-repl-mode-hook 'turn-on-clojure-font-lock-setup)))



;;; Re-implementation of clojure-test-mode functions for Midje
;;; Hat-tip : Kapil Reddy
;;; https://github.com/kapilreddy/dotemacs/blob/5d6cfc2215b8f1eb2dd0ca14d871478fee053db3/configurations/clojure-config.el

(defun midje-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (test-segments (append (list "test") segments)))
    (mapconcat 'identity test-segments "/")))


(defun midje-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (format "%s/%s_test.clj"
                     (file-name-as-directory
                      (locate-dominating-file buffer-file-name "src/"))
                     (midje-test-for (clojure-find-ns)))))


(defun midje-implementation-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string (replace-regexp-in-string "_test" "" namespace) "\\.")))
    (mapconcat 'identity segments "/")))


(defun midje-jump-to-implementation ()
  "Jump from midje test file to implementation."
  (interactive)
  (find-file (format "%s/src/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (midje-implementation-for (clojure-find-package)))))


(defun midje-jump-between-tests-and-code ()
  (interactive)
  (if (clojure-in-tests-p)
      (midje-jump-to-implementation)
    (midje-jump-to-test)))


(defun midje-test-maybe-enable ()
  "Stop clojure-test-mode from loading, instead use my midje functions"
  (let ((ns (clojure-find-package)))
    (when (and ns (string-match "test\\(\\.\\|$\\)" ns))
      (if (and (listp clojure-mode-hook)
               (memq 'clojure-test-maybe-enable clojure-mode-hook))
          (remove-hook 'clojure-mode-hook 'clojure-test-maybe-enable)))))


;;;###autoload
(progn
  (add-hook 'clojure-mode-hook 'turn-on-paredit-clojure)
  (add-hook 'clojure-mode-hook 'pretty-fns)
  (add-hook 'clojure-mode-hook 'pretty-sets)
  (add-hook 'clojure-mode-hook 'pretty-reader-macros)
  ;; I use Midje for writing clojure tests.
  ;; Activating clojure-test-mode is irritating for me.
  ;; Didn't want to change lib mode, so removing it here.
  (add-hook 'clojure-mode-hook 'midje-test-maybe-enable)
  (eval-after-load "clojure-mode"
    '(progn
       (define-key clojure-mode-map
         (kbd "C-c t")
         'midje-jump-between-tests-and-code))))


(provide 'clojure-mode-config)
