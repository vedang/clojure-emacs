;;; clojure-emacs-init.el --- Load code for a better clojure environment
;;; Author: Vedang Manerikar
;;; Created on: 27 May 2012
;;; Time-stamp: "2013-08-15 01:18:13 vedang"
;;; Copyright (c) 2012 Vedang Manerikar <vedang.manerikar@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Commentary:
;; To use this code, simply add
;;    (add-to-list 'load-path "/path/to/clojure-emacs/")
;;    (require 'clojure-emacs-init)
;; to your .emacs file

;;; Code:


(defun vedang/list-dirs-recursively (directory)
  "List all the directories in DIRECTORY and in it's sub-directories."
  (let* ((current-directory-list (directory-files-and-attributes directory t))
         (dirs-list (delq nil (mapcar (lambda (lst)
                                        (and (car (cdr lst))
                                             (not (equal "." (substring (car lst) -1)))
                                             (not (equal ".git" (substring (car lst) -4)))
                                             (car lst)))
                                      current-directory-list))))
    (apply #'append dirs-list (mapcar (lambda (d)
                                        (vedang/list-dirs-recursively d))
                                      dirs-list))))


(defun vedang/add-dirs-to-load-path (directory)
  "Add all directories under the input directory to the emacs load path"
  (dolist (dirname (vedang/list-dirs-recursively directory))
    (add-to-list 'load-path dirname)))


(defvar current-dirname (file-name-directory
                         (or load-file-name (buffer-file-name)))
  "The directory where clojure-emacs resides")
(defvar tempfiles-dirname (concat current-dirname "temp-files/")
  "Directory to store temporary files")
(make-directory tempfiles-dirname t)


(vedang/add-dirs-to-load-path current-dirname)


(require 'nrepl-autoloads)
(autoload 'clojure-mode "clojure-mode" "Load clojure mode" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))


(eval-after-load "clojure-mode"
  '(progn
     (require 'clojure-mode-config)))
(eval-after-load "nrepl"
  '(progn
     (require 'nrepl-config)))


(provide 'clojure-emacs-init)
