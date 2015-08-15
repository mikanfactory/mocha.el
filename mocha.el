;;; mocha.el --- Run mocha quickly -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Shoji Sugai

;; Author: Shoji Sugai <k952i4j14x17@gmail.com>
;; Maintainer: Shoji Sugai <k952i4j14x17@gmail.com>
;; Version: 0.0.1
;; Keywords: test, javascript, mocha
;; URL: http://github.com/mikanfactory/mocha.el
;; Package-Requires: ((s "1.9.0") (dash "2.5.0") (f "0.16.0") (emacs "24))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'f)
(require 's)
(require 'dash)

(defgroup mocha nil
  "Run mocha quickly"
  :prefix "mocha"
  :group 'languages)

(defcustom mocha-reporter "spec"
  "Used in reporter option"
  :type 'string
  :group 'mocha)

(defcustom mocha-project-root-specifier "node_modules"
  "Search this directory as a project root"
  :type 'string
  :group 'mocha)

(defvar mocha-previous-command nil)
(defvar mocha-previous-directory nil)
(defvar mocha-describe-regexp "describe[\s\t()]+'\\([a-z|A-Z|1-9|#_?!()]*\\)'[,\s\t]+")

(defun mocha-project-root-path (file)
  (or (f--traverse-upwards (f-exists? (f-expand mocha-project-root-specifier it))
                           (f-dirname file))
      (user-error "Could not find project root. Please make and retry.")))

(defun mocha-executable-path (file project-root)
  (or (executable-find "mocha")
      (executable-find (f-join project-root "node_modules" ".bin" "mocha"))
      (user-error "Could not find `mocha.js'. Please install and retry.")))

(defun* mocha-make-minimum-command (exec-path &optional (opt nil opt-supplied-p))
  (append (list exec-path (format "--reporter %s" mocha-reporter))
          opt))

(defun mocha-grep-option (target)
  (list "-g" (format "'%s'" target)))

(defun* mocha-make-command (file exec-path &optional (opt nil opt-supplied-p))
  (s-join " "
          (append (mocha-make-minimum-command exec-path) opt (list file))))

(defun mocha-cd-and-run-command (destination command)
  (message (concat destination "/"))
  (let ((default-directory (concat destination "/")))
    (compile command)))

;;;###autoload
(defun mocha-run-this-file ()
  (interactive)
  (lexical-let* ((file (f-this-file))
                 (project-root (mocha-project-root-path file))
                 (exec-path (mocha-executable-path file project-root))
                 (command (mocha-make-command file exec-path)))
    (setq mocha-previous-command command)
    (setq mocha-previous-directory (f-dirname file))
    (compile command)))

;;;###autoload
(defun mocha-run-at-point ()
  (interactive)
  (lexical-let* ((file (f-this-file))
                 (project-root (mocha-project-root-path file))
                 (exec-path (mocha-executable-path file project-root)))
    (save-excursion
      (end-of-line)
      (or (re-search-backward mocha-describe-regexp nil t)
          (user-error "Could not find spec before this point."))
      (lexical-let* ((target (match-string 1))
                     (command (mocha-make-command
                               file exec-path (mocha-grep-option target))))
        (setq mocha-previous-command command)
        (setq mocha-previous-directory (f-dirname file))
        (compile command)))))

;;;###autoload
(defun mocha-run-all-test ()
  (interactive)
  (lexical-let* ((file (f-this-file))
                 (project-root (mocha-project-root-path file))
                 (exec-path (mocha-executable-path file project-root))
                 (command (mocha-make-command "" exec-path)))
    (setq mocha-previous-command command)
    (setq mocha-previous-directory project-root)
    (mocha-cd-and-run-command project-root command)))

;;;###autoload
(defun mocha-run-previous-process ()
  (interactive)
  (lexical-let ((previous-dir mocha-previous-directory)
                (previous-command mocha-previous-command))
    (if (and previous-dir previous-command)
        (mocha-cd-and-run-command previous-dir previous-command)
      (user-error "Could not find previous process. Please run other command."))))

(provide 'mocha)
