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

;;; TODO:
;; + display compiled buffer clearly
;; + add comment
;; + add test
;; + make function which search json file and toggle jump src/test file

;;; Code:
(require 'f)
(require 's)
(require 'dash)
(require 'json)

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
(defvar mocha-suffix-candidates '("-test" "_test" "-spec" "_spec"))
(defvar mocha-project-structure nil)
(defvar mocha-project-structure-save-path "~/.emacs.d")

(defun mocha-project-root-path (file)
  (or (f--traverse-upwards (f-exists? (f-expand mocha-project-root-specifier it))
                           (f-dirname file))
      (user-error "Could not find project root. Please make it and retry.")))

(defun mocha-executable-path (file project-root)
  (or (executable-find "mocha")
      (executable-find (f-join project-root "node_modules" ".bin" "mocha"))
      (user-error "Could not find `mocha.js'. Please install and retry.")))

;; TODO: add option `--recursive' if needed
(defun* mocha-make-minimum-command (exec-path &optional (opt nil opt-supplied-p))
  (append (list exec-path (format "--reporter %s" mocha-reporter))
          opt))

(defun mocha-grep-option (target)
  (list "-g" (format "'%s'" target)))

(defun* mocha-make-command (file exec-path &optional (opt nil opt-supplied-p))
  (s-join " "
          (append (mocha-make-minimum-command exec-path) opt (list file))))

(defun mocha-cd-and-run-command (destination command)
  (let ((default-directory (f-slash destination)))
    (compile command)))

(defun mocha-test-file? (file)
  (lexical-let ((path (f-dirname file)))
    (or (s-contains? "test" path)
        (s-contains? "spec" path))))

(defun mocha-add-suffix (src-file)
  (cons src-file
        (loop for suffix in mocha-suffix-candidates
              collect (format "%s%s.js" (f-no-ext src-file) suffix))))

(defun mocha-minimize-path (src-file)
  (if (= 1 (length (f-split src-file)))
      (user-error "the depth of path is too shallow.")
    (f-relative src-file (f-parent (f-dirname src-file)))))

(defun mocha-remove-suffix (test-file)
  (replace-regexp-in-string "\\(-test\\|_test\\|-spec\\|_spec\\).js"
                            ".js" test-file))

(defun mocha--collect-entries (path recursive)
  (let (result
        (entries
         (-reject
          (lambda (file) (or (equal (f-filename file) ".")
                             (equal (f-filename file) "..")
                             (equal (f-filename file) "node_modules")
                             (equal (f-filename file) "bin")
                             (equal (f-filename file) "db")
                             (equal (f-filename file) "log")
                             (equal (f-filename file) "vendor")))
          (directory-files path t))))
    (cond (recursive
           (-map
            (lambda (entry) (if (f-file? entry)
                                (setq result (cons entry result))
                              (when (f-directory? entry)
                                (setq result (cons entry result))
                                (setq result (append result
                                                     (f--collect-entries entry recursive))))))
            entries))
          (t (setq result entries)))
    result))

(defun mocha-toggle-jump (file)
  (if (mocha-test-file? file)
      (mocha-find-spec-file-of file)
    (mocha-find-test-file-of file)))

(defun mocha-find-test-file-under-test-dir (project-root candidates)
  (cl-loop for candidate in candidates
           for target = (f-join project-root "test" candidate)
           if (f-exists? target) return target))

(defun mocha-find-pair-test-file (src-file project-root)
  (lexical-let* ((mini-path (mocha-minimize-path src-file))
                 (mini-path-candidates (mocha-add-suffix mini-path))
                 (flatten-canndidates (mocha-add-suffix (f-filename src-file))))
    (or (mocha-find-test-file-under-test-dir project-root
                                             mini-path-candidates)
        (mocha-find-test-file-under-test-dir project-root
                                             flatten-canndidates))))

(defun mocha-find-pair-src-file (test-file project-root)
  (lexical-let* ((mini-path (mocha-minimize-path test-file))
                 (mini-path-target (mocha-remove-suffix mini-path))
                 (filename (f-filename test-file))
                 (entries (mocha--collect-entries project-root t)))
    (-find (lambda (entry) (or (s-contains? filename entry)
                               (s-contains? mini-path-target entry)))
                   entries)))

(defun mocha-append-hash-table (project-root)
  (lexical-let ((json-object-type 'hash-table)
                (all-project-structure (make-hash-table)))
    (-if-let (all-project-structure
              (gethash project-root (json-read-file mocha-project-structure-save-path)))
        (setf (gethash project-root mocha-project-structure) all-project-structure))))

(defun mocha-save-src-test-pairs (project-root)
  (lexical-let ((json-object-type 'hash-table)
                (all-project-structure (make-hash-table)))
    (setf (gethash project-root all-project-structure) mocha-project-structure)
    ))

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
