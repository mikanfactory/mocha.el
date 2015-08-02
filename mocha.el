;;; mocha.el --- Run mocha quickly -*- lexical-binding: t; -*-y

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

(provide 'mocha)
