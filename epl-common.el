;;; epl-common.el --- Emacs Package Library: Common functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Keywords: convenience
;; URL: http://github.com/cask/epl

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Common functions of EPL, the Emacs Package Library.

;; The functions in this library are independent from the specific backend.

;;; Code:

(eval-and-compile
  (unless (require 'package nil :no-error)
    ;; Load the legacy package.el from Emacs 23.  We don't need to specify the
    ;; full path here, because the package directory is conveniently added to
    ;; `load-path'
    (load "package-legacy")))


;;;; Package.el cruft

(setq package-archives nil)             ; Clear the default list of archives to
                                        ; let the user have exact control over
                                        ; all archives


;;;; Package objects

(defun epl-requirement-version-string (requirement)
  "The version of a REQUIREMENT, as string."
  (package-version-join (epl-requirement-version requirement)))

(defun epl-package-version-string (package)
  "Get the version from a PACKAGE, as string."
  (package-version-join (epl-package-version package)))

(defun epl-package-installed-p (package)
  "Determine whether a PACKAGE is installed.

PACKAGE is either a package name as symbol, or a package object."
  (let ((name (if (epl-package-p package)
                  (epl-package-name package)
                package))
        (version (when (epl-package-p package)
                   (epl-package-version package))))
    (package-installed-p name version)))

(defun epl-package-from-file (file-name)
  "Parse the package headers the file at FILE-NAME.

Return an `epl-package' object with the header metadata."
  (with-temp-buffer
    (insert-file-contents file-name)
    (epl-package-from-buffer (current-buffer))))


;;;; Package directory
(defun epl-package-dir ()
  "Get the directory of packages."
  package-user-dir)

(defun epl-default-package-dir ()
  "Get the default directory of packages."
  (eval (car (get 'package-user-dir 'standard-value))))

(defun epl-change-package-dir (directory)
  "Change the directory of packages to DIRECTORY."
  (setq package-user-dir directory)
  (epl-initialize))


;;;; Package system management

(defvar epl--load-path-before-initialize nil
  "Remember the load path for `epl-reset'.")

(defun epl-initialize (&optional no-activate)
  "Load Emacs Lisp packages and activate them.

With NO-ACTIVATE non-nil, do not activate packages."
  (setq epl--load-path-before-initialize load-path)
  (package-initialize no-activate))

(defalias 'epl-refresh 'package-refresh-contents)

(defun epl-add-archive (name url)
  "Add a package archive with NAME and URL."
  (add-to-list 'package-archives (cons name url)))

(provide 'epl-common)

;;; epl-common.el ends here
