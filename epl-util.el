;;; epl-util.el --- Emacs Package Library: Utilities  -*- lexical-binding: t; -*-

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

;; Utilities for EPL, the Emacs Package Library.

;;; Code:

;; Declare error symbols
(put 'epl-error 'error-conditions '(error epl-error))
(put 'epl-error 'error-message "EPL Error")

(defun epl-error (string &rest args)
  "Signal an EPL error.

Signal an error with `epl-error' type.

STRING is a `format' string, and ARGS are the formatted objects.

This function does not return."
  (signal 'epl-error (list (apply #'format string args))))

(provide 'epl-util)

;;; epl-util.el ends here
