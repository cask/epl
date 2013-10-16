;;; package-structures-test.el --- EPL: Tests for package structures  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: http://github.com/cask/epl

;; This file is NOT part of GNU Emacs.

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Keywords: convenience

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

;; Test the `epl-package' and related structures.

;;; Code:

(require 'epl)

(require 'ert)
(require 'pcase)

(ert-deftest epl-package-as-description-needs-symbol ()
  ;; We explicitly `eval' the expression here, to avoid eager macro expansion
  ;; kicking in, and triggering the error before the test gets executed
  (pcase-let* ((expr '(epl-package-as-description "foo"
                        (message "bar")))
               (`(,err . ,data) (should-error (eval expr)))
               (`(,predicate ,value) data))
    (should (eq err 'wrong-type-argument))
    (should (eq predicate #'symbolp))
    (should (equal value "foo"))))

(ert-deftest epl-package-as-description-needs-variable-bound-to-epl-package ()
  (pcase-let* ((foo "bar")
               (`(,err . ,data) (should-error (epl-package-as-description foo
                                                (message "bar"))))
               (`(,predicate ,value) data))
    (should (eq err 'wrong-type-argument))
    (should (eq predicate #'epl-package-p))
    (should (equal value "bar"))))

;;; package-structures-test.el ends here
