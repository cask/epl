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

(ert-deftest epl-package-from-file-lisp ()
  (let* ((file (epl-test-resource-file-name "dummy-package.el"))
         (package (epl-package-from-file file)))
    (should (epl-package-p package))
    (should (string= (epl-package-name package) 'dummy-package))
    (should (string= (epl-package-summary package)
                     "EPL: Dummy package for unit tests"))
    (should (equal (epl-package-version package) '(4 3 1 2 -3)))
    (should (equal (epl-package-requirements package)
                   (list (epl-requirement-create :name 'foo :version '(1 2))
                         (epl-requirement-create :name 'bar :version '(2 2)))))))

(ert-deftest epl-package-from-file-tar-no-package-descriptor ()
  "Test a TAR package without package descriptor."
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "dummy-package-4.3.1.2alpha.tar"))))

(ert-deftest epl-package-from-file-tar ()
  (let* ((file (epl-test-resource-file-name "dummy-package-4.3.2.tar"))
         (package (epl-package-from-file file)))
    (should (epl-package-p package))
    (should (string= (epl-package-name package) 'dummy-package))
    (should (string= (epl-package-summary package) "EPL dummy package"))
    (should (equal (epl-package-version package) '(4 3 2)))
    (should (equal (epl-package-requirements package)
                   (list (epl-requirement-create :name 'foo :version '(0 3))
                         (epl-requirement-create :name 'spam :version '(0 4)))))))

(ert-deftest epl-package-from-file-tar-non-existing ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "no-such-package.tar"))))

(ert-deftest epl-package-from-file-lisp-non-existing ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "no-such-package.el"))))

(ert-deftest epl-package-from-descriptor-file ()
  (let* ((file (epl-test-resource-file-name "dummy-package-pkg.el"))
         (package (epl-package-from-descriptor-file file)))
    ;; Make sure that loading a package descriptor has no side effects on the
    ;; database. package.el tends to have such unfortunate side effects.
    (should-not (assq 'dummy-package package-alist))
    (should (epl-package-p package))
    (should (string= (epl-package-name package) 'dummy-package))
    (should (string= (epl-package-summary package)
                     "EPL dummy package"))
    (should (equal (epl-package-version package) '(4 3 6)))
    (should (equal (epl-package-requirements package)
                   (list (epl-requirement-create :name 'bar :version '(8 1 -3))
                         (epl-requirement-create :name 'spam :version '(0 4)))))))

(ert-deftest epl-package-from-descriptor-file-nonexisting ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "no-such-descriptor-pkg.el"))))

(ert-deftest epl-package-from-descriptor-file-invalid ()
  (should-error
   (epl-package-from-file
    (epl-test-resource-file-name "invalid-package-pkg.el"))))

;;; package-structures-test.el ends here
