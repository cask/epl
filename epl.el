;;; epl.el --- Emacs Package Library -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
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

;; A package management library for Emacs, based on package.el.

;; The purpose of this library is to wrap all the quirks and hassle of
;; package.el into a sane API.

;; The following functions comprise the public interface of this library:

;; `epl-package-p' determines whether an object is a package object.

;; `epl-package-name' gets the package name from a package object.

;; `epl-package-summary' gets the package summary from a package object.

;; `epl-package-version' gets the package version from a package object.

;; `epl-package-version-string' gets the package version as string from a
;; package object.

;; `epl-package-requirements' gets the requirements of a package.

;; `epl-package-installed-p' determines whether a package is installed.

;; `epl-initialize' initializes the package system and activates all
;; packages.

;; `epl-reset' resets the package system.

;; `epl-refresh' refreshes all package archives.

;; `epl-package-dir' gets the directory of packages.

;; `epl-default-package-dir' gets the default package directory.

;; `epl-change-package-dir' changes the directory of packages.

;; `epl-add-archive' adds a new package archive.

;; `epl-installed-packages' and `epl-available-packages' get all packages
;; installed and available for installed respectively.

;; `epl-find-installed-package' and `epl-find-available-packages' find installed
;; and available packages by name.

;; `epl-find-upgrades' finds all upgradable packages.

;; `epl-package-install' installs a package.

;; `epl-package-delete' deletes a package.

;; `epl-upgrade' upgrades packages.

;; This version of the EPL library requires a recent package.el which provides
;; the `package-desc' struct.  Loading it with a legacy package.el signals an
;; error.

;;; Code:

(require 'epl-util)


;;;; Load implementation

(eval-and-compile
  (condition-case nil
      (require 'epl-package-desc)
    (error
     (require 'epl-legacy))))

;; The following function definitions are independent from the concrete
;; implementation, and hence defined in the top-level library.


;;;; Package database access
(defun epl-find-upgrades (&optional packages)
  "Find all upgradable PACKAGES.

PACKAGES is a list of package objects to upgrade, defaulting to
all installed packages.

Return a list of `epl-upgrade' objects describing all upgradable
packages."
  (let ((packages (or packages (epl-installed-packages)))
        upgrades)
    (dolist (pkg packages)
      (let* ((version (epl-package-version pkg))
             (name (epl-package-name pkg))
             ;; Find the latest available package for NAME
             (available-pkg (car (epl-find-available-packages name)))
             (available-version (when available-pkg
                                  (epl-package-version available-pkg))))
        (when (and available-version (version-list-< version available-version))
          (push (epl-upgrade-create :installed pkg
                                    :available available-pkg)
                upgrades))))
    (nreverse upgrades)))


;;;; Package operations

(defun epl-upgrade (&optional packages preserve-obsolete)
  "Upgrade PACKAGES.

PACKAGES is a list of package objects to upgrade, defaulting to
all installed packages.

The old versions of the updated packages are deleted, unless
PRESERVE-OBSOLETE is non-nil.

Return a list of all performed upgrades, as a list of
`epl-upgrade' objects."
  (let ((upgrades (epl-find-upgrades packages)))
    (dolist (upgrade upgrades)
      (epl-package-install (epl-upgrade-available upgrade))
      (unless preserve-obsolete
        (epl-package-delete (epl-upgrade-installed upgrade))))
    upgrades))

(provide 'epl)

;;; epl.el ends here
