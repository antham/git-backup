;;; env.el --- Environment use with ecukes

;; Copyright (C) 2013-2019 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
;; URL: http://github.com/antham/git-backup

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

;;; Code:

(require 'f)

(defvar git-backup-support-path (f-dirname load-file-name))
(defvar git-backup-features-path (f-parent git-backup-support-path))
(defvar git-backup-root-path (f-parent git-backup-features-path))
(defvar git-backup-git-path "git")
(defvar git-backup-test-directory "/tmp/test")
(defvar git-backup-backup-repository "/tmp/git-backup-repository")

(add-to-list 'load-path git-backup-root-path)

(require 'git-backup)
(require 'espuds)
(require 'ert)

(defun backup-file ()
  (git-backup-version-file "git" git-backup-backup-repository () (buffer-file-name)))

(Setup
 (add-hook 'after-save-hook 'backup-file)
 (ignore-errors
   (delete-directory git-backup-test-directory t)
   (delete-directory git-backup-backup-repository t)))

(Before
 (ignore-errors
   (make-directory git-backup-test-directory t)))

;;; env.el ends here
