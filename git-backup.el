;;; git-backup.el --- Backup each file change using git -*- lexical-binding: t -*-

;; Copyright (C) 2013-2019 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
;; URL: http://github.com/antham/git-backup
;; Version: 1.0.0
;; Package-Requires: ((s "1.8.0") (cl-lib "0"))
;; Keywords: backup, files, tools, git

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

;; Library to provide primitive to implement a backup system on top of git

;;; Code:

(require 'cl-lib)
(require 's)

(defun git-backup-version-file (git-backup-git-binary git-backup-path git-backup-excluded-entries filename)
  "Version file using FILENAME in backup repository."
  (when (and filename
             (file-name-absolute-p filename)
             (file-exists-p filename)
             (not (git-backup--file-excluded-p git-backup-excluded-entries filename)))
    (git-backup--init-git-repository git-backup-git-binary git-backup-path)
    (git-backup--copy-file-to-repository git-backup-path filename)
    (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "add" (git-backup--transform-filename-for-git filename)) t)
    (git-backup--exec-git-command git-backup-git-binary git-backup-path '("commit" "-m" "backup") t)
    t))

(defun git-backup-clean-repository ()
  "Clean repository running gc."
  (git-backup--exec-git-command (list "gc") t))

(defun git-backup-remove-file-backups (git-backup-git-binary git-backup-path filename)
  "Remove all history associated with FILENAME."
  (git-backup--remove-file-history git-backup-git-binary git-backup-path filename)
  (git-backup--remove-file git-backup-path filename))

(defun git-backup--init-git-repository (git-backup-git-binary git-backup-path)
  "Initialize git repository."
  (unless (file-directory-p
           git-backup-path)
    (call-process git-backup-git-binary nil nil nil "init" git-backup-path)
    (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "config" "--local" "user.email" "noemail@noemail.com"))
    (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "config" "--local" "user.name" "noname"))))

(defun git-backup--exec-git-command (git-backup-git-binary git-backup-path command &optional strip-last-newline)
  "Execute a git COMMAND inside backup repository, optionally STRIP-LAST-NEWLINE."
  (when (file-directory-p (expand-file-name ".git" git-backup-path))
    (let* ((default-directory git-backup-path)
           (output (shell-command-to-string (combine-and-quote-strings (append (list git-backup-git-binary) command)))))
      (if strip-last-newline
          (s-chomp output)
          output))))

(defun git-backup--transform-filename-for-git (filename)
  "Transform FILENAME to be used in git repository."
  (when (and filename
             (file-name-absolute-p filename))
    (substring filename 1)))

(defun git-backup--copy-file-to-repository (git-backup-path filename)
  "Create folder in repository and copy file using FILENAME in it."
  (let ((directory (concat git-backup-path (file-name-directory filename))))
    (make-directory directory t)
    (copy-file filename directory t t t)))

(defun git-backup--remove-file (git-backup-path filename)
  "Remove FILENAME from repository."
  (let ((f (concat git-backup-path filename)))
    (when (file-exists-p f)
      (delete-file f))))

(defun git-backup--file-excluded-p (git-backup-excluded-entries filename)
  "Check if a FILENAME is excluded from backup."
  (cl-some (lambda (regexp) (string-match-p (concat "\\`" regexp "\\'") filename))
           git-backup-excluded-entries))

(defun git-backup--list-file-change-time (git-backup-git-binary git-backup-path git-backup-list-format filename)
  "Build assoc list using commit id and message rendering format using FILENAME."
  (let ((filename-for-git (git-backup--transform-filename-for-git filename)))
    (when (and filename
               (string= (s-chop-suffixes '("\0") (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "ls-files" "-z" filename-for-git) t))
                        filename-for-git) t)
      (cl-mapcar #'cons
                 (split-string (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "log" (format
                                                                          "--pretty=format:%s"
                                                                          git-backup-list-format)
                                                                   filename-for-git) t) "\n")
                 (split-string (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "log" "--pretty=format:%h"
                                                                   filename-for-git) t) "\n")))))

(defun git-backup--fetch-backup-file (git-backup-git-binary git-backup-path commit-id filename)
  "Retrieve content file from backup repository using COMMIT-ID and FILENAME."
  (let ((filename-for-git (git-backup--transform-filename-for-git filename)))
    (when (and commit-id
               filename
               (not (string= (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "log" "--ignore-missing" "-1"
                                                                 commit-id "--" filename-for-git) t)
                             "")))
      (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "show" (concat commit-id ":" filename-for-git))))))

(defun git-backup--create-backup-buffer (git-backup-git-binary git-backup-path commit-id filename)
  "Create a buffer using chosen backup using COMMIT-ID and FILENAME."
  (let ((data (git-backup--fetch-backup-file git-backup-git-binary git-backup-path commit-id filename)))
    (when data
      (let ((buffer (get-buffer-create (concat filename " | " (git-backup--exec-git-command git-backup-git-binary git-backup-path (list
                                                                                             "diff-tree"
                                                                                             "-s"
                                                                                             "--pretty=format:%cd"
                                                                                             commit-id)
                                                                                            t))))
            (mode major-mode))
        (with-current-buffer buffer
          (erase-buffer)
          (insert data)
          (funcall mode)
          (set-buffer-modified-p nil)
          buffer)))))

(defun git-backup--remove-file-history (git-backup-git-binary git-backup-path filename)
  "Remove commits history for FILENAME."
  (git-backup--exec-git-command git-backup-git-binary git-backup-path (list "filter-branch"
                                      "--force"
                                      "--index-filter"
                                      "'"
                                      "git"
                                      "rm"
                                      "--cached"
                                      "--ignore-unmatch"
                                      (s-chop-prefix "/" filename)
                                      "'"
                                      "--prune-empty"
                                      "--tag-name-filter"
                                      "cat"
                                      "--"
                                      "--all"))t)

(defun git-backup--open-in-new-buffer (git-backup--create-backup-buffer commit-id filename)
  "Open backup in new buffer using COMMIT-ID and FILENAME."
  (let ((backup-buffer (git-backup--create-backup-buffer git-backup-git-binary backup-folder-test-repository commit-id filename)))
    (switch-to-buffer backup-buffer)))

(defun git-backup--replace-current-buffer (commit-id filename)
  "Replace current buffer with backup using COMMIT-ID and FILENAME."
  (erase-buffer)
  (insert (git-backup--fetch-backup-file commit-id filename)))

(defun git-backup--create-ediff (commit-id buffer)
  "Create a ediff buffer with backup using COMMIT-ID and existing BUFFER."
  (let ((backup-buffer (git-backup--create-backup-buffer git-backup-git-binary backup-folder-test-repository commit-id (buffer-file-name buffer))))
    (ediff-buffers (buffer-name backup-buffer)
                   (buffer-name buffer))))

(provide 'git-backup)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; git-backup.el ends here
