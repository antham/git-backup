;;; helm-backup.el --- Backup each file change using git

;; Copyright (C) 2013 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
;; URL: http://github.com/antham/helm-backup
;; Version: 0.2.0
;; Package-Requires: ((helm "1.5.5") (s "1.8.0"))
;; Keywords: backup

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

;; To store change every time you save a file add :
;; (add-hook 'after-save-hook 'helm-backup-versioning)
;; or from Emacs you can do :
;; M-x customize-variable RET after-save-hook RET [INS] helm-backup-versioning

;; To retrieve file backup, from buffer call `helm-backup' :
;; M-x helm-backup
;; for convenience you can define key binding as follow :
;; (global-set-key (kbd "C-c b") 'helm-backup)

;;; Code:

(require 'helm)
(require 'helm-utils)
(require 'cl-lib)
(require 's)

(defgroup helm-backup nil
  "Backup system using git and helm."
  :group 'helm)

(defcustom helm-backup-path "~/.helm-backup"
  "Backup location."
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-git-binary "/usr/bin/git"
  "Git binary path."
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-list-format "%cd, %ar"
  "Format use to display entries in helm buffer, follow git log format."
  :group 'helm-backup
  :type 'string)

(defcustom helm-backup-excluded-entries nil
  "Define a list of file/folder regexp to exclude from backup.
/home/user/password => exclude password in /home/user
.*\\.el$ => exclude .el extension
/root/.* => exclude everything inside root
.*/password/.* => exclude all folders with name 'password'"
  :group 'helm-backup
  :type '(repeat regexp))

(defun helm-backup-init-git-repository ()
  "Initialize git repository."
  (unless (file-directory-p helm-backup-path)
    (call-process helm-backup-git-binary nil nil nil "init" helm-backup-path)
    (helm-backup-exec-git-command (list "config" "--local" "user.email" "noemail@noemail.com"))
    (helm-backup-exec-git-command (list "config" "--local" "user.name" "noname"))))

(defun helm-backup-exec-git-command (command &optional strip-last-newline)
  "Execute a git COMMAND inside backup repository, optionally STRIP-LAST-NEWLINE."
  (when (file-directory-p (concat helm-backup-path "/.git"))
    (let ((output (shell-command-to-string (combine-and-quote-strings (append (list "cd"
                                                                                    helm-backup-path
                                                                                    "&&" "git")
                                                                              command)))))
      (if strip-last-newline
          (s-chomp output)
        output))))

(defun helm-backup-transform-filename-for-git (filename)
  "Transform FILENAME to be used in git repository."
  (when (and filename
             (helm-backup-is-absolute-filename filename))
    (substring filename 1)))

(defun helm-backup-is-absolute-filename (filename)
  "Check if a FILENAME is absolute or not."
  (when (and filename
             (string= (substring filename 0 1) "/"))
    t))

(defun helm-backup-copy-file-to-repository (filename)
  "Create folder in repository and copy file using FILENAME in it."
  (let ((directory (concat helm-backup-path (file-name-directory filename))))
    (make-directory directory t)
    (copy-file filename directory t t t)))

(defun helm-backup-is-excluded-filename (filename)
  "Check if a FILENAME is excluded from backup."
  (when filename
    (cl-dolist (regexp helm-backup-excluded-entries)
      (let ((index (string-match (concat "^" regexp "$") filename)))
        (when (and (integerp index)
                   (zerop index))
          (cl-return
           t))))))

(defun helm-backup-version-file (filename)
  "Version file using FILENAME in backup repository."
  (when (and filename
             (helm-backup-is-absolute-filename filename)
             (file-exists-p filename)
             (not (helm-backup-is-excluded-filename filename)))
    (helm-backup-init-git-repository)
    (helm-backup-copy-file-to-repository filename)
    (helm-backup-exec-git-command (list "add" (helm-backup-transform-filename-for-git filename)) t)
    (helm-backup-exec-git-command '("commit" "-m" "backup") t)
    t))

(defun helm-backup-list-file-change-time (filename)
  "Build assoc list using commit id and message rendering format using FILENAME."
  (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
    (when (and filename
               (string= (helm-backup-exec-git-command (list "ls-files" filename-for-git) t)
                        filename-for-git)
               t)
      (cl-mapcar #'cons
                 (split-string (helm-backup-exec-git-command (list "log" (format
                                                                          "--pretty=format:%s"
                                                                          helm-backup-list-format)
                                                                   filename-for-git) t) "\n")
                 (split-string (helm-backup-exec-git-command (list "log" "--pretty=format:%h"
                                                                   filename-for-git) t) "\n")))))

(defun helm-backup-fetch-backup-file (commit-id filename)
  "Retrieve content file from backup repository using COMMIT-ID and FILENAME."
  (let ((filename-for-git (helm-backup-transform-filename-for-git filename)))
    (when (and commit-id
               filename
               (not (string= (helm-backup-exec-git-command (list "log" "--ignore-missing" "-1"
                                                                 commit-id "--" filename-for-git) t)
                             "")))
      (helm-backup-exec-git-command (list "show" (concat commit-id ":" filename-for-git))))))

(defun helm-backup-create-backup-buffer (commit-id filename)
  "Create a buffer using chosen backup using COMMIT-ID and FILENAME."
  (let ((data (helm-backup-fetch-backup-file commit-id filename)))
    (when data
      (let ((buffer (get-buffer-create (concat filename " | " (helm-backup-exec-git-command (list
                                                                                             "diff-tree"
                                                                                             "-s"
                                                                                             "--pretty=format:%cd"
                                                                                             commit-id)
                                                                                            t))))
            (mode
             (with-current-buffer (current-buffer)
               major-mode)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert data)
          (funcall mode)
          buffer)))))

(defun helm-backup-clean-repository ()
  "Clean repository running gc."
  (helm-backup-exec-git-command (list "gc") t))

;;;###autoload
(defun helm-backup-versioning ()
  "Helper to add easily versionning."
  (helm-backup-version-file (buffer-file-name)))

(defun helm-backup-open-in-new-buffer (commit-id filename)
  "Open backup in new buffer using COMMIT-ID and FILENAME."
  (let ((backup-buffer (helm-backup-create-backup-buffer commit-id filename)))
    (switch-to-buffer backup-buffer)))

(defun helm-backup-replace-current-buffer (commit-id filename)
  "Replace current buffer with backup using COMMIT-ID and FILENAME."
  (erase-buffer)
  (insert (helm-backup-fetch-backup-file commit-id filename)))

(defun helm-backup-create-ediff (commit-id buffer)
  "Create a ediff buffer with backup using COMMIT-ID and existing BUFFER."
  (let ((backup-buffer (helm-backup-create-backup-buffer commit-id (buffer-file-name buffer))))
    (ediff-buffers (buffer-name backup-buffer)
                   (buffer-name buffer))))

(defun helm-backup-source ()
  "Source used to populate buffer."
  `((name .
          ,((lambda
              ()
              (format "Backup for %s" (buffer-file-name)))))
    (candidates .
                ,((lambda
                    ()
                    (helm-backup-list-file-change-time (buffer-file-name)))))
    (action ("Ediff file with backup" .
             (lambda (candidate)
               (helm-backup-create-ediff candidate (current-buffer))))
            ("Open in new buffer" .
             (lambda (candidate)
               (helm-backup-open-in-new-buffer candidate (buffer-file-name))))
            ("Replace current buffer" .
             (lambda (candidate)
               (with-helm-current-buffer
                 (helm-backup-replace-current-buffer candidate (buffer-file-name))))))))

;;;###autoload
(defun helm-backup ()
  "Main function used to call `helm-backup`."
  (interactive)
  (let ((helm-quit-if-no-candidate
         (lambda ()
           (error
            "No filename associated with buffer, file has no backup yet or filename is blacklisted"))))
    (helm-other-buffer (helm-backup-source) "*Helm Backup*")))

(eval-after-load "helm-backup"
  '(progn
     (helm-backup-clean-repository)))

(provide 'helm-backup)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-backup.el ends here
