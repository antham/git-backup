;;; git-backup-test.el --- Test for git-backup

;; Copyright (C) 2019 Anthony HAMON

;; Author: Anthony HAMON
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

(require 'ert)
(require 'git-backup)

(defmacro test-wrapper (body)
  `(let ((git-binary-path "git")
         (git-output-format "%cd, %ar")
         (backup-folder-test (file-truename "/tmp/git-backup-test"))
         (backup-folder-test-repository (file-truename (concat (file-truename "/tmp/git-backup-test") "/git-backup"))))
     (unwind-protect
         (progn
           (ignore-errors
             (delete-directory backup-folder-test t)
             (make-directory backup-folder-test))
           (setq backup-path backup-folder-test-repository)
           (,body))
       (ignore-errors
         (delete-directory backup-folder-test t)))))

(ert-deftest git-backup-init-git-repository-test ()
  (test-wrapper
   (lambda ()
     ;; invoking command create a git repository
     (git-backup--init-git-repository git-binary-path backup-path)
     (should (eql (file-exists-p backup-folder-test-repository) t))
     (should (eql (file-exists-p (concat backup-folder-test-repository "/.git")) t))
     (delete-directory backup-folder-test-repository t)
     ;; if directory exists nothing is done
     (make-directory backup-folder-test-repository)
     (git-backup--init-git-repository git-binary-path backup-path)
     (should (eql (file-exists-p (concat backup-folder-test-repository "/.git")) nil)))))


(ert-deftest git-backup-transform-filename-for-git-test ()
  (test-wrapper
   (lambda ()
     ;; nil filename
     (should (eql (git-backup--transform-filename-for-git nil) nil))
     ;; relative filename
     (should (eql (git-backup--transform-filename-for-git "relative/path") nil))
     ;; absolute filename
     (should (equal-including-properties (git-backup--transform-filename-for-git "/absolute/path") "absolute/path")))))


(ert-deftest git-backup-exec-git-command-test ()
  (test-wrapper
   (lambda ()
     ;; we can do any git command in backup repository
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository)
     (write-region "" nil (concat backup-folder-test-repository "/test") nil 'nomessage)
     (git-backup--exec-git-command git-binary-path backup-folder-test-repository (list "add" "test"))
     (should (equal-including-properties (git-backup--exec-git-command git-binary-path backup-folder-test-repository (list "status" "-s")) "A  test\n"))
     (should (equal-including-properties (git-backup--exec-git-command git-binary-path backup-folder-test-repository (list "status" "-s") t) "A  test")))))

(ert-deftest git-backup-copy-file-to-repository-test ()
  (test-wrapper
   (lambda ()
     ;; copy a file to backup repository recreating tree
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository)
     (write-region "" nil (concat backup-folder-test "/fake-file") nil 'nomessage)
     (git-backup--copy-file-to-repository backup-folder-test-repository (concat backup-folder-test "/fake-file"))
     (should (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "/fake-file"))) t)))))

(ert-deftest git-backup-version-file-test ()
  (test-wrapper
   (lambda ()
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository nil nil)
     ;; version file
     (write-region "" nil (concat backup-folder-test "/fake-file") nil 'nomessage)
     (should (eql (git-backup-version-file git-binary-path backup-path () (concat backup-folder-test "/fake-file")) t))
     (message (concat backup-folder-test-repository (concat backup-folder-test "/fake-file")))
     (should (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "/fake-file"))) t))
     (should (equal-including-properties (shell-command-to-string (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "status" "-s"))) ""))
     ;; version file with relative path
     (write-region "" nil (concat backup-folder-test "/fake-file-1") nil 'nomessage)
     (should (eql (git-backup-version-file git-binary-path backup-path () (substring (concat backup-folder-test "/fake-file-1") 1)) nil))
     (should (eql (file-exists-p (concat backup-folder-test-repository "/fake-file")) nil))
     (should (equal-including-properties (shell-command-to-string (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "status" "-s"))) ""))
     ;; version non existing file
     (should (eql (git-backup-version-file git-binary-path backup-path () (concat backup-folder-test "/fake-fake-fake-fake")) nil))
     (should-not (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "fake-fake-fake-fake"))) t))
     ;; version crap
     (should (eql (git-backup-version-file git-binary-path backup-path () (concat backup-folder-test "/fake-fake-fake")) nil))
     (should (eql (file-exists-p (concat backup-folder-test-repository (concat backup-folder-test "/fake-fake-fake-fake"))) nil)))))

(ert-deftest git-backup-list-file-change-time-test ()
  (test-wrapper
   (lambda ()
     ;; nil value
     (should (eq (git-backup-list-file-change-time git-binary-path backup-path git-output-format nil) nil))
     ;; non existing repository
     (should (eq (git-backup-list-file-change-time git-binary-path backup-path git-output-format "/fake-file") nil))
     ;; add several modifications to a file
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository)
     ;; non existing file in repository
     (should (eq (git-backup-list-file-change-time git-binary-path backup-path git-output-format "/fake-file") nil))
     ;; add a file, change and version it several time
     (write-region "" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "add" "fake-file" "&&" git-binary-path "commit" "-m" "' '")))
     (write-region "data" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "add" "fake-file" "&&" git-binary-path "commit" "-m" "' '")))
     (write-region "data data" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "add" "fake-file" "&&" git-binary-path "commit" "-m" "' '")))
     (should (eq (safe-length (git-backup-list-file-change-time git-binary-path backup-path git-output-format "/fake-file")) 3))
     (dolist (row (git-backup-list-file-change-time git-binary-path backup-path git-output-format "/fake-file"))
       (should (eq (string-match "[0-9a-f]+" (cdr row)) 0))))))

(ert-deftest git-backup-fetch-backup-file-test ()
  (test-wrapper
   (lambda ()
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository)
     (write-region "data" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "add" "fake-file" "&&" git-binary-path "commit" "-m" "' '")))
     (let ((commit-id (car (split-string (shell-command-to-string (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "log" "-1" "--oneline"))) " "))))
       ;; nil value
       (should (eql (git-backup--fetch-backup-file git-binary-path backup-path nil nil) nil))
       ;; wrong id
       (should (eql (git-backup--fetch-backup-file git-binary-path backup-path "9090909" "/fake-file") nil))
       ;; wrong file
       (should (eql (git-backup--fetch-backup-file git-binary-path backup-path commit-id "/non-existing-file") nil))
       ;; existing commit and file
       (should (equal-including-properties (git-backup--fetch-backup-file git-binary-path backup-path commit-id "/fake-file") "data"))))))

(ert-deftest git-backup-file-excluded-p-test ()
  (test-wrapper
   (lambda ()
     (let ((excluded-entry-paths (list "/password" "/password\[0-9\]+""/root/.*" ".*\.text" ".*/pgp/.*")))
       ;; excluded file
       (should (git-backup--file-excluded-p excluded-entry-paths "/password"))
       (should (git-backup--file-excluded-p excluded-entry-paths "/password1"))
       ;; file inside excluded folder
       (should (git-backup--file-excluded-p excluded-entry-paths "/root/file"))
       ;; file at the second level in excluded folder
       (should (git-backup--file-excluded-p excluded-entry-paths "/root/folder/file2"))
       ;; file inside global excluded folder
       (should (git-backup--file-excluded-p excluded-entry-paths "/home/user/pgp/key"))
       (should (git-backup--file-excluded-p excluded-entry-paths "/home/admin/pgp/key"))
       (should (git-backup--file-excluded-p excluded-entry-paths "/pgp/key"))
       ;; excluded extension
       (should (git-backup--file-excluded-p excluded-entry-paths "/file.text"))
       (should (git-backup--file-excluded-p excluded-entry-paths "/home/user/file.text"))
       (should (git-backup--file-excluded-p excluded-entry-paths "/home/user/file.text"))
       ;; allowed file
       (should-not (git-backup--file-excluded-p excluded-entry-paths "/file"))
       (should-not (git-backup--file-excluded-p excluded-entry-paths "/password-public"))
       ;; allowed folder
       (should-not (git-backup--file-excluded-p excluded-entry-paths "/home/user/file"))
       ;; allowed extension
       (should-not (git-backup--file-excluded-p excluded-entry-paths "/home/user/file.el"))))))

(ert-deftest git-backup-create-backup-buffer ()
  (test-wrapper
   (lambda ()
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository)
     (write-region "data" nil (concat backup-folder-test-repository "/fake-file") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "add" "fake-file" "&&" git-binary-path "commit" "-m" "' '")))

     (let* ((commit-id (car (split-string (shell-command-to-string (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "log" "-1" "--oneline"))) " ")))
            (buffer (git-backup--create-backup-buffer git-binary-path backup-folder-test-repository commit-id "/fake-file"))
            (data nil))
       ;; nil value
       (should (eql (git-backup--create-backup-buffer git-binary-path backup-folder-test-repository nil nil) nil))
       ;; wrong id
       (should (eql (git-backup--create-backup-buffer git-binary-path backup-folder-test-repository "9090909" "/fake-file") nil))
       ;; wrong file
       (should (eql (git-backup--create-backup-buffer git-binary-path backup-folder-test-repository commit-id "/non-existing-file") nil))
       ;; existing commit and file
       (with-current-buffer buffer
         (should (equal-including-properties (buffer-substring (point-min) (point-max)) "data")))))))

(ert-deftest git-backup-remove-file-backups ()
  (test-wrapper
   (lambda ()
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository nil nil)
     ;; version file three times and delete it from repository
     (write-region "" nil (concat backup-folder-test "/test") nil 'nomessage)
     (git-backup-version-file git-binary-path backup-path () (concat backup-folder-test "/test"))
     (write-region "" nil (concat backup-folder-test "/fake-file") nil 'nomessage)
     (git-backup-version-file git-binary-path backup-path () (concat backup-folder-test "/fake-file"))
     (write-region "test" nil (concat backup-folder-test "/fake-file") nil 'nomessage)
     (git-backup-version-file git-binary-path backup-path () (concat backup-folder-test "/fake-file"))
     (write-region "test2" nil (concat backup-folder-test "/fake-file") nil 'nomessage)
     (git-backup-version-file git-binary-path backup-path () (concat backup-folder-test "/fake-file"))
     (should (equal-including-properties (length (split-string (git-backup--exec-git-command git-binary-path backup-path (list "log" "--oneline") t) "\n")) 4))
     (should (equal-including-properties  (file-exists-p (concat backup-folder-test-repository backup-folder-test "/fake-file")) t))
     (git-backup-remove-file-backups git-binary-path backup-path (concat backup-folder-test "/fake-file"))
     ;; commits related to file and the file are removed from backup folder
     (should (equal-including-properties (length (split-string (git-backup--exec-git-command git-binary-path backup-path (list "log" "--oneline") t) "\n")) 1))
     (should (equal-including-properties  (file-exists-p (concat backup-folder-test-repository backup-folder-test "/fake-file")) nil)))))

(ert-deftest git-backup-list-file-with-special-characters-in-filename-test ()
  (test-wrapper
   (lambda ()
     (call-process-shell-command git-binary-path nil nil nil "init" backup-folder-test-repository)
     (write-region "" nil (concat backup-folder-test-repository "/ôëàèéü") nil 'nomessage)
     (shell-command (combine-and-quote-strings (list "cd" backup-folder-test-repository "&&" git-binary-path "add" "ôëàèéü" "&&" git-binary-path "commit" "-m" "' '")))
     (should (eq (safe-length (git-backup-list-file-change-time git-binary-path backup-path git-output-format "/ôëàèéü")) 1)))))
