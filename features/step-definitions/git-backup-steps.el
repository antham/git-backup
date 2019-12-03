;;; git-backup-steps.el --- Steps used with ecukes

;; Copyright (C) 2019 Anthony HAMON

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

(Given "^I open file \"\\(.+\\)\"$" "Open given filename"
       (lambda (filename)
         (find-file filename)))

(Then "^I insert \"\\(.+\\)\" and save" "Insert given content and save"
      (lambda (data)
        (insert data)
        (save-buffer)))

(Then "^I should be in buffer name matching regexp \"\\(.+\\)\"$" "Match REGEXP against buffer-name"
      (lambda (expected)
        (let ((message "Expected to be in buffer '%s', but was in '%s'"))
          (should (string-match expected (buffer-name))))))

(When "^I run \"\\(.+\\)\"$"
      "Run an emacs command"
      (lambda (command)
        (execute-extended-command command)))

(When "^I open backup number \"\\(.+\\)\""
      (lambda (number)
        (let ((item (git-backup-list-file-change-time git-backup-git-path git-backup-backup-repository "%H" (buffer-file-name))))
          (git-backup-open-in-new-buffer git-backup-git-path git-backup-backup-repository (cdr (car (nthcdr (- (string-to-number number) 1) item))) (buffer-file-name))
        )))

(When "^I replace current buffer with backup number \"\\(.+\\)\""
      (lambda (number)
        (let ((item (git-backup-list-file-change-time git-backup-git-path git-backup-backup-repository "%H" (buffer-file-name))))
          (git-backup-replace-current-buffer git-backup-git-path git-backup-backup-repository (cdr (car (nthcdr (- (string-to-number number) 1) item))) (buffer-file-name))
          )))


(provide 'git-backup-steps)

;;; git-backup-steps.el ends here

