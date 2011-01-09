;;; org-magit.el --- basic support for magit links

;; Copyright (C) 2011  Yann Hodique.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'org)
(require 'magit)

(defun org-magit-split-string (str)
  (split-string str "::"))

(defun org-magit-open-log ()
  (magit-display-log)
  (current-buffer))

(defun org-magit-open-commit (commit)
  (magit-show-commit commit)
  (get-buffer magit-commit-buffer-name))

(defun org-magit-open (str)
  (let* ((strlist (org-magit-split-string str))
         (repo (first strlist))
         (view (second strlist))
         (func nil)
         (args nil))
    (cond ((string-match "^status" view)
           (setq func 'current-buffer))
          ((string-match "^log" view)
           (setq func 'org-magit-open-log))
          ((string-match "^commit@\\(.*\\)" view)
           (setq func 'org-magit-open-commit
                 args (list (match-string 1 view)))))

    (when func
      (pop-to-buffer
       (save-window-excursion
         (magit-status repo)
         (apply func args))))))

(defun org-magit-make-link (repo &rest components)
  (apply 'org-make-link "magit:" repo components))

(defun org-magit-clean-repository (repo)
  (let ((name (file-name-nondirectory (directory-file-name repo))))
    (when (not (string-match "\\.git" name))
      (setq name (concat name ".git")))
    name))

(defun org-magit-store-link ()
  (when (eq major-mode 'magit-mode)
    (let* ((repo default-directory)
           (link nil)
           (description (org-magit-clean-repository repo)))
      (cond (magit-status-mode
             (setq link (org-magit-make-link repo "::status")
                   description (format "%s status" description)))
            (magit-log-mode
             (setq link (org-magit-make-link repo "::log")
                   description (format "%s log" description)))
            (magit-commit-mode
             (setq link (org-magit-make-link repo "::commit@" magit-currently-shown-commit)
                   description (format "%s commit #%s" description
                                       (substring magit-currently-shown-commit 0 8)))))

      (org-store-link-props
       :type "magit"
       :link link
       :description description))))

(org-add-link-type "magit" 'org-magit-open)
(add-hook 'org-store-link-functions 'org-magit-store-link)

(provide 'org-magit)
;;; org-magit.el ends here
