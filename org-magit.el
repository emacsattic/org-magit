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

;; This module adds support for magit links in org buffers. The following links
;; are supported:
;; - magit:/path/to/repo::commit@<hash>
;; - magit:/path/to/repo::status
;; - magit:/path/to/repo::log

;; Of course those links can be stored as usual with `org-store-link' from the
;; corresponding magit buffers. By default the path to the repo is abbreviated
;; with `abbreviate-file-name', just like org-mode does. See
;; `directory-abbrev-alist' for configuring its behavior. Alternately, you can
;; customize `org-magit-filename-transformer' and provide your own
;; transformer function.

;; When exporting those links, the variable `org-magit-known-public-providers'
;; is used to generate meaningful links. This assumes there exists a public
;; http server that is able to expose those objects.

;; Certain settings can be configured directly at the repository level
;; if needed. For example
;;
;; $ git config org-magit.remote upstream
;;
;; In this case, html links will point to the "upstream" webserver, instead of
;; the default "origin". URL templates can also be stored in the
;; repository. For example
;;
;; $ git config org-magit.log http://myserver/plop.git/history

;;; Code:

(require 'org)
(require 'magit)

(defvar org-magit-actions
  '((status :open current-buffer)
    (log :open org-magit-open-log)
    (commit :open org-magit-open-commit)))

(defgroup org-magit nil
  "Magit links for org-mode"
  :group 'magit
  :group 'org-link)

(defcustom org-magit-public-remote "origin"
  "Default remote to use when exporting links."
  :group 'org-magit
  :type 'string)

(defcustom org-magit-config-prefix "org-magit"
  "Section to read from in git repository configuration."
  :group 'org-magit
  :type 'string)

(defcustom org-magit-known-public-providers
  '(("^git@github.com:\\(.*\\)\\.git"
     status "https://github.com/\\1/"
     log "https://github.com/\\1/commits"
     commit "https://github.com/\\1/commit/%s"))
  "List of git providers, and how to generate links for each
  object category."
  :group 'org-magit
  :type '(repeat (list :tag "Provider identifier" regexp
                       (set :tag "URL templates" :inline t
                            (list :inline t
                                  (const :tag "Status" status)
                                  (string :tag "Status URL"))
                            (list :inline t
                                  (const :tag "Log" log)
                                  (string :tag "Log URL"))
                            (list :inline t
                                  (const :tag "Commit" commit)
                                  (string :tag "Commit URL"))
                            ))))

(defcustom org-magit-filename-transformer
  'abbreviate-file-name
  "Function to call to produce canonical repository name. This
must take a path as input, and provide an equivalent
representation of this path as output."
  :group 'org-magit
  :type 'function)

(defun org-magit-split-string (str)
  (let* ((strlist (split-string str "::"))
         (repo (first strlist))
         (view (second strlist))
         (view-sym nil)
         (args nil))
    (cond ((string-match "^status" view)
           (setq view-sym 'status))
          ((string-match "^log" view)
           (setq view-sym 'log))
          ((string-match "^commit@\\(.*\\)" view)
           (setq view-sym 'commit
                 args (list (match-string 1 view)))))
    (list view-sym repo args)))

(defun org-magit-open-log ()
  (magit-display-log)
  (current-buffer))

(defun org-magit-open-commit (commit)
  (magit-show-commit commit)
  (get-buffer magit-commit-buffer-name))

(defun org-magit-open (str)
  (let* ((split (org-magit-split-string str))
         (view (first split))
         (repo (second split))
         (func (plist-get (cdr (assoc view org-magit-actions)) :open))
         (args (third split)))
    (when func
      (pop-to-buffer
       (save-window-excursion
         (magit-status repo)
         (apply func args))))))

(defun org-magit-get (repo &rest keys)
  (let ((default-directory repo))
    (apply 'magit-get keys)))

(defun org-magit-guess-public-url (view url)
  (let ((res nil))
    (dolist (provider org-magit-known-public-providers)
      (let ((regexp (car provider)))
        (when (string-match regexp url)
          (setq res (replace-match (plist-get (cdr provider) view) nil nil url)))))
    res))

(defun org-magit-generate-public-url (path)
  (let* ((split (org-magit-split-string path))
         (view (first split))
         (repo (second split))
         (remote (or (org-magit-get repo (format "%s.remote" org-magit-config-prefix))
                     org-magit-public-remote))
         (tpl (or (org-magit-get repo (format "%s.%s" org-magit-config-prefix view))
                  (org-magit-guess-public-url
                   view (org-magit-get repo (format "remote.%s.url" remote))))))
    (apply 'format tpl (third split))))

(defun org-magit-export (path desc format)
  (let ((url (or (org-magit-generate-public-url path) path)))
    (set-text-properties 0 (length url) nil url)
    (set-text-properties 0 (length desc) nil desc)
    (cond
     ((eq format 'html) (format "<a href=\"%s\">%s</a>" url (or desc url)))
     ((eq format 'latex) (format "\\href{%s}{%s}" url (or desc url)))
     (t (or desc url)))))

(defun org-magit-make-link (repo &rest components)
  (apply 'org-make-link "magit:" repo components))

(defun org-magit-clean-repository (repo)
  (let ((name (file-name-nondirectory (directory-file-name repo))))
    (when (not (string-match "\\.git" name))
      (setq name (concat name ".git")))
    name))

(defun org-magit-store-link ()
  (when (eq major-mode 'magit-mode)
    (let* ((repo (or (and org-magit-filename-transformer
                          (funcall org-magit-filename-transformer
                                   default-directory))
                     default-directory))
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

(org-add-link-type "magit" 'org-magit-open 'org-magit-export)
(add-hook 'org-store-link-functions 'org-magit-store-link)

(provide 'org-magit)
;;; org-magit.el ends here
