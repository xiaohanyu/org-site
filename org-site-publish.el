;;; org-site-publish.el --- publish related org-mode files as a website
;; Copyright (C) 2006-2013 Free Software Foundation, Inc.

;; Author: Xiao Hanyu <xiaohanyu1988 AT gmail DOT com>
;; Keywords: org-mode, site-generator
;; Version: 0.01

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 's)

(require 'org-site-utils)
(require 'org-site-hack)

(with-namespace "org-site"
  (defun publish-get-base-files (base-dir)
    "get all org files under `base-dir`"
    (setq org-sitemap-requested nil)
    (setq org-publish-temp-files nil)
    (setq extension "org")
    (setq match (concat "^[^\\.].*\\.\\(" extension "\\)$"))
    (org-publish-get-base-files-1 base-dir t match "index" nil)
    org-publish-temp-files)

  (defun pre-publish (site-dir)
    (let* ((base-dir (plist-get project-plist :base-directory))
           (post-dir (file-name-as-directory
                      (expand-file-name site-dir base-dir)))
           (post-index (expand-file-name "index.org" post-dir))
           (buffer (find-file-noselect post-index))
           (absolute-org-files (org-site-publish-get-base-files post-dir)))
      (set-buffer buffer)
      (erase-buffer)
      (setq title-path-org-files
            (mapcar (lambda (filename)
                      (cons (s-chop-prefix post-dir filename)
                            (org-site-get-org-file-title filename)))
                    absolute-org-files))
      (dolist (title-path-pair title-path-org-files)
        (insert (format "- [[file:%s][%s]]\n"
                        (car title-path-pair)
                        (cdr title-path-pair))))
      (save-buffer)
      (kill-buffer (current-buffer))))

  (defun post-publish (site-dir)
    (let* ((base-dir (plist-get project-plist :base-directory))
           (post-dir (file-name-as-directory
                      (expand-file-name site-dir base-dir)))
           (post-index (expand-file-name "index.org" post-dir)))
      (delete-file post-index)))

  (defun pre-post-publish ()
    (org-site-pre-publish "post"))

  (defun post-post-publish ()
    (org-site-post-publish "post"))

  (defun pre-wiki-publish ()
    (org-site-pre-publish "wiki"))

  (defun post-wiki-publish ()
    (org-site-post-publish "wiki"))

  (defun publish (project-dir &optional force)
    (interactive
     (list (read-directory-name "Project directory: " org-site-project-directory)
           (y-or-n-p "Force republish all? ")))
    (org-site-load-project project-dir)
    ;; enable and activate the monkey-patched `org-export-as-html`
    (ad-enable-advice 'org-export-as-html 'around 'org-site-export-as-html)
    (ad-activate 'org-export-as-html)
    (setq org-publish-project-alist
      `(("org-site"
         :components ("org-site-html" "org-site-static"))
        ("org-site-html"
         :base-directory ,project-dir
         :base-extension "org"
         :publishing-directory ,org-site-html-publish-dir
         :recursive t
         :publishing-function org-publish-org-to-html
         :export-with-tags nil
         :headline-levels 4
         :table-of-contents ,org-site-enable-toc
         :section-numbers t
         :sub-superscript nil
         :todo-keywords nil
         :convert-org-links t
         :author ,org-site-author-name
         :email ,org-site-author-email
         :creator-info nil
         :html-preamble nil
         :html-postamble nil
         :style nil
         :style-include-default nil
         :style-include-scripts nil
         :script nil
         :timestamp t
         :exclude-tags ("noexport" "todo")
         :auto-preamble t
         :preparation-function (org-site-pre-post-publish
                                org-site-pre-wiki-publish)
         :completion-function (org-site-post-post-publish
                               org-site-post-wiki-publish))
        ("org-site-static"
         :base-directory ,(org-site-get-static-dir)
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|otf"
         :publishing-directory ,(file-name-as-directory
                                 (expand-file-name "static"
                                                   org-site-html-publish-dir))
         :recursive t
         :publishing-function org-publish-attachment)))
    (org-publish-all force)
    (ad-disable-advice 'org-export-as-html 'around 'org-site-export-as-html)
    (ad-deactivate 'org-export-as-html)))

(provide 'org-site-publish)
