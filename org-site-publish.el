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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; This file contains various publishing related functions, and is main
;; customized for org-site based on org-publish

(require 'org-publish)

(require 's)

(require 'org-site-utils)
(require 'org-site-hack)

(with-namespace "org-site"
  (defun publish-get-base-files (base-dir)
    "Get all org files under `base-dir`

This function actually copy some code from `org-publish-get-base-files`."
    (setq org-sitemap-requested nil)
    (setq org-publish-temp-files nil)
    (setq extension "org")
    (setq match (concat "^[^\\.].*\\.\\(" extension "\\)$"))
    (org-publish-get-base-files-1 base-dir t match "index" nil)
    org-publish-temp-files)

  (defun pre-publish (base-dir site-sub-dir)
    "Generate necessary \"index.org\" file which is used to generate related
\"index.html\" for org-site.

TODO:
1. support sort criteria, should contains alphabetical and chronological sort."
    (let* ((sub-dir (file-name-as-directory
                     (expand-file-name site-sub-dir base-dir)))
           (sub-index (expand-file-name "index.org" sub-dir))
           (absolute-org-files (org-site-publish-get-base-files sub-dir)))
      (setq title-path-org-files
            (mapcar (lambda (filename)
                      (cons (concat site-sub-dir
                                    "/"
                                    (s-chop-prefix sub-dir filename))
                            (org-site-get-org-file-title filename)))
                    absolute-org-files))
      (with-temp-buffer
        (dolist (title-path-pair title-path-org-files)
          (insert (format "- [[file:%s][%s]]\n"
                          (car title-path-pair)
                          (cdr title-path-pair))))
        (when (file-writable-p sub-index)
          (write-region (point-min)
                        (point-max)
                        sub-index)))))

  (defun post-publish (base-dir site-sub-dir)
    "Delete uncessary \"index.org\" after publish."
    (let* ((sub-dir (file-name-as-directory
                     (expand-file-name site-sub-dir base-dir)))
           (sub-index (expand-file-name "index.org" sub-dir)))
      (delete-file sub-index)))

  (defun publish (project-dir &optional force)
    "This function will publish your site to necessary output result,
currentlly, only html publish is supported.

`project-dir` is where your org-site project located.
if `force` is non-nil, then the project publishing directory will first be
cleared, then the whole org-site project will be republished.

This function is based on `org-publish`, and used org-site's monkey patched
`org-export-as-html` as html's :publishing-function."
    (interactive
     (list (read-directory-name "Project directory: " org-site-project-directory)
           (y-or-n-p "Force republish all? ")))
    (org-site-load-project project-dir)

    (if (file-exists-p org-site-html-publish-dir)
        (if force
            (delete-directory org-site-html-publish-dir t))
      (make-directory org-site-html-publish-dir))

    (dolist (sub-dir '("post" "wiki"))
      (org-site-pre-publish project-dir sub-dir))
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
         :preparation-function nil
         :completion-function nil)
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
    (ad-deactivate 'org-export-as-html)
    (dolist (sub-dir '("post" "wiki"))
      (org-site-post-publish project-dir sub-dir))))

(provide 'org-site-publish)
