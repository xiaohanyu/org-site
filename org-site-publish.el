;;; org-site-publish.el --- publish related org-mode files as a website

;; Copyright (C) 2013 Xiao Hanyu

;; Author: Xiao Hanyu <xiaohanyu1988@gmail.com>
;; Version: 0.01
;; Keywords: org-mode, site-generator
;; URL: http://github.com/xiaohanyu/org-site

;; This file is not part of GNU Emacs.

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

;; This file contains various publishing related functions, and is main
;; customized for org-site based on org-publish

;;; Code:

(require 'org-publish)

(require 's)

(require 'org-site-utils)
(require 'org-site-hack)

(defun org-site-publish-get-base-files (base-dir &optional extension)
  "Get all org files under BASE-DIR.

This function actually copy some code from `org-publish-get-base-files'."
  (let ((org-sitemap-requested nil)
        (org-publish-temp-files nil)
        (match (concat "^[^\\.].*\\.\\("
                       (if extension extension "org")
                       "\\)$")))
    (org-publish-get-base-files-1 base-dir t match "index" nil)
    org-publish-temp-files))

(defun org-site-generate-index (base-dir site-sub-dir)
  "Generate necessary \"index.org\" file which is used to generate related
\"index.html\" for org-site.

TODO:
1. support sort criteria, should contains alphabetical and chronological sort."
  (let* ((sub-dir (file-name-as-directory
                   (expand-file-name site-sub-dir base-dir)))
         (sub-index (expand-file-name "index.org" sub-dir))
         (absolute-org-files (org-site-publish-get-base-files sub-dir)))
    (setq path-title-mtime-org-files
          (mapcar (lambda (filename)
                    (list (concat site-sub-dir
                                  "/"
                                  (s-chop-prefix sub-dir filename))
                          (org-org-get-file-title filename)
                          (org-org-get-file-mtime filename)))
                  absolute-org-files))
    (setq path-title-mtime-org-files
          (sort path-title-mtime-org-files
                #'(lambda (path-title-mtime1 path-title-mtime2)
                    (not (time-less-p (nth 2 path-title-mtime1)
                                      (nth 2 path-title-mtime2))))))
    (with-temp-buffer
      (insert (format "#+TITLE: %ss\n" (capitalize site-sub-dir)))
      (dolist (path-title-mtime path-title-mtime-org-files)
        (insert (format "- %s :: [[file:%s][%s]]\n"
                        (format-time-string "%Y-%m-%d" (caddr path-title-mtime))
                        (car path-title-mtime)
                        (cadr path-title-mtime))))
      (when (file-writable-p sub-index)
        (write-region (point-min)
                      (point-max)
                      sub-index)))))

(defun org-site-generate-tags (base-dir site-sub-dir)
  "Generate necessary \"tags.org\" for posts."
  (let* ((sub-dir (file-name-as-directory
                   (expand-file-name site-sub-dir base-dir)))
         (tags-file (expand-file-name "tags.org" base-dir))
         (absolute-org-files (org-site-publish-get-base-files sub-dir))
         (tags (ht-create)))
    (dolist (org-file absolute-org-files)
      (dolist (tag (org-org-get-file-tags org-file))
        (let ((path-title-pair
               (cons (concat site-sub-dir
                             "/"
                             (s-chop-prefix sub-dir org-file))
                     (org-org-get-file-title org-file)))
              (ht-tag (ht-get tags tag)))
          (ht-set tags tag (cons path-title-pair ht-tag)))))
    (with-temp-buffer
      (insert (format "#+TITLE: Tags\n"))
      (dolist (tag (ht-keys tags))
        (insert (format "* %s\n" tag))
        (dolist (path-title-pair (ht-get tags tag))
          (insert (format "- [[file:%s][%s]]\n"
                          (car path-title-pair)
                          (cdr path-title-pair)))))
      (when (file-writable-p tags-file)
        (write-region (point-min)
                      (point-max)
                      tags-file)))))

(defun org-site-generate-categories (base-dir site-sub-dir)
  "Generate necessary \"categories.org\" for posts."
  (let* ((sub-dir (file-name-as-directory
                   (expand-file-name site-sub-dir base-dir)))
         (categories-file (expand-file-name "categories.org" base-dir))
         (absolute-org-files (org-site-publish-get-base-files sub-dir))
         (categories (ht-create)))
    (dolist (org-file absolute-org-files)
      (let* ((path-title-pair
              (cons (concat site-sub-dir
                            "/"
                            (s-chop-prefix sub-dir org-file))
                    (org-org-get-file-title org-file)))
             (category (org-org-get-file-category org-file))
             (ht-category (ht-get categories category)))
        (if category
            (ht-set categories category (cons path-title-pair ht-category)))))
    (with-temp-buffer
      (insert (format "#+TITLE: Categories\n"))
      (dolist (category (ht-keys categories))
        (insert (format "* %s\n" category))
        (dolist (path-title-pair (ht-get categories category))
          (insert (format "- [[file:%s][%s]]\n"
                          (car path-title-pair)
                          (cdr path-title-pair)))))
      (when (file-writable-p categories-file)
        (write-region (point-min)
                      (point-max)
                      categories-file)))))

(defun org-site-pre-publish (base-dir)
  "Generate necessary index, tags, categories org files."
  (dolist (sub-dir '("post" "wiki"))
    (org-site-generate-index base-dir sub-dir))
  (org-site-generate-tags base-dir "post")
  (org-site-generate-categories base-dir "post"))

(defun org-site-post-publish (base-dir)
  "Delete the auto-generated index, tags, categories org files after publish."
  (let ((post-index (expand-file-name "post/index.org" base-dir))
        (wiki-index (expand-file-name "wiki/index.org" base-dir))
        (tags (expand-file-name "tags.org" base-dir))
        (categories (expand-file-name "categories.org" base-dir)))
    (unless org-site-debug
      (dolist (org-file (list post-index wiki-index tags categories))
        (delete-file org-file)))))

(defun org-site-publish (project-dir &optional republish localhost)
  "This function will publish your site to necessary output result,
currentlly, only html publish is supported.

PROJECT-DIR is where your org-site project located.

If REPUBLISH is non-nil, then the project publishing directory will first be
cleared, then the whole org-site project will be republished.

If LOCALHOST is non-nil, then org-site will set `org-site-url' to \"localhost\",
thus you can preview your site even if you didn't set `org-site-url' to
\"localhost\" in org-site-config.el.

This function is based on `org-publish', and used org-site's monkey patched
`org-export-as-html' as html's :publishing-function."
  (interactive
   (list (read-directory-name "Project directory: " org-site-project-directory)
         (y-or-n-p "Force republish all? ")
         (unless (s-matches? "localhost" org-site-url)
           (y-or-n-p "Force publish in localhost mode? "))))
  (org-site-load-project project-dir)

  (unless org-site-html-publish-dir
    (setq org-site-html-publish-dir
          (expand-file-name "publish"
                            org-site-project-directory)))

  (if (file-exists-p org-site-html-publish-dir)
      (progn
        (unless (file-directory-p org-site-html-publish-dir)
          (error "%s exists but is not a directory"
                 org-site-html-publish-dir))
        (if republish
            (dolist (file-or-dir
                     (directory-files org-site-html-publish-dir t))
              ;; do not delete '.', '..' and '.git', '.gitignore'
              (unless (s-matches? "\\.$\\|\\.git" file-or-dir)
                (if (file-directory-p file-or-dir)
                    (delete-directory file-or-dir t)
                  (delete-file file-or-dir))))))
    (make-directory org-site-html-publish-dir))

  (if localhost
      (setq org-site-url "localhost"))

  (org-site-pre-publish project-dir)
  ;; Enable and activate the monkey-patched `org-export-as-html'.
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
  (org-publish-all republish)
  (ad-disable-advice 'org-export-as-html 'around 'org-site-export-as-html)
  (ad-deactivate 'org-export-as-html)
  (org-site-post-publish project-dir))

(provide 'org-site-publish)
;;; org-site-publish.el ends here
