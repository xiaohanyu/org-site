;;; org-site-utils.el --- various utils for org-site

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

;; This file contains various utility functions for org-site:
;; 1. project create and load functions
;; 2. template loader and render functions, inspired by Django
;; 3. preamble/footer/postamble generators

;;; Code:

(require 'cl)

(require 'ht)
(require 'mustache)

(require 'org-site-vars)

(defun file-to-string (file)
  "Return the file contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mustache-file-render (file context)
  "Read the file contents, then render it with a hashtable context.

Actually, this function is a shortcut function inspired by Django's
render_to_response."
  (mustache-render (file-to-string file) context))

(defun org-html-get-body (org-html)
  "Get the \"<body>...</body>\" from org-html string.

ORG-HTML is the exported html string of a org file."
  (let ((body-regexp "<body>\\(.\\|\n\\)*</body>"))
    (car (s-match body-regexp org-html))))

(defun org-html-get-body-toc (org-html)
  "Get the toc node from html <body>.

ORG-HTML is the exported html string of a org file.

We just get the <div> with id \"table-of-contents\", which has a preconfigured
toc title \"Table of Contents\". Additional string manipulation is needed if
customizable toc title is needed"
  (let ((org-html-body (org-html-get-body org-html))
        (toc-regexp "<div id=\"table-of-.*\">\\(.\\|\n\\)*?</div>\n</div>"))
    (car (s-match toc-regexp org-html-body))))

(defun org-html-get-body-content (org-html)
  "Get the content node without toc from html <body>.

ORG-HTML is the exported html string of a org file.

First, you need to get the full content dom node, then you substract the toc
node from the content dom node. Lots of dirty code and tricks here, any better
ideas?"
  (let ((org-html-body (org-html-get-body org-html))
        (body-regexp "</?body?>")
        (title-regexp "<h1 class=\"title\">.*?</h1>")
        (toc-regexp "<div id=\"table-of-.*\">\\(.\\|\n\\)*?</div>")
        (text-toc-regexp "<div id=\"text-table-of-.*\">\\(.\\|\n\\)*?</div>"))
    (s-trim
     (reduce
      #'(lambda (regexp string)
          (replace-regexp-in-string regexp "" string))
      (list body-regexp title-regexp toc-regexp text-toc-regexp)
      :initial-value org-html-body
      :from-end t))))

;; This function is inspired by org-website-get-file-property from org-website
;; package, see https://github.com/renard/org-website
;; I do some refactoring to make it suite for org-site
;; org-site need this to provide TAGS/CATEGORY support
(defun org-org-get-file-properties (org-file)
  "Return a property dict of ORG-FILE.

Org file could contains lots of \"#+\...\" properties, which could be used to
control exporting actions, provide document metainfo, etc. This function will
read all the properties and turn it into a property dict."
  (let ((org-file-string-list
         (s-lines (file-to-string org-file)))
        (prop-regexp "^#\\+\\(.*?\\):[ \t]+\\(.*\\)")
        (prop-dict (ht-create)))
    (dolist (line org-file-string-list)
      (setq match-data (s-match prop-regexp line))
      (setq prop-key (nth 1 match-data))
      (setq prop-value (nth 2 match-data))
      (ht-set prop-dict prop-key prop-value))
    (ht-remove prop-dict nil)
    prop-dict))

(defun org-org-get-file-tags (org-file)
  "Get tags of ORG-FILE and return it as a list."
  (let ((tags (ht-get (org-org-get-file-properties org-file)
                      "TAGS")))
    (if tags
        (s-split-words tags)
      nil)))

(defun org-org-get-file-mtime (org-file)
  "Get the mtime of ORG-FILE.

By default, we will use the file mtime as the final return result. But you can
override it with a \"#+DATE: <org-time-stamp>\" in ORG-FILE. Refer
`org-time-stamp' and `org-time-stamp-inactive' for details.

This function is a must to sort posts in `org-site-generate-index'."
  (let* ((attrs (file-attributes org-file))
         (mtime (nth 5 attrs))
         (org-file-prop-dict (org-org-get-file-properties org-file))
         (org-file-date (ht-get org-file-prop-dict "DATE")))
    (if org-file-date
        (setq mtime
              (apply #'encode-time
                     (org-parse-time-string org-file-date))))
    mtime))

(defun org-org-have-math (org-file-or-string)
  "Check whether ORG-FILE has latex math fragment.

ORG-FILE is a org file or (buffer-string) of a org file."
  (let ((org-file-string
         (if (file-exists-p org-file-or-string)
             (file-to-string org-file-or-string)
           org-file-or-string)))
    (-any? (lambda (e)
             (setq re (nth 1 e))
             (s-matches? re org-file-string))
           org-latex-regexps)))

(defun org-org-get-file-category (org-file)
  "Get category of ORG-FILE and return it as a list.

One post could only has one category, which is specified by \"#+CATEGORY: \"
in org file header section."
  (let ((CATEGORY (ht-get (org-org-get-file-properties org-file)
                          "CATEGORY")))
    (if CATEGORY
        (s-trim CATEGORY)
      nil)))

(defun org-org-get-file-title (org-file)
  "Get org file title based on contents or filename.

Org-mode has a `org-publish-find-title' function, but this function has some
minor problems with `org-publish-cache'."
  (with-temp-buffer
    (insert-file-contents org-file)
    (setq opt-plist (org-infile-export-plist))
    (or (plist-get opt-plist :title)
        (file-name-sans-extension
         (file-name-nondirectory filename)))))

(defun org-site-new-project (&optional project-directory)
  "Create a new org-site project.

This function just build a basic directory structure and copy a necessary
org-site configuration file to the project's directory"
  (interactive "GProject directory: ")
  (unless project-directory
    (setq project-directory default-directory))
  (unless (file-exists-p project-directory)
    (make-directory project-directory))
  (setq old-default-directory default-directory)
  (unwind-protect
      (progn
        (cd project-directory)
        (make-directory "post")
        (make-directory "wiki")
        (copy-file (expand-file-name "org-site-config.el"
                                     org-site-load-directory)
                   project-directory)
        (org-site-new-org-file
         (expand-file-name "index.org"
                           project-directory)
         nil)
        (org-site-new-org-file
         (expand-file-name "about.org"
                           project-directory)
         nil)
        (org-site-new-org-file
         (expand-file-name "post/post1.org"
                           project-directory)
         nil)
        (org-site-new-org-file
         (expand-file-name "wiki/wiki1.org"
                           project-directory)
         nil)
        (cd old-default-directory))))

(defun org-site-load-project (&optional project-directory)
  "Load the project settings and make org-site know its current project."
  (interactive
   (list (read-directory-name "Project directory: " org-site-project-directory)))
  (unless project-directory
    (setq project-directory default-directory))
  (setq old-default-directory default-directory)
  (unwind-protect
      (progn
        (cd project-directory)
        (load-file "org-site-config.el"))
    (cd old-default-directory))
  (setq org-site-project-directory project-directory))

(defun org-site-load-template (theme template)
  (expand-file-name
   (format "template/%s/%s" theme template)
   (or org-site-load-directory default-directory)))

(defun org-site-get-static-dir ()
  (file-name-as-directory
   (expand-file-name "static"
                     org-site-load-directory)))

(defun org-site-new-org-file (org-file &optional view-org-file)
  "Find a new org-file and insert some basic org options.

If VIEW-ORG-FILE is non-nil, switch to that buffer, else, kill that buffer."
  (if (file-exists-p org-file)
      (error "File already exists, please type a new file."))
  (let ((buffer (find-file-noselect org-file)))
    (set-buffer buffer)
    (org-insert-export-options-template)
    (save-buffer)
    (if view-org-file
        (switch-to-buffer buffer)
      (kill-buffer buffer))))

(defun org-site-new-post (org-file)
  (interactive
   (list (read-file-name
          "file name: "
          (file-name-as-directory
           (expand-file-name "post"
                             org-site-project-directory)))))
  (new-org-file org-file))

(defun org-site-new-wiki (org-file)
  (interactive
   (list (read-file-name
          "file name: "
          (file-name-as-directory
           (expand-file-name "wiki"
                             org-site-project-directory)))))
  (new-org-file org-file))

(defun org-site-render (template context)
  (mustache-file-render
   (org-site-load-template org-site-theme template)
   context))

(defun org-site-generate-preamble ()
  (let ((context
         (ht-from-plist
          `("site-title" ,org-site-title
            "site-url" ,org-site-url
            "nav-post" "post/"
            "nav-wiki" "wiki/"
            "nav-categories" "categories.html"
            "nav-tags" "tags.html"
            "nav-about" "about.html"
            "enable-google-search" ,org-site-enable-google-search))))
    (org-site-render "preamble.html" context)))

(defun org-site-generate-comment ()
  (let ((context
         (ht-from-plist
          `("disqus-shortname" ,org-site-disqus-shortname))))
    (org-site-render "comment.html" context)))

(defun org-site-generate-meta-info ()
  (let ((context
         (ht-from-plist
          `("post-date" "post-date"
            "update-date" "update-date"
            "tags" "tags"
            "author-name" ,org-site-author-name))))
    (org-site-render "meta-info.html" context)))

(defun org-site-generate-footer ()
  (let ((context
         (ht-from-plist
          `("author-email" ,org-site-author-email
            "author-name" ,org-site-author-name))))
    (org-site-render "footer.html" context)))

(defun org-site-generate-postamble ()
  (let ((context
         (ht-from-plist
          `("footer" ,(org-site-generate-footer)))))
    (if org-site-enable-meta-info
        (ht-set context
                "meta-info"
                (org-site-generate-meta-info)))
    (org-site-render "postamble.html" context)))

(provide 'org-site-utils)
;;; org-site-utils.el ends here
