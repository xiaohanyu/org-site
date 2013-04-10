;;; org-site-config.el --- site config template for org-site
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
;; This file defines a basic configuration template for a org-site project. Each
;; org-site project should contains this file. When you create a new org-site
;; project using `org-site-new-project`, this file will be copied to that
;; org-site project's directory. When you load a org-site project using
;; `org-site-load-project`, this file will be loaded by elisp to make org-site
;; know necesary information of the current project.

(require 'with-namespace)

(with-namespace "org-site"
  ;;; basic site settings
  (defconst title "Org-Site"
    "title for your org-site")

  (defconst url "your.personal.org.site"
    "url of your org-site")

  (defconst theme "bootstrap"
    "theme for your org-site")

  ;;; personal information
  (defconst author-name "Your name"
    "author name of the site")

  (defconst author-email "org-site@org-site.com"
    "author email")

  ;;; meta info of post
  ;; meta info contains thing like post date, author,category, etc.
  (defconst enable-meta-info nil
    "enable meta info or not")

  ;;; toc of post
  (defconst enable-toc nil
    "enable toc(Table Of Contents) or not")

  ;;; comment system settings
  (defconst enable-comment nil
    "enable disqus comment or not")

  ;; static site need extra third comment system
  ;; currently, org-site only support disqus
  (defconst disqus-identifier "test"
    "disqus-identifier for your org-site")

  (defconst disqus-url "test"
    "disqus-url for your org-site")

  (defconst disqus-shortname "test"
    "disqus-shortname for your org-site")

  ;;; org-publish settings
  (defconst html-publish-dir "~/tmp/org-site-publish"
    "publishing directory for html files"))
