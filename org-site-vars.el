;;; org-site-vars.el --- the minimal initial global variable needed by org-site
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


(defgroup org-site nil
  "Options for org-site, a site generator based on Emacs org-mode."
  :tag "Emacs org-mode based site generator"
  :group 'org)

(defconst org-site-load-directory
  (file-name-directory load-file-name)
  "The directory where org-site is loaded from.")

(defconst org-site-project-directory nil
  "The current org-site project directory.")

(provide 'org-site-vars)
