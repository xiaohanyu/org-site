;;; org-site-vars.el --- the minimum global variables required by org-site

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

;; This file defines the minimum variables required by org-site, other necessary
;; variables and configurations are loaded through seperate site config file
;; org-site-config.el

;;; Code:


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
;;; org-site-vars.el ends here
