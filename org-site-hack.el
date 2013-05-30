;;; org-site-hack.el --- some hacked org-export-as-* for org-site

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

;; This file will contains various hacked org-export-as-* functions for
;; org-site. Currently, only `org-export-as-html' is hacked. The hacked
;; `org-export-as-html' support org-site mustache template load and render.

;;; Code:

(require 'org-html)

(require 'org-site-utils)

(defadvice org-export-as-html (around org-site-export-as-html disable)
  "A hacked `org-export-as-html' for org-site.

This function is monkey-patched to support mustache template load and render. It
contains some code copied of modified from upstream `org-mode'. This
monkey-patched function will only be used by `org-site-publish', and will be
disabled outside `org-site-publish'."
  (let* ((preamble (org-site-generate-preamble))
         (postamble (org-site-generate-postamble))
         (to-buffer 'string)
         (body-only nil)
         (opt-plist
          (org-export-process-option-filters
           (org-combine-plists (org-default-export-plist)
                               ext-plist
                               (org-infile-export-plist))))
         (title (org-html-expand
                 (or (plist-get opt-plist :title)
                     (and (not body-only)
                          (not
                           (plist-get opt-plist :skip-before-1st-heading))
                          (org-export-grab-title-from-buffer))
                     (and buffer-file-name
                          (file-name-sans-extension
                           (file-name-nondirectory buffer-file-name)))
                     "UNTITLED")))

         (html-extension (plist-get opt-plist :html-extension))
         (pub-dir (plist-get opt-plist :publishing-directory))
         (filename (expand-file-name
                    (concat
                     (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name))
                     "." html-extension)))
         ; Avoid any auto-insert stuff for the new file
         (auto-insert nil)
         (base-url nil)
         (org-file-string (substring-no-properties (buffer-string))))

    ad-do-it

    (setq have-math (org-org-have-math org-file-string))
    (setq org-html (substring-no-properties (car kill-ring)))
    (setq content (org-html-get-body-content org-html))
    (setq toc (org-html-get-body-toc org-html))

    (setq ftname
          (concat (file-name-as-directory pub-dir)
                  (and (string-match (regexp-quote base-dir) filename)
                       (substring filename (match-end 0)))))

    (setq buffer (find-file-noselect ftname))
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)
    (and (fboundp 'set-buffer-file-coding-system)
         (set-buffer-file-coding-system coding-system-for-write))

    (if (s-matches? "localhost" org-site-url)
        (setq base-url
              "/"

              bootstrap-min-css
              "static/bootstrap/css/bootstrap.min.css"

              bootstrap-responsive-min-css
              "static/bootstrap/css/bootstrap-responsive.min.css"

              jquery-min-js
              "static/jquery/jquery-2.0.0.min.js"

              bootstrap-min-js
              "static/bootstrap/js/bootstrap.min.js")
      (setq base-url
            org-site-url

            bootstrap-min-css
            org-site-bootstrap-min-css-link

            bootstrap-responsive-min-css
            org-site-bootstrap-reponsive-min-css-link

            jquery-min-js
            org-site-jquery-min-js-link

            bootstrap-min-js
            org-site-bootstrap-min-js-link))

    (let ((context
           (ht-from-plist
            `("title" ,title
              "base-url" ,base-url
              "bootstrap-min-css" ,bootstrap-min-css
              "bootstrap-responsive-min-css" ,bootstrap-responsive-min-css
              "jquery-min-js" ,jquery-min-js
              "bootstrap-min-js" ,bootstrap-min-js
              "have-math" ,have-math
              "preamble" ,preamble
              "content" ,content
              "postamble" ,postamble))))
      (ht-set context "enable-toc"
              (if (and toc org-site-enable-toc)
                  (list (ht-from-plist `("toc" ,toc)))
                nil))
      (if org-site-enable-comment
          (ht-set context
                  "comment"
                  (org-site-generate-comment)))
      (insert (org-site-render "page.html" context)))

    (save-buffer)
    (kill-buffer (current-buffer))))

(provide 'org-site-hack)
;;; org-site-hack.el ends here
