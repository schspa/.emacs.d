;;; setup-blog.el --- Blogs

;; Author: schspa  schspa@gmail.com
;; URL:

;; Copyright (C) 2019, schspa, all rights reserved.
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

;;

;;; Code:
(defun blog/get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun blog/fix-publish-url(from)
  "Insert publish url"
  (replace-regexp-in-string "BLOG_PUBLISH_URL" "" from t))

(use-package org-static-blog
  :ensure t)
(setq org-static-blog-publish-title "schspa's Blog")
(setq org-static-blog-publish-url (blog/fix-publish-url "BLOG_PUBLISH_URL/"))
(setq org-static-blog-publish-directory "~/sites/")
(setq org-static-blog-posts-directory "~/Sync/org/posts/")
(setq org-static-blog-drafts-directory "~/sites/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-static-blog-use-preview t)
(setq org-static-blog-preview-convert-titles t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(setq org-static-blog-page-header
	  (blog/fix-publish-url (blog/get-string-from-file (expand-file-name "custom/templete/blog-page-header.html" user-emacs-directory))))

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(setq org-static-blog-page-preamble
	  (blog/fix-publish-url (blog/get-string-from-file (expand-file-name "custom/templete/blog-page-preamble.html" user-emacs-directory))))

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-page-postamble
	  (blog/fix-publish-url (blog/get-string-from-file (expand-file-name "custom/templete/blog-page-postamble.html" user-emacs-directory))))

(require 'org-static-blog)

(defun org-static-blog-generate-post-path (post-filename post-datetime)
  (concat (format-time-string "%Y/%m/%d" post-datetime)
          "/"
          (file-name-nondirectory post-filename)))

(provide 'setup-blog)
;; coding: utf-8
;; End:

;;; setup-blog.el ends here.
