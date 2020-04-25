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

(defcustom schspa/blog-source-dir "~/Sync/org/"
  "font-size in mm."
  :group 'schspa
  :type 'string)

(defcustom schspa/blog-sites "~/sites/"
  "font-size in mm."
  :group 'schspa
  :type 'string)

(use-package org-static-blog
  :load-path (lambda () (expand-file-name "git/org-static-blog" user-emacs-directory))
  :ensure t)

(setq org-static-blog-publish-title "schspa's Blog")
(setq org-static-blog-publish-url (blog/fix-publish-url "BLOG_PUBLISH_URL/"))
(setq org-static-blog-publish-directory schspa/blog-sites)
(setq org-static-blog-enable-tags t)
(setq org-static-blog-use-preview t)
(setq org-static-blog-preview-convert-titles t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

(dolist (directory
		 (list
		  org-static-blog-publish-directory))
  (if (not (file-directory-p directory))
	  (progn
		(message "Create %S for org-static-blog" directory)
		(make-directory directory))))

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(defun org-static-blog-page-header ()
  "This header is inserted into the <head> section of every page"
  (blog/fix-publish-url (blog/get-string-from-file (expand-file-name "blog-assert/blog-page-header.html" user-emacs-directory))))

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(defun org-static-blog-page-preamble ()
  "This preamble is inserted at the beginning of the <body> of every page"
  (blog/fix-publish-url (blog/get-string-from-file (expand-file-name "blog-assert/blog-page-preamble.html" user-emacs-directory))))

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(defun org-static-blog-page-postamble ()
  "This postamble is inserted at the end of the <body> of every page:"
  (blog/fix-publish-url (blog/get-string-from-file (expand-file-name "blog-assert/blog-page-postamble.html" user-emacs-directory))))

(require 'org-static-blog)

(defun org-static-blog-generate-post-path (post-filename post-datetime)
  (concat (format-time-string "%Y/%m/%d" post-datetime)
          "/"
          (file-name-nondirectory post-filename)))

(defun schspa/readlines (filePath)
  "Return a list for a file"
  (with-temp-buffer
	(insert-file-contents filePath)
	(split-string (buffer-string) "\n" t)))

(defun schspa/getposts (filePath)
  "Return posts file name"
  (seq-filter 'file-readable-p
			  (mapcar (lambda (arg)
						(expand-file-name arg schspa/blog-source-dir))
					  (schspa/readlines
					   (expand-file-name filePath schspa/blog-source-dir)))))

(defun org-static-blog-get-post-filenames ()
  "Returns a list of all posts."
  (schspa/getposts "posts"))

(defun org-static-blog-get-draft-filenames ()
  "Returns a list of all drafts."
  (schspa/getposts "drafts"))

(defun schspa/parent-directory (dir)
  (if dir (unless (equal "/" dir)
			(file-name-directory (directory-file-name dir)))
	"."))
(defun schspa/copy-file (from to)
  "copy file and make directory automaticly"
  (make-directory (schspa/parent-directory to) t)
  (copy-file from to t t))

(defun schspa/org-html-link (orig-fun &rest args)
  (if (string-equal "file" (org-element-property :type (car args)))
	  (let* ((his-link-path (org-element-property :path (car args)))
			 (his-output-file-name (org-static-blog-matching-publish-filename (buffer-file-name)))
			 (his-output-link-file-name
			  (expand-file-name his-link-path (schspa/parent-directory his-output-file-name))))
		(unless (string-prefix-p "/" his-link-path)
		  (message "copy from %s to %s" his-link-path his-output-link-file-name)
		  (schspa/copy-file his-link-path his-output-link-file-name))))
  (apply orig-fun args))

(defun my-org-static-blog-publish (orig-fun &rest args)
  "An advice to copy blog assert to site directory"
  (message "start publish my blog site")
  (advice-add 'org-html-link :around #'schspa/org-html-link)
  (copy-directory
   (expand-file-name "blog-assert" user-emacs-directory)
   (expand-file-name "assert" schspa/blog-sites) t t)
  (let ((res (apply orig-fun args)))
	(message "org-static-blog-publish returned %S" res)
	(advice-remove 'org-html-link #'schspa/org-html-link)
	res))

(advice-add 'org-static-blog-publish :around #'my-org-static-blog-publish)

(defun my-org-static-blog-publish-file (orig-fun &rest args)
  "An advice to copy blog assert to site directory"
  (message "start publish my blog post")
  (advice-add 'org-html-link :around #'schspa/org-html-link)
  (let ((res (apply orig-fun args)))
	(message "org-static-blog-publish-file returned %S" res)
	(advice-remove 'org-html-link #'schspa/org-html-link)
	res))

(advice-add 'org-static-blog-publish-file :around #'my-org-static-blog-publish-file)

(defun my-org-static-blog-render-post-content (orig-fun &rest args)
  "add comment system for every post"
  (let ((res (apply orig-fun args)))
	(concat res
			"<script src=\"https://utteranc.es/client.js\""
			"    repo=\"schspa/schspa.github.io\"\n"
			"    issue-term=\"title\"\n"
			"    label=\"✨💬✨\"\n"
			"    theme=\"github-light\"\n"
			"    crossorigin=\"anonymous\"\n"
			"    async>"
			"</script>"
			))
  )

(advice-add 'org-static-blog-render-post-content :around #'my-org-static-blog-render-post-content)
(provide 'setup-blog)
;; coding: utf-8
;; End:

;;; setup-blog.el ends here.
