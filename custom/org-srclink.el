;;; org-srclink.el --- Link to source file -*- lexical-binding: t -*-

;; Author: Schspa  schspa@ArchLinux
;; URL:

;; Copyright (C) 2021, Schspa, all rights reserved.
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
(require 'org)

(defcustom org-srclink-repos nil
  "Alist of properties that defines repo responsiale in Org mode.

The key in each association is a string of the link type.
Subsequent optional elements make up a property list for that
type.

  When nil, export for that type of link is delegated to the
  back-end.
"
  :group 'org-link
  :package-version '(Org . "9.1")

  :type '(alist :tag "Link display parameters"
		        :value-type plist)
  :safe t)

(defun org-srclink-set-parameters (type &rest parameters)
  "Set link TYPE properties to PARAMETERS.
PARAMETERS should be keyword value pairs.  See
`org-srclink-parameters' for supported keys."
  (let ((data (assoc type org-srclink-repos)))
    (if data (setcdr data (org-combine-plists (cdr data) parameters))
      (push (cons type parameters) org-srclink-repos))))

(defun org-srclink-get-repo (path)
  "Get org-srclink repo parameters for file path.

If a registered repo matchs, return the parameters.
Return nil if non match"
  (seq-some
   (lambda (item)
     (let* ((param (cdr item))
            (localpath (file-truename (plist-get param :localpath))))
       (when (string-prefix-p localpath (file-truename path))
         item))
     ) org-srclink-repos))

(defun org-srclink-get-repo-by-name (repo)
  "Get org-srclink repo parameters for repo name.

If a registered repo matchs, return the parameters.
Return nil if non match"
  (alist-get repo org-srclink-repos nil nil 'equal))

(defun org-srclink-open-as-file (path arg)
  "Export a srclink link from Org files."
  (let* ((link-params (split-string-and-unquote path "#"))
         (repo (org-srclink-get-repo-by-name (car link-params)))
         (path-index (if repo 1 0))
         (local-path (if repo (expand-file-name (nth path-index link-params) (cadr repo))
                       (nth path-index link-params)))
         (line (nth (+ 1 path-index) link-params))
         (revision (nth (+ 2 path-index) link-params))
         (full-path (if line (concat local-path "::" line) local-path)))
    (org-link-open-as-file full-path arg)))

(defun org-srclink-get-repo-name ()
  "Extract the path from the buffer name."
  (file-relative-name buffer-file-name (vc-git-root buffer-file-name)))

(defun run-shell-command-in-project-root-at-file-path (command file-path)
  "Run a shell command in the project root directory of FILE-PATH."
  (interactive "sShell command: \nFFile path: ")
  (let* ((project-root (projectile-project-root (file-name-directory file-path)))
         (default-directory project-root)
         (output-buffer (generate-new-buffer "*Shell Command Output*"))
         (exit-code (call-process-shell-command command nil output-buffer t)))
    (if (zerop exit-code)
        (with-current-buffer output-buffer
          (buffer-string))
      (progn
        (message "Shell command failed with exit code %d." exit-code)
        (let ((stdout (with-current-buffer output-buffer
                        (buffer-string)))
              (stderr (with-current-buffer (get-buffer "*Messages*")
                        (buffer-string))))
          (kill-buffer output-buffer)
          (error "Shell command failed.\nSTDOUT:\n%s\nSTDERR:\n%s" stdout stderr))))))



(defun org-srclink-export-to (file-path export-type)
  "Export a file as latex"
  (let* ((real-file-path (file-truename file-path))
         (project-root (projectile-project-root (file-name-directory real-file-path)))
         (relative-path (file-relative-name real-file-path project-root)))
    (message (format "Export %s to latex" file-path))
    (run-shell-command-in-project-root-at-file-path (format "%s --export_type %s %s" (expand-file-name "bin/doxygen-source-export.py" user-emacs-directory) export-type relative-path) real-file-path)))

(org-link-set-parameters
 "srclink"
 :follow #'org-srclink-open-as-file
 :export #'org-srclink-export
 :store #'org-srclink-store-link)

(defun org-srclink-export (link description linkformat)
  "Export a srclink link from Org files."
  (let* ((link-params (split-string-and-unquote link "#"))
         (repo (nth 0 link-params))
         (repo-info (org-srclink-get-repo-by-name repo))
         (path (nth 1 link-params))
         (line (nth 2 link-params))
         (revision (nth 3 link-params))
         (desc (or description link)))
    (pcase linkformat
      (`html (org-srclink-export-to (if path path link) "html"))
      (`latex (org-srclink-export-to (if path path link) "latex"))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (t path))))

(defun org-srclink-store-link ()
  "Store a link to a manpage."
  (let* ((repo-info (org-srclink-get-repo buffer-file-name)))
    (when repo-info
      ;; This is a man page, we do make this link
      (let* ((path (file-relative-name buffer-file-name (vc-git-root buffer-file-name)))
             (revision (when (and vc-mode buffer-file-name)
                         (vc-working-revision buffer-file-name)))
             (line (line-number-at-pos))
             (link (concat "srclink:"
                           (combine-and-quote-strings
                            (list (car repo-info) path (format "%s" line) revision) "#")))
             (description (format "%s source for %s" (car repo-info) path)))
        (org-link-store-props
         :type "srclink"
         :link link
         :description description)))))

(provide 'org-srclink)


;;; org-srclink.el ends here.
