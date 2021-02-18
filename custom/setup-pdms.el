;;; setup-pdms.el --- Personal Document Management System

;; Author: Schspa  schspa@gmail.com
;; Inspired by https://emacs-china.org/t/pdms/14782

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

(defcustom pdms-calibredb-root "~/doc/Calibre"
  "font-size in fixed."
  :group 'schspa
  :type 'string)

(use-package calibredb
  :ensure t
  :config
  :custom
  (calibredb-root-dir pdms-calibredb-root)
  (calibredb-db-dir (expand-file-name "metadata.db" pdms-calibredb-root))
  (calibredb-library-alist '((pdms-calibredb-root)))
  ;; add douban for metadata search
  (calibredb-fetch-metadata-source-list '("Douban Books" "Google" "Amazon.com")))

(provide 'setup-pdms)
;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-pdms.el ends here.
