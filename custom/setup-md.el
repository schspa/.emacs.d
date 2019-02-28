;;; setup-md.el --- setup-md

;; Author: schspa  schspa@schspas-iMac.local
;; Keywords: elisp
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

(use-package simple-httpd
  :ensure t)

(use-package maple-preview
  :load-path "~/.emacs.d/git/emacs-maple-preview"
  :ensure nil
  :commands (maple-preview-mode))

(provide 'setup-md)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-—.el ends here.
