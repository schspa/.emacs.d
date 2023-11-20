;;; setup-vterm.el --- vterm setup

;; Author: schspa  schspa@iMac.local
;; URL:

;; Copyright (C) 2020, schspa, all rights reserved.
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

(use-package use-package-ensure-system-package
  :ensure t)

(use-package vterm
  :commands vterm
  :ensure-system-package libtool-bin
  :hook (vterm-mode
         . (lambda() (when (>= emacs-major-version 26)
                       (display-line-numbers-mode -1)))))

(provide 'setup-vterm)
;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-vterm.el ends here.
