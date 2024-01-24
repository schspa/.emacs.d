;;; early-init.el --- Early init for emacs

;; Author: Schspa  schspa@ArchLinux
;; URL:

;; Copyright (C) 2022, Schspa, all rights reserved.
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
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(tool-bar-mode 0)
(scroll-bar-mode -1)
(column-number-mode)

(unless (eq system-type 'darwin)
  (menu-bar-mode 0))

(scroll-bar-mode 0)

(setq byte-compile-warnings nil)

;; Local Variables:
;; coding: utf-8
;; End:

;;; early-init.el ends here.
