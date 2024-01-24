;;; setup-security.el --- Setup Security

;; Author: Schspa Shi  schspa@gmail.com
;; URL:

;; Copyright (C) 2023, Schspa Shi, all rights reserved.
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
(setq auth-sources '("~/.authinfo.gpg"))
;; -*- epa-file-encrypt-to: ("schspa@gmail.com") -*-
(custom-set-variables
 '(nsm-settings-file (expand-file-name "network-security.data" my-cache-dir)))

(provide 'setup-security)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-security.el ends here.
