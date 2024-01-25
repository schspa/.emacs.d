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
(require 'epg-config)
;; (fset 'epg-wait-for-status 'ignore)
;; -*- epa-file-encrypt-to: ("schspa@gmail.com") -*-
(setq epa-file-encrypt-to '("schspa@gmail.com"))

;; https://www.reddit.com/r/emacs/comments/137r7j7/gnupg_241_encryption_issues_with_emacs_orgmode/
;; https://dev.gnupg.org/T6481
;; https://dev.gnupg.org/rG2f872fa68c6576724b9dabee9fb0844266f55d0d
(setq epg-gpg-program "/opt/homebrew/opt/gnupg@2.2/bin/gpg")
(setq epg-gpgsm-program "/opt/homebrew/opt/gnupg@2.2/bin/gpgsm")
(setq epg-gpgconf-program "/opt/homebrew/opt/gnupg@2.2/bin/gpgconf")
;; epg-gpg-home-directory

(custom-set-variables
 '(nsm-settings-file (expand-file-name "network-security.data" my-cache-dir)))

(provide 'setup-security)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-security.el ends here.
