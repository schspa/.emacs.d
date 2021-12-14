;;; setup-eaf.el --- Description -*- lexical-binding: t; -*-

;; Author: SH147NDT2F  schspa@SH147NDT2F
;; Keywords: elisp
;; URL:

;; Copyright (C) 2019, SH147NDT2F, all rights reserved.
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
(use-package eaf
  :if sys/linuxp
  :quelpa
  (eaf :fetcher github
       :repo "emacs-eaf/emacs-application-framework"
       :files ("core" "app" "*.el" "*.py"))
  :init
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (global-set-key (kbd "<f12>") #'eaf-toggle)
  (require 'eaf)
  (defun eaf-toggle ()
    (interactive)
    (if (advice-member-p 'eaf--find-file-advisor 'find-file)
        (advice-remove 'find-file 'eaf--find-file-advisor)
      (advice-add #'find-file :around #'eaf--find-file-advisor))))

(use-package eaf-pdf-viewer
  :if sys/linuxp
  :quelpa
  (eaf-pdf-viewer :fetcher github
                  :repo "emacs-eaf/eaf-pdf-viewer"
                  :files ("*"))
  :init
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (require 'eaf-pdf-viewer)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding))

(provide 'setup-eaf)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-eaf.el ends here.
