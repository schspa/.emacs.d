;;; setup-eaf.el --- Description

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
       :repo "manateelazycat/emacs-application-framework"
       :files ("*"))
  :defer t
  :init
  (defun schspa/move-mouse-to-frame-top-bottom ()
    "Move mouse position to bottom"
    (let* ((is_i3 (equal "i3" (getenv "DESKTOP_SESSION")))
           (frame (car (mouse-position)))
           (commands (format "xdotool mousemove %d %d" (car (frame-edges)) (nth 3 (frame-edges)))))
      (if is_i3 (shell-command commands))))
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  :hook (eaf-mode . schspa/move-mouse-to-frame-top-bottom))


(provide 'setup-eaf)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-eaf.el ends here.
