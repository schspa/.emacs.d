;;; setup-prog.el --- setup-prog

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

(add-hook 'prog-mode-hook
          '(lambda ()
             (smartparens-mode t)
             (when (< emacs-major-version 26)
               (line-number-mode))
             (show-paren-mode t)
			 ;; show unncessary whitespace that can mess up your diff
			 (setq show-trailing-whitespace 1)))

;; setup GDB
(setq-default
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(use-package flycheck
  :ensure t
  :init
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode t))

(provide 'setup-prog)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-prog.el ends here.
