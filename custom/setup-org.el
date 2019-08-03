;;; setup-org.el --- Setup org mode

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

(defun schspa/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "latex") (string= lang "dot"))))
(setq org-confirm-babel-evaluate 'schspa/org-confirm-babel-evaluate)

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (latex . t)
   (shell . t)
   ))

(provide 'setup-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-org.el ends here.
