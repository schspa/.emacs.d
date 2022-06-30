;;; setup-modeline.el --- modeline

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

(use-package find-file-in-project
  :ensure t)

(use-package mu4e-alert
  :ensure t
  :custom
  (mu4e-alert-notify-repeated-mails t)
  (mu4e-alert-enable-notifications t))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-project-detection 'ffip)
  (doom-modeline-mu4e t)
  :config
  (mu4e-alert-enable-mode-line-display)
  :hook (after-init . doom-modeline-mode))

(provide 'setup-modeline)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-modeline.el ends here.
