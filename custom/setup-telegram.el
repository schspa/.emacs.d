;;; setup-telegram.el --- telegram support

;; Author: schspa  schspa@schspa-pc
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
(use-package visual-fill-column
  :ensure t)

(use-package telega
  :quelpa (:fetcher github
                    :repo "zevlg/telega.el"
                    :branch "master"
                    :files ("*"))
  :commands (telega)
  :config
  (setq telega-proxies
        (list '(:server "127.0.0.1" :port 1081
                        :enable t
                        :type (:@type "proxyTypeHttp" :http_only nil)))
        )
  :defer t)

(provide 'setup-telegram)
;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-telegram.el ends here.
