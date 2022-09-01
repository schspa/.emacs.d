;;; setup-net.el --- setup-net

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




(use-package w3m
  :ensure t
  :config
  (setq w3m-search-default-engine "g")
  (setq w3m-command "w3m")
  (eval-after-load "w3m-search"
    '(progn
       (add-to-list 'w3m-search-engine-alist '("b" "http://www.baidu.com/search?hl=en&q=%s" nil))
       (add-to-list 'w3m-search-engine-alist '("g" "http://www.google.com.au/search?hl=en&q=%s" utf-8))
       (add-to-list 'w3m-search-engine-alist '("wz" "http://zh.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
       (add-to-list 'w3m-search-engine-alist '("q" "http://www.google.com.au/search?hl=en&q=%s+site:stackoverflow.com" utf-8))
       (add-to-list 'w3m-search-engine-alist '("s" "http://code.ohloh.net/search?s=%s&browser=Default"  utf-8))
       (add-to-list 'w3m-search-engine-alist '("bl" "http://blogsearch.google.com.au/blogsearch?q=%s" utf-8))
       (add-to-list 'w3m-search-engine-alist '("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
       (add-to-list 'w3m-search-engine-alist '("d" "http://dictionary.reference.com/search?q=%s" utf-8))
       (add-to-list 'w3m-search-engine-alist '("j" "http://www.google.com.au/search?ie=UTF-8&oe=UTF-8&sourceid=navclient&btnI=1&q=%s+site:developer.mozilla.org" utf-8))
       ))
  (setq w3m-command-arguments
        (nconc w3m-command-arguments
               '("-o" "http_proxy=http://127.0.0.1:1087/" "-o" "https_proxy=http://127.0.0.1:1087/")))
  :hook
  (w3m-mode . (lambda()
                (when (>= emacs-major-version 26)
                  (display-line-numbers-mode -1)
                  (linum-mode)))))

(use-package edit-server
  :ensure t
  :config
  (when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (edit-server-start)))

(provide 'setup-net)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-net.el ends here.
