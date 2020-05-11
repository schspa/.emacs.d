(use-package pdf-tools
  :if (display-graphic-p)
  :commands pdf-view-mode
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(provide 'setup-pdf)
