(use-package pdf-tools
  :if (display-graphic-p)
  :commands pdf-view-mode
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-pdfview
  :if (display-graphic-p)
  :after (pdf-tools)
  :ensure t
  :config
  (eval-after-load 'org '(require 'org-pdfview))
  (add-to-list 'org-file-apps
			   '("\\.pdf\\'" . (lambda (file link)
								 (org-pdfview-open link)))))

(provide 'setup-pdf)
