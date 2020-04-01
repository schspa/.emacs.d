(when (display-graphic-p)
  (progn
	(use-package pdf-tools
	  :config
	  ;; iniitialize
	  (pdf-tools-install)
	  (setq-default pdf-view-display-size 'fit-page)
	  (setq pdf-annot-ctivate-create_annotations t)
	  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

	(use-package org-pdfview
	  :ensure t
	  :config
	  (eval-after-load 'org '(require 'org-pdfview))
	  (add-to-list 'org-file-apps
				   '("\\.pdf\\'" . (lambda (file link)
									 (org-pdfview-open link)))))))

(provide 'setup-pdf)
