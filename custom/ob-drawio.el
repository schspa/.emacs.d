;;; ob-drawio.el --- org-babel support for drawio evaluation

;; Copyright (C) 2019 Schspa

;; Author: Alexei Nunez <schspa@gmail.com>
;; URL: https://github.com/arnm/ob-drawio
;; Package-Version: 20190828.1927
;; Keywords: lisp
;; Version: 0

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

;; Org-Babel support for evaluating drawio diagrams.

;;; Requirements:

;; drawio

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:drawio
  '((:results . "file") (:exports . "results") (:in-file . "in-file"))
  "Default arguments for evaluatiing a drawio source block.")

(defcustom ob-drawio-cli-path
  (cond (sys/macp "/Applications/draw.io.app/Contents/MacOS/draw.io")
        (t nil))
  "Path to drawio.cli executable."
  :group 'org-babel
  :type 'string)

(defcustom ob-drawio-cli-extra-param ""
  "drawio.cli extra custom parameter"
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:drawio (body params)
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "drawio requires a \":file\" header argument")))
		 (in-file (or (cdr (assoc :in-file params))
					  nil))
		 (temp-file (org-babel-temp-file "drawio-"))
         (cmd (if (not ob-drawio-cli-path)
                  (error "`ob-drawio-cli-path' is not set")
                (concat (shell-quote-argument (expand-file-name ob-drawio-cli-path))
                        " " ob-drawio-cli-extra-param
						" -f png "
                        " -x \"" (org-babel-process-file-name (or in-file temp-file))
                        "\" -o \"" (org-babel-process-file-name out-file)
						"\""))))
	(unless (file-exists-p ob-drawio-cli-path)
	  (error "could not find drawio.cli executable at %s" ob-drawio-cli-path))
	(with-temp-file temp-file (insert body))
	(message cmd)
	(org-babel-eval cmd "")
	nil))

(provide 'ob-drawio)


;;; ob-drawio.el ends here
