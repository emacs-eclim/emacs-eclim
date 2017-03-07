(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "*test*.el") (:report-file "emacs-coveralls.json") (:send-report nil)))

(provide 'undercover-init.el)
