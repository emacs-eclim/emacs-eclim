(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "*test*.el") (:send-report nil)))

(provide 'undercover-init.el)
