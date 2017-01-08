(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "test-*.el")))

(provide 'undercover-init.el)
