(when (require 'undercover nil t)
  (undercover "*.el"
              (:send-report nil)))

(provide 'undercover-init.el)
