(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*test*.el")
              (:report-file "coveralls.json")
              (:send-report nil)))

(provide 'undercover-init.el)
