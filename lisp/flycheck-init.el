;;; flycheck-init.el --- initialize basic flycheck related functionality

;;; Commentary:

;;; Code:
(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'flycheck-init)
;;; flycheck-init.el ends here
