;;; projectile-init.el --- initialize projectile

;;; Commentary:

;;; Code:
(require 'projectile)

(setq projectile-completion-system 'helm)
(projectile-mode)

(provide 'projectile-init)
;;; projectile-init.el ends here
