;;; helm-init.el --- initialize helm related functionality

;;; Commentary:

;;; Code:
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(require 'helm-ag)

;; keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-autoresize-max-height 0
      helm-autoresize-min-height 20)

(helm-autoresize-mode 1)

(helm-projectile-on)

(helm-mode 1)

(provide 'helm-init)
;;; helm-init.el ends here

