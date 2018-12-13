;;; settings-init.el --- initialize emacs settings

;;; Commentary:

;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode 1)
(column-number-mode 1)

(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; save and backup files in tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-git-gutter-mode +1)

(setq-default show-trailing-whitespace t)

(add-hook
 'term-mode-hook
 (lambda() (setq show-trailing-whitespace nil)))

;; appearance
(load-theme 'gruvbox t)
(set-face-attribute 'default nil :font "MesloLGLDZ Nerd Font-14")


(provide 'settings-init)
;;; settings-init.el ends here
