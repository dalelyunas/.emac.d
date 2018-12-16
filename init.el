
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

(setq-default show-trailing-whitespace t)

(add-hook
 'term-mode-hook
 (lambda() (setq show-trailing-whitespace nil)))

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default 1)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package helm
             :init (setq helm-M-x-fuzzy-match t
                         helm-buffers-fuzzy-matching t
                         helm-recentf-fuzzy-match t
                         helm-autoresize-max-height 0
                         helm-autoresize-min-height 20)
             :bind (("M-x" . helm-M-x)
                    ("M-y" . helm-show-kill-ring)
                    ("C-x b" . helm-mini)
                    ("C-x C-f" . helm-find-files)
                    ("C-c h o" . helm-occur)
                    ("C-h SPC" . helm-all-mark-rings))
             :config
             (helm-autoresize-mode 1)
             (helm-mode 1))

(use-package ag)

(use-package helm-ag)

(use-package elpy
             :config (elpy-enable))

(use-package dumb-jump
             :init (setq dumb-jump-selector 'helm)
             :config (dumb-jump-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package company
             :hook (after-init . global-company-mode)
             :config (company-mode))

(use-package git-gutter
             :config (global-git-gutter-mode 1))

(use-package doom-themes
             :config (load-theme 'doom-one t))

(set-face-attribute 'default nil :font "MesloLGLDZ Nerd Font-14")

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-follow-mode-persistent t)
 '(helm-source-names-using-follow (quote ("Search at ~/.emacs.d/")))
 '(package-selected-packages (quote (gruvbox-theme helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
