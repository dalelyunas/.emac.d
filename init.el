(defvar dalelyunas-temporary-file-directory (expand-file-name "~/emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode 1)
(column-number-mode 1)
(blink-cursor-mode 0)

(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

(add-hook
 'term-mode-hook
 (lambda() (setq show-trailing-whitespace nil)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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

;; install packages
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package helm
  :init (setq helm-M-x-fuzzy-match t
              helm-buffers-fuzzy-matching t
              helm-recentf-fuzzy-match t
              helm-autoresize-max-height 0
              helm-autoresize-min-height 20
              helm-follow-mode-persistent t)
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

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :hook ((typescript-mode . setup-tide-mode)
         (rjsx-mode . setup-tide-mode)
         (js2-mode . setup-tide-mode)))

(use-package js2-mode
  :mode "\\.js$"
  :init (setq js2-basic-offset 2
              js-indent-level 2))

(use-package rjsx-mode
  :mode "\\.jsx$")

(use-package json-mode
  :mode "\\.json$")

(use-package airline-themes
  :config (load-theme 'airline-light))

(set-face-attribute 'default nil :font "MesloLGLDZ Nerd Font-14")
