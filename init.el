;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode 1)
(setq-default show-trailing-whitespace t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; save and backup files in tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;----------------
;; LOAD PACKAGES
;;----------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

; list the packages you want
(setq package-list
      '(async helm gruvbox-theme company smartparens js2-mode js2-refactor
	      xref-js2 flycheck json-mode
              magit exec-path-from-shell git-gutter projectile helm-projectile
              vue-mode multi-term elpy sml-mode 2048-game))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;----------------
;; SETUP PACKAGES
;;----------------

;; PATH
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ELPY
(elpy-enable)

;; ASYNC
(require 'async)
(async-bytecomp-package-mode 1)

;; JS2-MODE
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(setq js-indent-level 2)

;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; SMARTPARENS
;; (require 'smartparens)
;; (smartparens-global-mode 1)

;; PROJECTILE
(require 'projectile)

(setq projectile-completion-system 'helm)
(projectile-mode)

;; MULTI-TERM
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;; HELM
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)

;; keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-projectile-on)

(helm-mode 1)

;; LINTING
(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

(add-hook 'js2-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; use local eslint instead of global
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; make eslint faster by disabling --print-config
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

;; GIT
(require 'magit)
(global-git-gutter-mode +1)

;; SML
(require 'sml-mode)

;;----------------
;; APPEARANCE
;;----------------

(load-theme 'gruvbox t)
(set-face-attribute 'default nil :font "MesloLGLDZ Nerd Font-14")


;;----------------
;; CUSTOM VARIABLES
;;----------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (gruvbox-theme helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
