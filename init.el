;;; init.el --- initialize emacs

;;; Commentary:

;;; Code:
;; code to load other init files
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; load packages if they are missing
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; list the packages you want
(setq package-list
      '(async helm gruvbox-theme company js2-mode
	      flycheck json-mode helm-ag
              exec-path-from-shell git-gutter projectile helm-projectile
              vue-mode elpy sml-mode dumb-jump ag))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; async package for loading
(require 'async)
(async-bytecomp-package-mode 1)

;; configure the rest of the packages
(elpy-enable)
(load-user-file "lisp/settings-init.el")
(load-user-file "lisp/flycheck-init.el")
(load-user-file "lisp/flycheck-sml.el")
(load-user-file "lisp/helm-init.el")
(load-user-file "lisp/javascript-init.el")
(load-user-file "lisp/projectile-init.el")
(load-user-file "lisp/dumb-jump-init.el")

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-follow-mode-persistent t)
 '(package-selected-packages (quote (gruvbox-theme helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
