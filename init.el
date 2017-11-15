;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;----------------
;; LOAD PACKAGES
;;----------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

; list the packages you want
(setq package-list
    '(async helm gruvbox-theme))

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

;; async
(require 'async)
(async-bytecomp-package-mode 1)

;; helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)


;;----------------
;; APPEARANCE
;;----------------
(load-theme 'gruvbox t)
(set-face-attribute 'default nil :font "Monaco-16")
(show-paren-mode 1)


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
