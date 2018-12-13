;;; javascript-init.el --- initialize javascript related functionality

;;; Commentary:

;;; Code:
(require 'js2-mode)
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

;; make eslint faster by disabling --print-config
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


(setq js-indent-level 2)

;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

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

(provide 'javascript-init)
;;; javascript-init.el ends here
