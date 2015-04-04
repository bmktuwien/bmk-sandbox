(require 'semantic/ia)
(require 'semantic/symref)
(require 'semantic/bovine/gcc)

;;----------------------------------------------------------

(defun ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include"))

(defun add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic-raw)
  (local-set-key (kbd "M-.") 'semantic-ia-fast-jump))

(defun my-common-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset  4)

  (semantic-mode 1)
  (ede-mode 1)

  (semantic-idle-scheduler-mode)
  (semantic-idle-completions-mode)
  (semantic-decoration-mode)
  (semantic-highlight-func-mode)
  (semantic-show-unmatched-syntax-mode)

  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  (semantic-add-system-include "/usr/include" 'c-mode)
  (semantic-add-system-include "/usr/include" 'c++-mode)

  (add-semantic-to-autocomplete)
  (ac-c-header-init))

(defun my-c-mode-hook ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))


;; Install hooks
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-common-hook 'my-common-c-mode-hook)
