(load "haskell-mode-autoloads")

; activate haskell-mode for the happy files
(add-to-list 'auto-mode-alist '("\\.y\\'" . haskell-mode))
; activate haskell-mode for the alex files
(add-to-list 'auto-mode-alist '("\\.x\\'" . haskell-mode))

(autoload 'ghc-init  "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(require 'haskell-interactive-mode)
(require 'haskell-process)

;; Customization
(custom-set-variables
 ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
 ;;'(haskell-process-type 'cabal-dev)

 ;; Use notify.el (if you have it installed) at the end of running
 ;; Cabal commands or generally things worth notifying.
 '(haskell-notify-p t)

 ;; To enable tags generation on save.
 '(haskell-tags-on-save t)

 ;; To enable stylish on save.
 '(haskell-stylish-on-save t)

 '(haskell-process-type 'cabal-repl))

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  ;; Use simple indentation.
  ;;(turn-on-haskell-simple-indent)
  (turn-on-haskell-indentation)
  ;;(turn-on-hi2)
  ;;(structured-haskell-mode)

  ;; Init ghc-mod
  (ghc-init)

  (setq tab-width 4)

  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)


  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1))))

(add-hook 'haskell-mode-hook 'haskell-hook)

(add-to-list 'warning-suppress-types '(stylish-haskell discard-error))
