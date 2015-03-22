(load "haskell-mode-autoloads")

(require 'haskell-interactive-mode)
(require 'haskell-process)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

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

 ;; Haskell process settings
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type 'cabal-repl))

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  ;; Use simple indentation.
  ;;(turn-on-haskell-simple-indent)
  (turn-on-haskell-indentation)
  ;;(define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)

  (interactive-haskell-mode)

  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

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

;; suppress annoying stylish-haskell warning pop-ups
(add-to-list 'warning-suppress-types '(stylish-haskell discard-error))
