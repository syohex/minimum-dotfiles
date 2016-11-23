;; (package-initialize)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

(require 'cl-lib)
(require 'subr-x)

(custom-set-variables
 '(electric-indent-mode nil)
 '(parens-require-spaces nil))

;; configuration based on Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(custom-set-variables
 '(evil-move-cursor-back nil)
 '(evil-search-module 'evil-search))

;; eldoc
(custom-set-variables
 '(eldoc-idle-delay 0.2))

;; paredit
(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;; electric-pair(like autopair)
(let ((modes '(c-mode
               c++-mode
               python-mode
               ruby-mode
               sh-mode
               js-mode
               go-mode
               css-mode
               cmake-mode
               markdown-mode
               gfm-mode)))
  (dolist (mode my/electric-pair-enabled-modes)
    (add-hook (intern (format "%s-hook" mode)) 'electric-pair-local-mode)))

;; smartrep
(require 'smartrep)

;; for GC
(setq gc-cons-threshold (* gc-cons-threshold 10)
      echo-keystrokes 0
      large-file-warning-threshold (* 25 1024 1024))

(setq-default indent-tabs-mode nil)

;; saveplace
(savehist-mode 1)
(save-place-mode +1)

;; hippie-expand
(custom-set-variables
 '(hippie-expand-verbose nil)
 '(hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-complete-file-name
     try-complete-file-name-partially
     try-expand-dabbrev-all-buffers)))

;; my key mapping
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "ESC M-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-x %") 'anzu-replace-at-cursor-thing)
(global-set-key (kbd "C-M-l") 'goto-line)
(global-set-key (kbd "C-M-z") 'helm-resume)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-x C-c") 'helm-M-x)
(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(global-set-key (kbd "M-g M-f") 'ffap)

(setq dabbrev-case-fold-search nil)

;; info for japanese
(auto-compression-mode t)

;; Coloring
(global-font-lock-mode t)

;; not highlight region
(transient-mark-mode nil)

;; indicate last line
(setq-default indicate-empty-lines t
	      indicate-buffer-boundaries 'right)

;; not create backup file
(setq backup-inhibited t
      delete-auto-save-files t)

;; Disable menu bar
(menu-bar-mode -1)

;; show paren
(show-paren-mode 1)
(custom-set-variables
 '(show-paren-delay 0)
 '(show-paren-style 'expression))

;; not beep
(setq ring-bell-function #'ignore)

;; not display start message
(setq inhibit-startup-message t)

;; display line infomation
(line-number-mode 1)
(column-number-mode 1)

;; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))
(defalias 'exit 'save-buffers-kill-emacs)

;; ignore upper or lower
(setq read-file-name-completion-ignore-case t)

;; Delete key
(global-set-key [delete] 'delete-char)

;; backspace
(when (not window-system)
  (normal-erase-is-backspace-mode 0))

(custom-set-variables
 '(c-basic-offset 8))

(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "M-q") 'nil)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file))

(defun my/c-mode-hook ()
  (c-set-style "k&r")
  (hs-minor-mode 1)
  (c-toggle-electric-state -1)

  (setq c-basic-offset 8)
  (setq indent-tabs-mode t)

  ;; company
  (add-to-list 'company-backends 'company-clang)
  (helm-gtags-mode))

(add-hook 'c-mode-hook 'my/c-mode-hook)

;; C++ coding style
(add-hook 'c++-mode-hook 'my/c-mode-hook)

;; asm-mode
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; ibuffer
(defalias 'list-buffers 'ibuffer)

(require 'undo-tree)
(setq undo-no-redo t
      undo-limit 600000
      undo-strong-limit 900000)
(global-set-key (kbd "M-/") 'undo-tree-redo)

(smartrep-define-key
    undo-tree-map "C-x" '(("u" . 'undo-tree-undo)
                          ("U" . 'undo-tree-redo)))

;;;; dired
(require 'dired)
;; Not create new buffer, if you chenge directory in dired
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
(define-key dired-mode-map (kbd "K") 'dired-k)
;; display directories by first
(load-library "ls-lisp")

;; recursive copy, remove
(custom-set-variables
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(ls-lisp-dirs-first t))

;; dired-x
(load "dired-x")

;; helm
(require 'helm-config)
(require 'helm)

(define-key helm-map (kbd "C-q") 'helm-execute-persistent-action)

(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-command-prefix-key nil)
 '(helm-candidate-number-limit 500))

(require 'helm-descbinds)
(helm-descbinds-install)

;; gtags
(custom-set-variables
  '(helm-gtags-pulse-at-cursor nil))

(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack))

(custom-set-variables
 '(helm-ag-insert-at-point 'symbol))

;; helm in dired
(setq-default split-width-threshold 0)

(define-key helm-map (kbd "C-p") 'helm-previous-line)
(define-key helm-map (kbd "C-n") 'helm-next-line)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)

;; helm-show-kill-ring
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)

;; apropos with helm
(global-set-key (kbd "C-h a") 'helm-apropos)

(global-set-key (kbd "M-g M-i") 'import-popwin)

;; helm faces
(require 'helm-files)
(custom-set-variables
 '(helm-find-files-doc-header ""))

;; naming of same name file
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq recentf-max-saved-items 1000)
(require 'recentf)
(run-at-time t 600 'recentf-save-list)
(setq recentf-exclude '(".recentf" "/repos/" "/elpa/" "CMakeCache.txt"
                        "\\.mime-example" "\\.ido.last" "/tmp/gomi/" "/.cpanm/"))
(recentf-mode +1)

;; for virsion control system
(global-auto-revert-mode 1)
(setq auto-revert-interval 10
      vc-follow-symlinks t
      auto-revert-check-vc-info t)

;; disable vc-mode
(setq vc-handled-backends '(Git))

;; which-func
(require 'which-func)
(set-face-foreground 'which-func "chocolate4")
(set-face-bold-p 'which-func t)
(which-func-mode t)

;; view-mode
(setq view-read-only t)

;; for regexp color
(set-face-foreground 'font-lock-regexp-grouping-backslash "#ff1493")
(set-face-foreground 'font-lock-regexp-grouping-construct "#ff8c00")

;; popwin
(require 'popwin)
(global-set-key (kbd "M-z") popwin:keymap)
(defvar popwin:special-display-config-backup popwin:special-display-config)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:special-display-config
      (append '(("*Apropos*") ("*sdic*") ("*Faces*") ("*Colors*"))
              popwin:special-display-config))

;; use popwin
(push '(Man-mode :stick t :height 20) popwin:special-display-config)

;; for symboliclink
(setq-default find-file-visit-truename t)

;; Ctrl-q map
(defvar my/ctrl-q-map (make-sparse-keymap)
  "My original keymap binded to C-q.")
(defalias 'my/ctrl-q-prefix my/ctrl-q-map)
(define-key global-map (kbd "C-q") 'my/ctrl-q-prefix)
(define-key my/ctrl-q-map (kbd "C-q") 'quoted-insert)

;; goto-chg
(smartrep-define-key
    global-map "C-q" '(("-" . 'goto-last-change)
                       ("+" . 'goto-last-change-reverse)))

(global-set-key (kbd "C-x v d") 'vc-diff)

;; company-mode
(require 'company)
(custom-set-variables
 '(company-idle-delay nil))

(global-company-mode +1)
(global-set-key (kbd "C-M-i") 'company-complete)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

(define-key lisp-interaction-mode-map (kbd "C-M-i") 'company-elisp)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

(set-face-attribute 'company-tooltip nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common nil
                    :foreground "black" :background "lightgrey")
(set-face-attribute 'company-tooltip-common-selection nil
                    :foreground "white" :background "steelblue")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "black" :background "steelblue")
(set-face-attribute 'company-preview-common nil
                    :background nil :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg nil
                    :background "orange")
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray40")

;; yasnippet
(autoload 'yas-minor-mode-on "yasnippet" nil t)
(with-eval-after-load 'yasnippet
  (setq	yas-prompt-functions '(my-yas/prompt))
  (yas-reload-all))

;; enable yasnippet mode
(dolist (hook '(c-mode-hook
                c++-mode-hook
                sh-mode-hook))
  (add-hook hook 'yas-minor-mode-on))

;; buffer-mode
(global-set-key (kbd "M-g h") 'buf-move-left)
(global-set-key (kbd "M-g j") 'buf-move-down)
(global-set-key (kbd "M-g i") 'buf-move-up)
(global-set-key (kbd "M-g l") 'buf-move-right)

;; anzu
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-replace-to-string-separator " => "))
(set-face-attribute 'anzu-mode-line nil :foreground nil)

;; editutil
(require 'editutil)
(editutil-default-setup)

;; view-mode
(custom-set-variables
 '(view-read-only t))

(defun my/git-commit-mode-hook ()
  (when (looking-at "\n")
    (open-line 1)))

(with-eval-after-load 'git-commit-mode
  (add-hook 'git-commit-mode-hook 'my/git-commit-mode-hook))

(defadvice git-commit-commit (after move-to-magit-buffer activate)
  (delete-window))

;; git-gutter
(global-git-gutter-mode t)
(custom-set-variables
 '(git-gutter:lighter " GG"))

(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x v u") 'git-gutter)
(smartrep-define-key
 global-map "C-x" '(("n" . 'git-gutter:next-hunk)
		    ("p" . 'git-gutter:previous-hunk)))

;; eshell
(custom-set-variables
 '(eshell-banner-message "")
 '(eshell-cmpl-cycle-completions nil)
 '(eshell-hist-ignoredups t)
 '(eshell-scroll-show-maximum-output nil))

(defun my/eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history))

(add-hook 'eshell-mode-hook 'my/eshell-mode-hook)
(global-set-key (kbd "C-\\") 'eshellutil-popup)

(with-eval-after-load 'diff-mode
  (set-face-attribute 'diff-added nil
		      :background nil :foreground "green"
		      :weight 'normal)
  (set-face-attribute 'diff-removed nil
		      :background nil :foreground "red"
		      :weight 'normal)

  (set-face-attribute 'diff-refine-changed nil
		      :background nil :foreground "yellow"
		      :weight 'normal)

  (set-face-attribute 'diff-file-header nil
		      :foreground "white"
		      :background nil :weight 'extra-bold)

  (set-face-attribute 'diff-function nil
		      :foreground "cyan"
		      :background nil
		      :underline nil)

  (set-face-attribute 'diff-header nil
		      :background nil
		      :underline nil)

  (set-face-attribute 'diff-hunk-header nil
		      :foreground "yellow"
		      :background nil
		      :weight 'bold
		      :underline t))

(progn
  (set-face-attribute 'helm-selection nil
		      :foreground "pink" :background "black")
  (set-face-attribute 'helm-ff-file nil
		      :foreground "white" :weight 'normal)
  (set-face-attribute 'helm-ff-directory nil
		      :foreground "cyan" :background nil)

  (set-face-attribute 'show-paren-match nil
		      :background nil :foreground nil
		      :underline t :weight 'bold)
  (set-face-background 'show-paren-match nil))


;; Go Lang
(custom-set-variables
 '(gofmt-command "goimports"))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook 'my/go-mode-hook)
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  (add-to-list 'company-backends 'company-go)

  (define-key go-mode-map (kbd "C-c C-a") 'helm-godoc-import)
  (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
  (define-key go-mode-map (kbd "C-c C-c") 'my/flycheck-list-errors)
  (define-key go-mode-map (kbd "C-c C-s") 'my/gofmt)
  (define-key go-mode-map (kbd "C-c C-t") 'ff-find-other-file)
  (define-key go-mode-map (kbd "C-c C-d") 'helm-godoc)
  (define-key go-mode-map (kbd "C-c ?") 'my/godoc-type-at-cursor)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

  (define-key go-mode-map (kbd ":") nil))

(defun my/godoc-type-at-cursor ()
  (interactive)
  (save-excursion
    (unless (looking-at-p "\\>")
      (forward-word 1))
    (let ((cand (go-eldoc--invoke-autocomplete)))
      (when (and cand (string-match "\\`\\([^,]+\\),,\\(.+\\)$" cand))
        (let ((name (match-string-no-properties 1 cand))
              (type (match-string-no-properties 2 cand)))
          (when (string-match "\\`var\\(.+\\)" type)
            (setq type (match-string-no-properties 1 type)))
          (message "%s:%s" (propertize name 'face 'font-lock-type-face) type))))))

(defun my/gofmt ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (gofmt)
  (when (buffer-modified-p)
    (save-buffer)))

(defun my/go-mode-hook ()
  (setq compile-command "go test"))

(add-to-list 'auto-mode-alist
	     '("\\.\\(pl\\|pm\\|cgi\\|t\\|psgi\\)\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile\\'" . cperl-mode))
(defalias 'perl-mode 'cperl-mode)

(with-eval-after-load 'cperl-mode
  (cperl-set-style "PerlStyle")
  (setq cperl-auto-newline nil)

  ;; bindings
  (define-key cperl-mode-map "\177" nil)
  (define-key cperl-mode-map (kbd ";") nil)
  (define-key cperl-mode-map (kbd ":") nil)
  (define-key cperl-mode-map (kbd "(") nil)
  (define-key cperl-mode-map (kbd "{") nil)
  (define-key cperl-mode-map (kbd "}") nil)
  (define-key cperl-mode-map (kbd "[") nil))

(custom-set-variables
 '(cperl-indent-parens-as-block t)
 '(cperl-close-paren-offset -4)
 '(cperl-indent-subs-specially nil))

;; python
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
