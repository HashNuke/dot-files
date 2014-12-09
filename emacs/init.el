(require 'package)
(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; (setq package-enable-at-startup nil)


(defvar required-packages
  '(
    coffee-mode
    elixir-mode
    smex  ;;smex-hook
    switch-window
    ack
    whole-line-or-region ;; whole-line-or-region-hook
    markdown-mode        ;; markdown-mode-hook
    color-theme          ;; color-theme-hook
    erlang               ;; erlang-mode-hook
    simp
    magit                ;; magit-hook
    css-mode             ;; css-mode-hook
    js3-mode             ;; js3-mode-hook
    haml-mode
    textmate
    find-file-in-project
    scss-mode
    ruby-mode     ;; ruby-mode-hook
    yaml-mode     ;; yaml-mode-hook
    rhtml-mode
   ) "a list of packages to ensure are installed at launch.")


; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))




;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Highlight matching paranthesis
(show-paren-mode t)

;; spaces, not tabs
(setq-default indent-tabs-mode nil)

;; no splash screen
(setq inhibit-splash-screen t)

;; show line numbers
(line-number-mode 1)

;; Show column numbers in mode-line
(column-number-mode 1)

;; line number format
(setq linum-format "%d  ")

;; Hide the menu bar
(menu-bar-mode 0)

(blink-cursor-mode t)

;; Highlight current line
(global-hl-line-mode)

;; Auto-revert changed files to on-disk state
(global-auto-revert-mode 1)


;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)

(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
;;(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; this takes care of the meta-key
;; (global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\C-c\C-m" 'execute-extended-command)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
          (remove-if-not 'buffer-file-name (buffer-list)))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(global-set-key [f11] 'fullscreen)

(global-linum-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(cua-mode)

;; alias "yes" and "no" to "y" and "n"
(defalias 'yes-or-no-p 'y-or-n-p)


;; Hooks

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun config-ido-mode ()
  (define-key ido-completion-map [tab] 'ido-complete))

(add-hook 'ido-setup-hook 'config-ido-mode)


(defun config-syntax ()
  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))

  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))

  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
)

(config-syntax)


(defun config-css-mode ()
  (setq css-indent-level 2)
  (setq css-indent-offset 2)
)

(add-hook 'css-mode-hook 'config-css-mode)


(defun config-js3-mode ()
  '(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
  '(js3-enter-indents-newline t) ; don't need to push tab before typing
  '(js3-indent-on-enter-key t)   ; fix indenting before moving on
)


(defun config-smex ()
  (require 'smex)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  (global-set-key [remap execute-extended-command] 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
)

(config-smex)

(defun config-markdown-mode ()
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
)

(config-markdown-mode)


(defun config-editor ()
  (global-set-key (kbd "C-x C-/") 'goto-last-change)
  (whole-line-or-region-mode t)
  (global-set-key (kbd "C-c t") 'color-theme-select)
  (global-set-key (kbd "C-x C-z") 'magit-status)


  ;; backward word kill
  (global-set-key "\C-w" 'backward-kill-word)
  (global-set-key "\C-x\C-k" 'kill-region)
  (global-set-key "\C-c\C-k" 'kill-region)
)

(config-editor)




(textmate-mode) ;; oh dear life saver

;; My first custom Emacs lisp function
(defun find-file-with-fresh-cache ()
  (interactive)
  (textmate-clear-cache)
  (textmate-goto-file)
  )

(global-set-key (kbd "C-x f") 'find-file-with-fresh-cache)
(electric-indent-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scss-compile-at-save nil)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
