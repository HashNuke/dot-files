(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Highlight matching paranthesis
(show-paren-mode t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

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

;; No scrollbar
(scroll-bar-mode -1)

;; Highlight current line
(global-hl-line-mode)

;; Auto-revert changed files to on-disk state
(global-auto-revert-mode 1)

;; backward word kill
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

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

(defun cfg-ido-mode ()
  (define-key ido-completion-map [tab] 'ido-complete))

(add-hook 'ido-setup-hook 'cfg-ido-mode)


(defun cfg-erlang-mode ()
  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
)

(cfg-erlang-mode)


(defun cfg-ruby-mode ()
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
)

(cfg-ruby-mode)


(defun cfg-rhtml-mode ()
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
)

(cfg-rhtml-mode)


(defun cfg-yaml-mode ()
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
)

(cfg-yaml-mode)


(defun cfg-css-mode ()
  (setq css-indent-level 2)
  (setq css-indent-offset 2)
)

(add-hook 'css-mode-hook 'cfg-css-mode)


(defun cfg-js3-mode ()
  '(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
  '(js3-enter-indents-newline t) ; don't need to push tab before typing
  '(js3-indent-on-enter-key t)   ; fix indenting before moving on
)

(add-hook 'js3-mode-hook 'cfg-js3-mode)


(defun cfg-smex ()
  (require 'smex)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  (global-set-key [remap execute-extended-command] 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
)

(cfg-smex)

(defun cfg-markdown-mode ()
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
)

(cfg-markdown-mode)


(defun cfg-helm ()
  (setq
    helm-ff-lynx-style-map nil
    helm-input-idle-delay 0.1
    helm-idle-delay 0.1
  )
)

(cfg-helm)


(defun cfg-goto-last-change ()
  (global-set-key (kbd "C-x C-/") 'goto-last-change))

(cfg-goto-last-change)


(defun cfg-whole-line-or-region ()
  (whole-line-or-region-mode t))

(cfg-whole-line-or-region)


(defun cfg-color-theme ()
  (global-set-key (kbd "C-c t") 'color-theme-select))

(cfg-color-theme)


(defun cfg-magit ()
  (global-set-key (kbd "C-x C-z") 'magit-status))

(cfg-magit)


(defun cfg-simp ()
  (require 'simp)
  (simp-project-define
   '(:has (.git)
          :ignore (tmp coverage log vendor .git public/system public/assets)))

  (global-set-key (kbd "C-c f") 'simp-project-find-file)
  (global-set-key (kbd "C-c d") 'simp-project-root-dired)
  (global-set-key (kbd "C-c s") 'simp-project-rgrep)
  (global-set-key (kbd "C-c S") 'simp-project-rgrep-dwim)
  (global-set-key (kbd "C-c b") 'simp-project-ibuffer-files-only)
  (global-set-key (kbd "C-c B") 'simp-project-ibuffer)
  (global-set-key (kbd "C-c C-f") 'simp-project-with-bookmark-find-file)
  (global-set-key (kbd "C-c C-s") 'simp-project-with-bookmark-rgrep)
  (global-set-key (kbd "C-c C-b") 'simp-project-with-bookmark-ibuffer)
)

(cfg-simp)


(setq my-packages
'(
    coffee-mode
    helm  ;; helm-hook
    switch-window
    ack
    smex ;;smex-hook
    whole-line-or-region ;; whole-line-or-region-hook
    color-theme ;; color-theme-hook
    erlang ;; erlang-mode-hook
    simp
    magit ;; magit-hook
    css-mode ;;css-mode-hook
    js3-mode ;;js3-mode-hook
    haml-mode
    textmate
    find-file-in-project
    markdown-mode ;; markdown-mode-hook
    scss-mode
    ruby-mode   ;; ruby-mode-hook
    yaml-mode   ;; yaml-mode-hook
    rhtml-mode  ;; rhtml-mode-hook
    clojure-mode
    yasnippet
  )
)


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)
  ))

; (helm-mode 1)
; (global-set-key (kbd "C-c h") 'helm-mini)
; (global-set-key (kbd "C-x C-f") 'helm-find-files)


(textmate-mode) ;; oh dear life saver
(global-set-key (kbd "C-x f") 'textmate-goto-file)
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
