(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

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

(setq make-backup-files nil)
(setq auto-save-default nil)

(cua-mode)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; alias "yes" and "no" to "y" and "n"
(defalias 'yes-or-no-p 'y-or-n-p)


;; hooks

(defun erlang-mode-hook ()
  (autoload 'erlang-mode "erlang-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
)

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (outline-minor-mode)
                               (local-set-key "\r" 'reindent-then-newline-and-indent)
                               (setq outline-regexp " *\\(def \\|class\\|module\\)")
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                                      ;; (rvm-activate-corresponding-ruby)
                               ;; (require 'inf-ruby)
                               ;; (require 'ruby-compilation)
                                      )))
(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-hook ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))



(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq el-get-sources
      '((:name el-get)
        (:name package)
        (:name helm)
        (:name switch-window)
        (:name ack)
        (:name smex
               :type elpa
               :after (progn
                        (package-initialize)
                        (setq smex-save-file (concat user-emacs-directory ".smex-items"))
                        (smex-initialize)
                        (global-set-key (kbd "C-x C-m") 'execute-extended-command)
                        (global-set-key [remap execute-extended-command] 'smex)
                        (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

        ;; base for all color themes
        (:name color-theme
               :after (progn
                        (global-set-key (kbd "C-c t") 'color-theme-select)))

        (:name color-theme-solarized
               :type elpa)

        (:name erlang
               :type elpa
               :after (progn (erlang-mode-hook)))

        (:name whole-line-or-region
               :type elpa
               ;; :features whole-line-or-region
               :after (progn
                        (package-initialize)
                        (whole-line-or-region-mode t)))
        
        (:name magit
               :after (global-set-key (kbd "C-x C-z") 'magit-status))
	
      	;; (:name lisppaste :type elpa)
      	(:name emacs-goodies-el)

        (:name css-mode :type elpa
               :after (progn (rhtml-mode-hook)))

        (:name js3-mode
               :type git
               :url "git://github.com/thomblake/js3-mode.git"
               :load "js3.el")

        (:name haml-mode
               :type git
               :url "git://github.com/nex3/haml-mode.git"
               :load "haml-mode.el")

        (:name find-file-in-project
               :type git
               :url "git://github.com/technomancy/find-file-in-project.git"
               :load "find-file-in-project.el")

        (:name markdown-mode
               :type git
               :url "git://github.com/defunkt/markdown-mode.git"
               :load "markdown-mode.el")

        (:name scss-mode
               :type git
               :url "git://github.com/antonj/scss-mode.git"
               :load "scss-mode.el")

        (:name ruby-mode
               :type elpa
               :after (progn (ruby-mode-hook)))

        (:name yaml-mode
               :type git
               :url "git://github.com/yoshiki/yaml-mode.git"
               :after (progn (yaml-mode-hook)))

        (:name rhtml
               :type git
               :url "https://github.com/eschulte/rhtml.git"
               :features rhtml-mode
               :after (progn (rhtml-mode-hook)))

        (:name helm-cmd-t
               :type git
               :url "git://github.com/lewang/helm-cmd-t.git"
               :load "helm-cmd-t.el")

        (:name coffee-mode
               :type git
               :url "git://github.com/defunkt/coffee-mode.git"
               :load "coffee-mode.el")

        (:name goto-last-change    ; move pointer back to last change
               :after (progn (lambda ()
                               ;; when using AZERTY keyboard, consider C-x C-_
                               (global-set-key (kbd "C-x C-/") 'goto-last-change))))
        ))

(setq
 my:el-get-packages
 '(el-get
   css-mode
   haml-mode
   sass-mode
   markdown-mode
   color-theme
   ruby-mode
   yaml-mode
   coffee-mode
   rhtml-mode
   js3-mode
   erlang
   find-file-in-project
   ))

(setq helm-ff-lynx-style-map nil
      helm-input-idle-delay 0.1
      helm-idle-delay 0.1
      )

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

(el-get 'sync my:el-get-packages)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; (helm-mode 1)
; (global-set-key (kbd "C-c h") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)


'(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
'(js3-enter-indents-newline t) ; don't need to push tab before typing
'(js3-indent-on-enter-key t)   ; fix indenting before moving on

(global-linum-mode t)

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
