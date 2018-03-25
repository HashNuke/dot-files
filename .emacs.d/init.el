(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/package") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marlalade" . "https://marmalde-repo.org/packages/") t)
  )

(set-terminal-parameter nil 'background-mode 'dark)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;; (load-theme 'solarized t)


;;;;;;;;;;;;;;;; GLOBAL CONFIG ;;;;;;;;;;;;;;;;


(set-face-attribute 'default nil :font "14")
(set-frame-font "16" nil t)

(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)

;; change buffers when files change on disk
(global-auto-revert-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e" default)))
 '(fci-rule-color "#3E4451")
 '(package-selected-packages
   (quote
    (swiper helm-ag hackernews yasnippet yaml-mode writeroom-mode web-mode scss-mode markdown-mode magit js2-mode helm-projectile emmet-mode alchemist)))
 '(with-editor-emacsclient-executable "emacsclient"))

;; Disable backup file creation
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Disable menu bar
(menu-bar-mode -1)

;; Don't start with the "GNU Emacs" buffer
(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)


;; always open buffer on the bottom instead of the right
(setq split-width-threshold most-positive-fixnum)

;; enable column numbers
(column-number-mode)

;; "Yes" or "No" should be "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;; PACKAGE INSTALL/REQUIRE ;;;;;;;;;;;

;; Activate all the packages
(package-initialize)

;; My list of packages
(setq package-list
      '(
        iedit
        swiper
        helm-ag
        yasnippet
        yaml-mode
        helm
        js2-mode
        magit
        scss-mode
        projectile
        helm-projectile
        markdown-mode
        elixir-mode
        emmet-mode
        writeroom-mode
        web-mode
        alchemist))

;; Refresh package archive contents
(unless package-archive-contents
  (package-refresh-contents))

;; Loop thru my list of packages
;; and ensure they are installed
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; quiet magit
;; (setq magit-emacsclient-executable "emacsclient")
;; (setq magit-last-seen-setup-instructions "1.4.0")


;;(set-face-foreground 'minibuffer-prompt "white")

;; require packages
(setq require-list
      '(
        iedit
        swiper
        helm-ag
        yasnippet
        yaml-mode
        helm-config
        magit
        projectile
        helm-projectile
        markdown-mode
        elixir-mode
        emmet-mode
        web-mode
        alchemist))

(dolist (require-item require-list)
  (require require-item))

(yas-global-mode t)

(projectile-global-mode)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(setq projectile-use-git-grep 1)
(helm-projectile-on)

;;enable line numbers globally
(global-linum-mode t)


;; space after line number
(setq linum-format "%d ")

;; disable vc-git because magit is enabled
(setq vc-handled-backends ())

;; Global key bindings
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-x f") 'helm-find)
(global-set-key (kbd "C-s") 'swiper-all)

;; tabs and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(setq-default c-basic-offset 4)
(defvaralias 'cperl-indent-level 'tab-width)

;; for default javascript mode
(setq js-indent-level 2)

(defun config-js2-mode ()
  (setq-default js2-basic-offset 2)
  )

(add-hook 'js2-mode-hook 'config-js2-mode)


(defun config-markdown-mode ()
  (setq tab-width 2)
  )

(add-hook 'markdown-mode-hook 'config-markdown-mode)
(add-hook 'gfm-mode-hook 'config-markdown-mode)

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)


(defun config-shell-mode ()
  (setq sh-basic-offset 2
        sh-indentation 2)
  )

(add-hook 'sh-mode-hook 'config-shell-mode)


(defun config-css-mode ()
  (setq css-indent-level 2)
  (setq css-indent-offset 2)
  )

(add-hook 'css-mode-hook 'config-css-mode)


(defun config-web-mode ()
  (setq web-mode-code-indent-offset 2)
  ;;(setq web-mode-style-padding 2)
  ;;(setq web-mode-script-padding 2
  ;;(setq web-mode-css-indent-offset 2)
  ;;(setq web-mode-block-padding 2)
  )

(add-hook 'web-mode-hook 'config-web-mode)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; when opening a file, don't change working dir to parent dir of the file
;; (add-hook 'find-file-hook
;;          (lambda ()
;;            (setq default-directory command-line-default-directory)))



;; let the terminal decide the color scheme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'wombat t)

(defun orgtbl-to-gfm (table params)
  "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
  (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                               org-table-last-alignment ""))
         (params2
          (list
           :splice t
           :hline (concat alignment "|")
           :lstart "| " :lend " |" :sep " | ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))


(when (display-graphic-p)
  (tool-bar-mode -1)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-enabled-themes (quote (tango-dark))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
