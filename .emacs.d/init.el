(setq package-archives
      `(("gnu" . "http://elpa.gnu.org/packages")
	("marmalage" . "http://marmalde-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(set-terminal-parameter nil 'background-mode 'dark)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)


;;;;;;;;;;;;;;;; GLOBAL CONFIG ;;;;;;;;;;;;;;;;

(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)

;; change buffers when files change on disk
(global-auto-revert-mode t)

;; Disable backup file creation
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Disable menu bar
(menu-bar-mode -1)

;; Don't start with the "GNU Emacs" buffer
(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)

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
        yaml-mode
        helm
        js2-mode magit
        scss-mode
        projectile
        helm-projectile
        markdown-mode
        elixir-mode
        web-mode
        emmet-mode
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
(setq magit-last-seen-setup-instructions "1.4.0")


;;(set-face-foreground 'minibuffer-prompt "white")

;; require packages
(setq require-list
      '(
        yaml-mode
        helm-config
        magit
        projectile
        helm-projectile
        markdown-mode
        elixir-mode
        web-mode
        emmet-mode
        alchemist))

(dolist (require-item require-list)
  (require require-item))

(projectile-global-mode)
(helm-projectile-on)

;; disable vc-git because magit is enabled
(setq vc-handled-backends ())

;; Global key bindings
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-x f") 'helm-find)

;; tabs and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)


;; for default javascript mode
(setq js-indent-level 2)

(defun config-js2-mode ()
  (setq-default js2-basic-offset 2)
  )

(add-hook 'js2-mode-hook 'config-js2-mode)


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
  ;; (setq web-mode-style-padding 2
  ;;       web-mode-script-padding 2
  ;;       web-mode-css-indent-offset 2
  ;;       web-mode-block-padding 2)
  )

(add-hook 'sh-mode-hook 'config-web-mode)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; when opening a file, don't change working dir to parent dir of the file
;; (add-hook 'find-file-hook
;;          (lambda ()
;;            (setq default-directory command-line-default-directory)))


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
