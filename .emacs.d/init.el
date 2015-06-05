(setq package-archives
      `(("gnu" . "http://elpa.gnu.org/packages")
	("marmalage" . "http://marmalde-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)

;; Disable backup file creation
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Disable menu bar
(menu-bar-mode -1)

;; Don't start with the "GNU Emacs" buffer
(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)

;; enable column numbers
(column-number-mode)

;; "Yes" or "No" should be "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Global key bindings
(global-set-key (kbd "M-i") 'imenu)

;; Activate all the packages
(package-initialize)

;; My list of packages
(setq package-list '(helm js2-mode magit))

;; Refresh package archive contents
(unless package-archive-contents
  (package-refresh-contents))

;; Loop thru my list of packages
;; and ensure they are installed
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'helm-config)
(require 'magit)

;; tabs and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; for default javascript mode
(setq js-indent-level 2)

(defun js2-mode-hook ()
  (setq-default js2-basic-offset 2)
  )

(add-hook 'js2-mode-hook 'js2-mode-hook)


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq magit-last-seen-setup-instructions "1.4.0")
