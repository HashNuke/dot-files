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

;; "Yes" or "No" should be "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Global key bindings
(global-set-key (kbd "M-i") 'imenu)

;; Activate all the packages
(package-initialize)

;; My list of packages
(setq package-list '(helm))

;; Refresh package archive contents
(unless package-archive-contents
  (package-refresh-contents))

;; Loop thru my list of packages
;; and ensure they are installed
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'helm-config)
