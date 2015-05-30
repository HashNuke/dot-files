(setq package-archives
      `(("gnu" . "http://elpa.gnu.org/packages")
	("marmalage" . "http://marmalde-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(ido-mode)
(setq ido-enable-flex-matching t)

;; Disable backup file creation
(setq auto-save-default nil)
(setq make-backup-files nil)

;; disable menu bar
(menu-bar-mode -1)

;; Don't start with the "GNU Emacs" buffer
(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)
