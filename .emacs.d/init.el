;;
;; init.el
;;

;; Language.
(set-language-environment 'Japanese)

;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; display line number
(global-linum-mode t)
;; tab
(setq-default indent-tabs-mode nil)
;; disable magic comment
(setq ruby-insert-encoding-magic-comment nil)

;; for package.el

(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; for helm
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-o") 'helm-for-files)


;; turn off auto save and auto backup
(setq make-backup-files nil)
(setq auto-save-default nil)

