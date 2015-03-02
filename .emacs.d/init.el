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

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; install packages

(unless (package-installed-p 'helm)
  (package-refresh-contents) (package-install 'helm))

(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))
  
(unless (package-installed-p 'ensime)
  (package-refresh-contents) (package-install 'ensime))



;; for helm
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-o") 'helm-mini)


;; turn off auto save and auto backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; backspace
(keyboard-translate ?\C-h ?\C-?)

;; help
(define-key global-map (kbd "C-x ?") 'help-command)

;; switch tab
(define-key global-map (kbd "C-t") 'other-window)

(electric-indent-mode 0)

(setenv "PATH" (concat "/usr/local/bin/sbt:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin/scala:" (getenv "PATH")))

;; scala-mode2
(require 'scala-mode2)

;; ensime
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)



