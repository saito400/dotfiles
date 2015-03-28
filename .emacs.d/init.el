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
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; disable magic comment
(setq ruby-insert-encoding-magic-comment nil)
;;parenthesis
(show-paren-mode t)

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

(unless (package-installed-p 'helm-ls-git)
  (package-refresh-contents) (package-install 'helm-ls-git))

(unless (package-installed-p 'web-mode)
  (package-refresh-contents) (package-install 'web-mode))

(unless (package-installed-p 'go-mode)
  (package-refresh-contents) (package-install 'go-mode))

;; for helm
(require 'helm)
(helm-mode 1)
(define-key global-map (kbd "C-x C-o") 'helm-mini)

;; turn off auto save and auto backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; backspace
(keyboard-translate ?\C-h ?\C-?)

(electric-indent-mode 0)

;; for scala
(setenv "PATH" (concat "/usr/local/bin/sbt:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin/scala:" (getenv "PATH")))

;; scala-mode2
(require 'scala-mode2)

;; ensime
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(progn
  (require 'helm-ls-git)
  (custom-set-variables
   '(helm-truncate-lines t)
   '(helm-delete-minibuffer-contents-from-point t)
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-recentf
                                 helm-source-files-in-current-dir
                                 helm-source-ls-git
                                 ))))


;; web-mode 
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; go-mode
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
