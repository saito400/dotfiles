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
(setq linum-format "%d ")

;; display current line
(global-hl-line-mode 1)
;;(set-face-attribute hl-line-face nil :underline t)
(setq hl-line-face 'underline)

;; tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

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

(unless (package-installed-p 'helm-ag)
  (package-refresh-contents) (package-install 'helm-ag))

(unless (package-installed-p 'web-mode)
  (package-refresh-contents) (package-install 'web-mode))

(unless (package-installed-p 'go-mode)
  (package-refresh-contents) (package-install 'go-mode))

(unless (package-installed-p 'auto-complete)
  (package-refresh-contents) (package-install 'auto-complete))

;;(unless (package-installed-p 'company)
;;  (package-refresh-contents) (package-install 'company))

(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents) (package-install 'markdown-mode))

(unless (package-installed-p 'wgrep)
  (package-refresh-contents) (package-install 'wgrep))

;;(unless (package-installed-p 'flycheck)
;;  (package-refresh-contents) (package-install 'flycheck))

(unless (package-installed-p 'atom-dark-theme)
  (package-refresh-contents) (package-install 'atom-dark-theme))

(unless (package-installed-p 'ace-jump-mode)
  (package-refresh-contents) (package-install 'ace-jump-mode))

;;theme
(load-theme 'atom-dark t)

;; for helm
(require 'helm)
(helm-mode 1)
(define-key global-map (kbd "C-x C-o") 'helm-mini)
(define-key global-map (kbd "C-x C-d") 'helm-browse-project)
(define-key global-map (kbd "C-x C-g") 'helm-ag)

;; turn off auto save and auto backup
(setq make-backup-files nil)
(setq auto-save-default nil)
;; (setq create-lockfiles nil)

;; for ace-jump
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

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
(add-hook 'scala-mode-hook 'my-scala-mode-hook)

(setq ensime-sem-high-faces
 '((var . (:foreground "#ff2222"))
   (val . (:foreground "#dddddd"))
   (varField . (:foreground "#ff3333"))
   (valField . (:foreground "#dddddd"))
   (functionCall . (:foreground "#84BEE3"))
   (param . (:foreground "#ffffff"))
   (class . font-lock-type-face)
   (trait . (:foreground "#084EA8"))
   (object . (:foreground "#026DF7"))
   (package . font-lock-preprocessor-face)))

(defun my-scala-mode-hook ()
  (setq scala-indent:use-javadoc-style t)
)

;; helm-mini
(progn
  (require 'helm-ls-git)
  (custom-set-variables
   '(frame-background-mode (quote dark))
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


;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'text-mode)
(define-key ac-completing-map (kbd "M-n") 'ac-next)
(define-key ac-completing-map (kbd "M-p") 'ac-previous)
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))
(setq ac-auto-start 2)
(ac-set-trigger-key "TAB")
(setq ensime-completion-style 'auto-complete)


;; company-mode
;; (add-hook 'after-init-hook 'global-company-mode)
;;(global-company-mode 1)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; wgrep
;;; eでwgrepモードにする
(setf wgrep-enable-key "e")
;;; wgrep終了時にバッファを保存
(setq wgrep-auto-save-buffer t)
;;; read-only bufferにも変更を適用する
(setq wgrep-change-readonly-file t)

(put 'downcase-region 'disabled nil)

;; flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)



