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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; install packages

(unless (package-installed-p 'helm)
  (package-refresh-contents) (package-install 'helm))

(unless (package-installed-p 'helm-ls-git)
  (package-refresh-contents) (package-install 'helm-ls-git))

(unless (package-installed-p 'helm-ag)
  (package-refresh-contents) (package-install 'helm-ag))

(unless (package-installed-p 'web-mode)
  (package-refresh-contents) (package-install 'web-mode))

(unless (package-installed-p 'go-mode)
  (package-refresh-contents) (package-install 'go-mode))

(unless (package-installed-p 'company)
  (package-refresh-contents) (package-install 'company))

(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents) (package-install 'markdown-mode))

(unless (package-installed-p 'wgrep)
  (package-refresh-contents) (package-install 'wgrep))

(unless (package-installed-p 'ruby-mode)
  (package-refresh-contents) (package-install 'ruby-mode))

(unless (package-installed-p 'ruby-block)
  (package-refresh-contents) (package-install 'ruby-block))

(unless (package-installed-p 'ruby-electric)
  (package-refresh-contents) (package-install 'ruby-electric))

(unless (package-installed-p 'helm-ag)
  (package-refresh-contents) (package-install 'helm-ag))

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; run following command to use rubocop
;; gem install rubocop ruby-lint
(unless (package-installed-p 'flycheck)
  (package-refresh-contents) (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (setq flycheck-checker 'ruby-rubocop)
    (flycheck-mode 1)))

(unless (package-installed-p 'atom-dark-theme)
  (package-refresh-contents) (package-install 'atom-dark-theme))

(unless (package-installed-p 'ace-jump-mode)
  (package-refresh-contents) (package-install 'ace-jump-mode))

(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

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

;; company-mode
 (add-hook 'after-init-hook 'global-company-mode)
(global-company-mode 1)

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


;; window resizer
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key "\C-c\C-r" 'window-resizer)

(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; helm-git
;;(add-hook 'emacs-startup-hook 'helm-ls-git-ls)
;; close helm-git mini buffer after launch
;;(setq close-buffer (listify-key-sequence "\C-g"))
;;(add-hook 'emacs-startup-hook 'close-buffer)

;; display column number
(column-number-mode t)

;; hilight space
(setq-default show-trailing-whitespace t)

;; display time
(display-time)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-ls-git)))
 '(helm-truncate-lines t t)
 '(package-selected-packages
   (quote
    (yasnippet magit ace-jump-mode atom-dark-theme flycheck ruby-electric ruby-block wgrep markdown-mode company go-mode web-mode helm-ag helm-ls-git helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
