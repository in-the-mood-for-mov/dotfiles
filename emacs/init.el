;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t
      initial-scratch-message ""
      visible-bell nil
      ring-bell-function 'ignore
      default-frame-alist '((undecorated . t)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 40))
      make-backup-files nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(column-number-mode)
(global-display-line-numbers-mode)

(pcase system-type
  ('darwin
   (setq mac-option-modifier 'none)
   (setq mac-command-modifier 'meta)))

(set-face-attribute 'default nil :font "PragmataPro Liga" :height 180)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default indent-tabs-mode nil
              fill-column 80)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-to-list 'auto-mode-alist '("\\.\\(scm\\|sld\\)\\'" . scheme-mode))

(let* ((buffer (find-file-noselect (concat user-emacs-directory "path.s")))
       (path (unwind-protect
                 (with-current-buffer buffer
                   (goto-char (point-min))
                   (read (current-buffer)))
               (kill-buffer buffer))))
  (setq exec-path (cl-union exec-path path))
  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

(let* ((buffer (find-file-noselect (concat user-emacs-directory "env.s")))
       (env (unwind-protect
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (read (current-buffer)))
              (kill-buffer buffer))))
  (dolist (pair env)
    (pcase-let ((`(,name . ,value) pair))
      (setenv name value))))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package message
  :commands message-mode
  :config
  (add-hook 'message-mode-hook #'(lambda () (abbrev-mode -1))))

(use-package recentf
  :config (recentf-mode 1))

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-syntax '(alt-syntax green-strings yellow-comments))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(use-package general
  :ensure t)

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (auto-revert-mode nil "autorevert"))))

(use-package selectrum
  :ensure t
  :init (selectrum-mode 1))

(use-package selectrum-prescient
  :ensure t
  :init
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package projectile
  :ensure t
  :delight '(:eval (concat " ‹" (projectile-project-name) "›"))
  :commands (projectile-project-root)
  :custom (projectile-completion-system 'default)
  :config (projectile-mode 1)
  :bind-keymap ("C-x p" . projectile-command-map))

(defun consult--fd-builder (input)
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended)))
    (when re
      `(:command
        ("fd"
         "--color=never"
         "--full-path"
         ,(consult--join-regexps re 'extended)
         ,@opts)
        :highlight
        ,hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

(use-package consult
  :ensure t
  :commands (consult--directory-prompt)
  :config
  (consult-customize
   consult-buffer consult-ripgrep consult-fd
   :preview-key (kbd "M-."))
  :custom
  (consult-project-root-function #'projectile-project-root)
  :bind (("C-x b" . consult-buffer)
         ("M-l" . consult-line)
         ("C-x s" . consult-ripgrep)
         ("C-x f" . consult-fd)))

(use-package consult-dir
  :ensure t
  :custom
  (consult-dir-project-list-function nil)
  (consult-dir-default-command #'consult-fd)
  (consult-dir-sources (list #'consult-dir--source-bookmark
                             #'consult-dir--source-project
                             #'consult-dir--source-recentf))
  :bind (("C-x C-d" . consult-dir)
         :map selectrum-minibuffer-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package embark
  :ensure t
  :after (consult))

(use-package embark-consult
  :ensure t
  :after (consult embark))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :delight
  :custom (which-key-idle-delay 0.4)
  :config (which-key-mode))

(use-package vi-tilde-fringe
  :ensure t
  :delight
  :config (global-vi-tilde-fringe-mode 1))

(use-package evil
  :ensure t
  :custom
  (evil-mode-line-format nil)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-digraphs-table-user '(((?z ?z) . #x21af) ; ↯
                              ((?l ?l) . #x2113) ; ℓ
                              ((?H ?H) . #x210b) ; ℋ
                              ((?1 ?>) . #x2192) ; →
                              ((?2 ?>) . #x21d2) ; ⇒
                              ((?3 ?>) . #x21db))) ; ⇛

  :config (evil-mode 1))

(use-package evil-escape
  :ensure t
  :delight
  :config (evil-escape-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :commands (evilnc-comment-operator))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(general-define-key
 :states '(visual)
 "s" #'evil-surround-region
 "S" #'evil-substitute)

(use-package parinfer-rust-mode
  :ensure t
  :delight '(:eval (concat " "
                           (pcase parinfer-rust--mode
                            ("smart" "𝔰")
                            ("indent" "𝔦")
                            ("paren" "𝔭"))))
  :hook (emacs-lisp-mode scheme-mode))

(use-package magit
  :ensure t
  :config (magit-auto-revert-mode)
  :bind (("C-x g" . magit-status)))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org\\'" . org-mode)))

(use-package tuareg
  :ensure t
  :init
  (setq tuareg-indent-align-with-first-arg t)
  (add-hook 'tuareg-mode-hook #'rainbow-delimiters-mode)
  :mode (("\\.mli?\\'" . tuareg-mode)))

(use-package merlin
  :ensure t
  :hook (tuareg-mode . merlin-mode))

(use-package latex
  :ensure auctex
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (tex-fontify-script nil)
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 'color)
  :mode (("\\.tex\\'" . tex-mode))
  :init (add-hook 'TeX-mode-hook #'auto-fill-mode))

(use-package powershell
  :ensure t
  :custom
  (powershell-indent 2)
  :mode (("\\.ps1\\'" . powershell-mode)))

(use-package haskell-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :delight
  :init (setq lsp-keymap-prefix "C-;")
  :hook
  ((haskell-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :ensure t)

(use-package flycheck
  :ensure t
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package geiser-gambit
  :ensure t
  :hook (((sheme-mode) . geiser-mode--maybe-activate)))

(use-package evil-collection
  :ensure t
  :after evil
  :commands (evil-collection-custom-setup
             evil-collection-dired-setup
             evil-collection-magit-setup)
  :config
  (evil-collection-package-menu-setup))

(with-eval-after-load 'custom (evil-collection-custom-setup))
(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 ";" #'evilnc-comment-operator)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))
