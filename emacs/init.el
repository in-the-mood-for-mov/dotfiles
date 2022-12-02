;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t
      initial-scratch-message ""
      visible-bell nil
      ring-bell-function 'ignore
      make-backup-files nil)

(defvar mac-option-modifier)
(defvar mac-command-modifier)
(pcase system-type
  ('darwin
   (setq mac-option-modifier 'none)
   (setq mac-command-modifier 'meta)))

(defconst my/face-height
  (pcase system-type
    ('darwin 210)
    ('windows-nt 170)))
(set-face-attribute 'default nil :font "Iosevka Term" :height my/face-height)
(set-face-attribute 'fixed-pitch nil :family 'unspecified :inherit 'default)

(define-key input-decode-map "\C-i" [C-i])

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

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
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package auto-compile
  :ensure t
  :custom
  (load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package simple
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config (column-number-mode))

(use-package display-line-numbers
  :config (global-display-line-numbers-mode))

(use-package recentf
  :custom (recentf-max-saved-items 40)
  :config (recentf-mode 1))

(use-package whitespace
  :hook (before-save . whitespace-cleanup))

(use-package solarized-theme
  :ensure t
  :custom
  (solarized-distinct-fringe-background t)
  (solarized-use-variable-pitch nil)
  (solarized-high-contrast-mode-line t)
  (solarized-use-more-italic t)
  :config (load-theme 'solarized-dark-high-contrast t))

(use-package general
  :ensure t)

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (auto-revert-mode nil "autorevert"))))

(use-package corfu
  :ensure t
  :custom (corfu-auto t)
  :general
  (:keymaps 'corfu-map
            "\r" nil)
  :config (global-corfu-mode))

(use-package corfu-popupinfo
  :ensure corfu
  :custom (corfu-popupinfo-delay t)
  :config (corfu-popupinfo-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package savehist
  :ensure t
  :init (savehist-mode))

(use-package projectile
  :ensure t
  :delight '(:eval (concat " ‹" (projectile-project-name) "›"))
  :commands (projectile-project-root)
  :custom (projectile-completion-system 'default)
  :config (projectile-mode 1)
  :bind-keymap ("C-x p" . projectile-command-map))

(use-package consult
  :ensure t
  :commands (consult--directory-prompt)
  :config
  (consult-customize
   consult-buffer consult-ripgrep consult-find
   :preview-key (kbd "M-."))
  :custom
  (consult-project-root-function #'projectile-project-root)
  :bind (("C-x b" . consult-buffer)
         ("M-l" . consult-line)
         ("C-x s" . consult-ripgrep)
         ("C-x f" . consult-find)))

(use-package consult-dir
  :ensure t
  :custom
  (consult-dir-project-list-function nil)
  (consult-dir-default-command #'consult-fd)
  (consult-dir-sources (list #'consult-dir--source-bookmark
                             #'consult-dir--source-project
                             #'consult-dir--source-recentf))
  :bind (("C-x C-d" . consult-dir)))

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
  :hook ((emacs-lisp-mode TeX-mode) . rainbow-delimiters-mode))

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
  (evil-undo-system 'undo-redo)
  (evil-shift-width 2)
  (evil-mode-line-format nil)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-symbol-word-search t)
  (evil-digraphs-table-user '(((?_ ?0) . #x2080) ; ₀
                              ((?_ ?1) . #x2081) ; ₁
                              ((?_ ?2) . #x2082) ; ₂
                              ((?l ?l) . #x2113) ; ℓ
                              ((?1 ?>) . #x2192) ; →
                              ((?z ?z) . #x21af) ; ↯
                              ((?2 ?>) . #x21d2) ; ⇒
                              ((?3 ?>) . #x21db))) ; ⇛
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (define-key evil-motion-state-map (kbd "<C-i>") 'evil-jump-forward)
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :delight
  :config (evil-escape-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :general (:states '(normal visual) "C-;" #'evilnc-comment-operator)
  :commands (evilnc-comment-operator))

(use-package evil-surround
  :ensure t
  :general (:states '(visual) "s" #'evil-surround-region "S" #'evil-substitute)
  :config (global-evil-surround-mode 1))

(use-package parinfer-rust-mode
  :ensure t
  :hook (emacs-lisp-mode scheme-mode))

(use-package magit
  :ensure t
  :config (magit-auto-revert-mode)
  :bind (("C-x g" . magit-status)))

(use-package message
  :commands message-mode
  :config
  (add-hook 'message-mode-hook #'(lambda () (abbrev-mode -1))))

(use-package org
  :ensure t
  :mode (("\\.org\\'" . org-mode)))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.avsc\\'")
  :config
  (add-hook
   'json-mode-hook
   #'(lambda ()
       (make-local-variable 'js-indent-level)
       (setq js-indent-level 2))))

(use-package dhall-mode
  :ensure t
  :config
  (setq dhall-use-header-line nil))

(use-package tuareg
  :ensure t
  :init
  (setq tuareg-indent-align-with-first-arg t)
  (add-hook 'tuareg-mode-hook #'rainbow-delimiters-mode)
  :mode (("\\.mli?\\'" . tuareg-mode)))

(use-package merlin
  :ensure t
  :hook (tuareg-mode . merlin-mode)
  :bind (:map merlin-mode-map ("C-c C-e" . merlin-error-next)))

(use-package latex
  :ensure auctex
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (tex-fontify-script nil)
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 'color)
  :mode (("\\.tex\\'" . tex-mode))
  :init (add-hook 'TeX-mode-hook #'auto-fill-mode)
  :config (define-key TeX-mode-map "$" nil))

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
  :commands lsp
  :hook
  ((dhall-mode . lsp)
   (haskell-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook ((lsp-mode-hook . lsp-ui-mode)))

(use-package lsp-haskell
  :ensure t)

(use-package flycheck
  :ensure t
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                tex-chktex
                                tex-lacheck)))

(use-package scheme
  :mode "\\.\\(scm\\|sld\\)\\'"
  :config
  (put 'with-input-from-u8vector 'scheme-indent-function 1))

(use-package geiser-gambit
  :ensure t
  :hook (((sheme-mode) . geiser-mode--maybe-activate)))

(use-package macrostep
  :ensure t
  :general (:keymaps 'lisp-mode-shared-map "C-c C-e" #'macrostep-expand))

(use-package typer-mode
  :load-path "typer"
  :mode (("\\.typer\\'" . typer-mode)))

(use-package ahk-mode
  :ensure t
  :mode (("\\.ahk\\'" . ahk-mode)))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-package-menu-setup)
  (with-eval-after-load 'arc-mode (evil-collection-arc-mode-setup))
  (with-eval-after-load 'custom (evil-collection-custom-setup))
  (with-eval-after-load 'dired (evil-collection-dired-setup))
  (with-eval-after-load 'magit (evil-collection-magit-setup)))

(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (fill-column 80)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(when-let ((agda-mode-path (executable-find "agda-mode")))
  (load-file
   (with-temp-buffer
     (let ((coding-system-for-read 'utf-8))
       (call-process agda-mode-path nil t nil "locate"))
     (buffer-string))))

(add-to-list 'auto-mode-alist '("\\.\\(agda\\|lagda\\.md\\)\\'" . agda2-mode))

(use-package ispell
  :defer t
  :custom
  (ispell-program-name "aspell")
  (ispell-local-dictionary "francais")
  (ispell-local-dictionary-alist
   '(("francais" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t nil nil utf-8))))

(define-key global-map (kbd "C-f") 'universal-argument)
(define-key universal-argument-map (kbd "C-f") 'universal-argument-more)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-f") nil))

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
