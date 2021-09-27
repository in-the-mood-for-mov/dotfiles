;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t
      visible-bell t
      default-frame-alist '((undecorated . t))
      make-backup-files nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(display-time-mode 1)
(set-fringe-mode 20)

(pcase system-type
  ('darwin
   (setq mac-option-modifier 'none)
   (setq mac-command-modifier 'meta)))

(set-face-attribute 'default nil :font "PragmataPro Liga" :height 180)
(setq-default indent-tabs-mode nil)
(set-default-coding-systems 'utf-8)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq-default fill-column 80)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(let* ((buffer (find-file-noselect (concat user-emacs-directory "path.s")))
       (path (unwind-protect
                 (with-current-buffer buffer
                   (goto-char (point-min))
                   (read (current-buffer)))
               (kill-buffer buffer))))
  (setq exec-path (cl-union exec-path path))
  (setenv "PATH" (mapconcat #'identity exec-path ";")))

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

(use-package recentf
  :config (recentf-mode 1))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org\\'" . org-mode)))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon nil)
  :config (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package general
  :ensure t)

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
  :commands (projectile-project-root)
  :custom (projectile-completion-system 'default)
  :config (projectile-mode 1)
  :bind-keymap ("C-x p" . projectile-command-map))

(use-package consult
  :ensure t
  :custom
  (consult-preview-key (kbd "M-."))
  (consult-project-root-function #'projectile-project-root)
  :bind (("C-x b" . consult-buffer)
         ("M-l" . consult-line)))

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
  :custom (which-key-idle-delay 0.4)
  :config (which-key-mode))

(use-package vi-tilde-fringe
  :ensure t
  :config (global-vi-tilde-fringe-mode 1))

(use-package evil
  :ensure t
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-digraphs-table-user '(((?z ?z) . #x21af) ; ↯
                              ((?l ?l) . #x2113) ; ℓ
                              ((?H ?H) . #x210b) ; ℋ
                              ((?1 ?>) . #x2192) ; →
                              ((?2 ?>) . #x21d2) ; ⇒
                              ((?3 ?>) . #x21db) ; ⇛
                              ))
  :config (evil-mode 1))

(use-package evil-escape
  :ensure t
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

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  :hook ((emacs-lisp-mode) . smartparens-strict-mode))

(use-package evil-cleverparens
  :ensure t
  :custom (evil-cleverparens-use-additional-movement-keys nil)
  :config (require 'evil-cleverparens-text-objects)
  :hook ((smartparens-enabled) . evil-cleverparens-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package tuareg
  :ensure t
  :init (setq tuareg-indent-align-with-first-arg t)
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
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package evil-collection
  :ensure t
  :after evil
  :commands (evil-collection-dired-setup
             evil-collection-magit-setup)
  :config
  (evil-collection-package-menu-setup))

(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 ";" #'evilnc-comment-operator)
