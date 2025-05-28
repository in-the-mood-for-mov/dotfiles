;; -*- lexical-binding: t -*-

(define-key input-decode-map "\C-i" [C-i])

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(let* ((buffer (find-file-noselect (concat user-emacs-directory "path.s")))
       (path (unwind-protect
                 (with-current-buffer buffer
                   (goto-char (point-min))
                   (read (current-buffer)))
               (kill-buffer buffer))))
  (setq exec-path (seq-uniq (seq-concatenate 'list exec-path path)))
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
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-expand-minimally t)

(use-package auto-compile
  :ensure t
  :custom
  (load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package emacs
  :custom
  (inhibit-startup-message t)
  (initial-scratch-message "")

  (indent-tabs-mode nil)
  (fill-column 80)
  (sentence-end-double-space nil)
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (make-backup-files nil)

  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-region '(accented bg-only))
  (modus-themes-paren-match '(intense))
  (modus-themes-fringes 'subtle)

  :config
  (load-theme 'modus-vivendi)

  (column-number-mode)
  (global-display-line-numbers-mode)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'before-save-hook #'whitespace-cleanup))

(use-package ns-win
  :requires ns
  :custom
  (mac-option-modifier 'none)
  (mac-command-modifier 'meta))

(use-package faces
  :if window-system
  :config
  (set-face-attribute 'default nil :font "Cascadia Code"
                      :height
                      (pcase system-type
                        ('darwin 210)
                        ('windows-nt 170)
                        ('gnu/linux 200)))
  (set-face-attribute 'fixed-pitch nil :family 'unspecified :inherit 'default))

(use-package recentf
  :custom (recentf-max-saved-items 40)
  :config (recentf-mode))

(use-package savehist
  :init (savehist-mode))

(use-package general
  :ensure t)

(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (auto-revert-mode nil "autorevert"))))

(use-package projectile
  :ensure t
  :defer nil
  :delight '(:eval (concat " ‹" (projectile-project-name) "›"))
  :custom (projectile-completion-system 'default)
  :config (projectile-mode 1)
  :bind-keymap ("C-x p" . projectile-command-map))

(use-package consult
  :ensure t
  :custom
  (consult-project-root-function #'projectile-project-root)
  :config
  (defalias 'consult-ripgrep-at-point 'consult-ripgrep)
  (consult-customize
   consult-ripgrep-at-point
   :initial (thing-at-point 'symbol))
  (consult-customize
   consult-buffer consult-ripgrep consult-find
   :preview-key "M-.")
  :bind (("C-x b" . consult-buffer)
         ("M-l" . consult-line)
         ("C-x s" . consult-ripgrep)
         ("C-x *" . consult-ripgrep-at-point)
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

(use-package corfu
  :ensure t
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match 'separator)
  :general
  (:states '(insert) "<tab>" #'completion-at-point)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package corfu-candidate-overlay
  :ensure t
  :after corfu
  :general
  (:states '(insert) "C-<tab>" #'corfu-candidate-overlay-complete-at-point)
  :init
  (corfu-candidate-overlay-mode +1))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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
  :commands (rainbow-delimiters-mode))

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
  :commands (parinfer-rust-mode)
  :custom (parinfer-rust-auto-download t))

(use-package magit
  :ensure t
  :config (magit-auto-revert-mode)
  :bind (("C-x g" . magit-status)))

(use-package lsp-mode
  :ensure t
  :delight
  :commands lsp-mode
  :custom (lsp-keymap-prefix "C-;")
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-ui
  :ensure t
  :hook lsp-mode)

(use-package flycheck
  :ensure t
  :delight
  :commands (flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save new-line mode-enabled))
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package elisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-rust-mode)
  (add-hook 'emacs-lisp-mode-hool #'flycheck-mode))

(use-package org
  :ensure t
  :mode (("\\.org\\'" . org-mode)))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-contrib
  :ensure t
  :after (org)
  :config (require 'ob-csharp))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.avsc\\'")
  :config
  (add-hook
   'json-mode-hook
   #'(lambda ()
       (make-local-variable 'js-indent-level)
       (setq js-indent-level 2))))

(use-package powershell
  :ensure t
  :mode (("\\.ps1\\'" . powershell-mode))
  :custom (powershell-indent 2))

(use-package ahk-mode
  :ensure t
  :mode (("\\.ahk\\'" . ahk-mode))
  :custom (ahk-indentation 2))

(use-package yaml
  :ensure t
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(use-package yaml-pro
  :ensure t
  :hook (yaml-mode . yaml-pro-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-package-menu-setup)
  (with-eval-after-load 'arc-mode (evil-collection-arc-mode-setup))
  (with-eval-after-load 'custom (evil-collection-custom-setup))
  (with-eval-after-load 'dired (evil-collection-dired-setup))
  (with-eval-after-load 'magit (evil-collection-magit-setup)))

(use-package ispell
  :defer t
  :custom
  (ispell-program-name "aspell")
  (ispell-local-dictionary "francais")
  (ispell-local-dictionary-alist
   '(("francais" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t nil nil utf-8)))
  :init
  (evil-define-operator evil-ispell-operator (beg end)
    "Spell check region."
    (ispell-region beg end))
  :general (:states '(normal visual) "z /" #'evil-ispell-operator))

(define-key global-map (kbd "C-f") 'universal-argument)
(define-key universal-argument-map (kbd "C-f") 'universal-argument-more)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-f") nil))
