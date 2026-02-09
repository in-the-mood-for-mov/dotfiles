;;; pkl-ts-mode.el --- Tree-sitter support for Pkl -*- lexical-binding: t -*-

;;; Commentary:

;; Major mode for editing Pkl configuration files, powered by tree-sitter.
;; Install the grammar via M-x treesit-install-language-grammar and selecting pkl.

;;; Code:

(require 'treesit)

(defvar pkl-ts-mode--grammar-source
  '(pkl "https://github.com/apple/tree-sitter-pkl")
  "Tree-sitter grammar source for Pkl.")

(defvar pkl-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'pkl
   :feature 'comment
   '((lineComment) @font-lock-comment-face
     (blockComment) @font-lock-comment-face
     (docComment) @font-lock-doc-face)

   :language 'pkl
   :feature 'string
   '((slStringLiteralExpr) @font-lock-string-face
     (mlStringLiteralExpr) @font-lock-string-face
     (stringConstant) @font-lock-string-face)

   :language 'pkl
   :feature 'escape-sequence
   :override t
   '((escapeSequence) @font-lock-escape-face)

   :language 'pkl
   :feature 'number
   '((intLiteralExpr) @font-lock-number-face
     (floatLiteralExpr) @font-lock-number-face)

   :language 'pkl
   :feature 'constant
   '((trueLiteralExpr) @font-lock-constant-face
     (falseLiteralExpr) @font-lock-constant-face
     (nullLiteralExpr) @font-lock-constant-face)

   :language 'pkl
   :feature 'keyword
   '(["import" "import*" "as" "is"
      "if" "else" "for" "when" "let"
      "in" "new" "read" "read*" "read?"
      "throw" "trace" "module" "open" "class"
      "typealias" "function" "extends" "amends"
      "abstract" "external" "local" "hidden"
      "fixed" "const" "out"] @font-lock-keyword-face)

   :language 'pkl
   :feature 'builtin
   '((moduleExpr) @font-lock-builtin-face
     (outerExpr) @font-lock-builtin-face
     (thisExpr) @font-lock-builtin-face
     (superAccessExpr "super" @font-lock-builtin-face))

   :language 'pkl
   :feature 'type
   '((clazz (identifier) @font-lock-type-face)
     (typeAlias (identifier) @font-lock-type-face)
     (declaredType (qualifiedIdentifier) @font-lock-type-face)
     (moduleClause (qualifiedIdentifier) @font-lock-type-face))

   :language 'pkl
   :feature 'function
   '((classMethod (methodHeader (identifier) @font-lock-function-name-face))
     (objectMethod (methodHeader (identifier) @font-lock-function-name-face)))

   :language 'pkl
   :feature 'property
   '((classProperty (identifier) @font-lock-property-name-face)
     (objectProperty (identifier) @font-lock-property-name-face))

   :language 'pkl
   :feature 'variable
   '((typedIdentifier (identifier) @font-lock-variable-name-face)
     (letExpr (typedIdentifier (identifier) @font-lock-variable-name-face)))

   :language 'pkl
   :feature 'annotation
   :override t
   '((annotation "@" @font-lock-preprocessor-face
                 (qualifiedIdentifier) @font-lock-preprocessor-face))

   :language 'pkl
   :feature 'operator
   '(["??" "=" "==" "!=" "<" "<=" ">" ">="
      "+" "-" "*" "/" "%" "**" "~/" "&&" "||"
      "!" "|>" "->"] @font-lock-operator-face)

   :language 'pkl
   :feature 'delimiter
   '(["," ":" "." "?."] @font-lock-punctuation-face)

   :language 'pkl
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face))
  "Font-lock settings for `pkl-ts-mode'.")

;;;###autoload
(define-derived-mode pkl-ts-mode prog-mode "Pkl"
  "Major mode for editing Pkl files, powered by tree-sitter."
  (unless (treesit-ready-p 'pkl)
    (user-error "Tree-sitter grammar for Pkl is not installed.
Install it with M-x treesit-install-language-grammar RET pkl RET"))

  (treesit-parser-create 'pkl)

  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq-local treesit-font-lock-settings pkl-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (keyword type constant number)
                (builtin function property variable annotation escape-sequence)
                (operator delimiter bracket)))

  (treesit-major-mode-setup))

(add-to-list 'treesit-language-source-alist pkl-ts-mode--grammar-source)

(when (treesit-ready-p 'pkl)
  (add-to-list 'auto-mode-alist '("\\.pkl\\'" . pkl-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.pcf\\'" . pkl-ts-mode)))

(provide 'pkl-ts-mode)

;;; pkl-ts-mode.el ends here
