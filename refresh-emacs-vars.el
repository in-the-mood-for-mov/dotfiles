;; -*- lexical-binding: t -*-

(with-temp-file (concat user-emacs-directory "path.s")
  (pp (parse-colon-path (getenv "PATH")) (current-buffer)))

(let ((vars '("LANG" "RUSTUP_HOME" "CARGO_HOME" "OPAMROOT"))
      (to-env-pair (lambda (var) (cons var (getenv var)))))
  (with-temp-file (concat user-emacs-directory "env.s")
    (pp (mapcar to-env-pair vars) (current-buffer))))
