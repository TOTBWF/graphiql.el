;;; graphiql-font.el --- Syntax highlighting for GraphQL -*- lexical-binding: t; -*-

;; Author: Reed Mullanix <reedmullanix@gmail.com>
;; Keywords: graphql

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Syntax highlighting for GraphQL
;; Inspired by https://github.com/davazp/graphql-mode

;;; Code:
;;;
(defvar graphiql-definition-regex
    (concat "\\(" (regexp-opt '("query" "mutation" "subscription")) "\\)"
            "[[:space:]]+\\(\\_<.+?\\_>\\)")
    "Definition Regular Expression.")

(defvar graphiql-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\$ "'" st)
    st)
  "Syntax table for GraphiQL.")

(defvar graphiql-font-lock-keywords
  `(
    ;; Definitions
    (,graphiql-definition-regex
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; Constants
    (,(regexp-opt "true" "false" "null") . font-lock-constant-face)

    ;; Variables
    ("\\$\\_<.+?\\_>" . font-lock-variable-name-face)

    ;; Types
    (":[[:space:]]*\\(\\_<.+?\\_>\\)" . font-lock-type-face)

    ;; Fragment Spreads
    ("\\.\\.\\.[[:space:]]*\\(on\\)?[[:space:]]\\[?\\(\\_<.+?\\_>\\)\\]?"
     (1 font-lock-type-face)
     (2 font-lock-type-face))

    ;; Input fields
    ("\\(\\_<\\w+?\\_>\\)[[:space:]]*:" . font-lock-constant-face))
  "GraphQL Font Lock Keywords.")

;; (add-hook 'post-self-insert-hook 'graphiql-bracket-post-self-insert-function
;;           ;; Most likely, this hook is nil, so this arg doesn't matter,
;;           ;; but I use it as a reminder that this function usually
;;           ;; likes to be run after others since it does
;;           ;; `sit-for'. That's also the reason it get a `priority' prop
;;           ;; of 100.
;;           'append)

(provide 'graphiql-font)
;;; graphiql-font.el ends here
