;;; graphiql-indent.el --- GraphQL Indentation -*- lexical-binding: t; -*-

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

;; GraphQL Indentation
;; Inspired by https://github.com/davazp/graphql-mode

;;; Code:
(require 'cl-lib)

(defun graphiql-indent-line ()
  "Indent a GraphQL document."
  (let (indent-pos)
    (save-excursion
      (let ((level (car (syntax-ppss (point-at-bol)))))
        (when (looking-at "\\s-*\\s)")
          (decf level))
        (indent-to (* 2 level)) ;; TODO Make this configurable
        (setq indent-pos (point))))
    (when (< (point) indent-pos)
      (goto-char indent-pos))))

(defun graphiql-bracket-post-self-insert-function ()
  "When inserting a new line between to curly brackets, insert an extra newline, and move between them."
  (when (and
         (eq last-input-event 'return)
         (looking-at "\\s)"))
    (newline)
    (graphiql-indent-line)
    (forward-line -1)))

(provide 'graphiql-indent)
;;; graphiql-indent.el ends here
