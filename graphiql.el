;;; graphiql.el --- GraphQL development environment  -*- lexical-binding: t; -*-


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

;; This is a tool for testing and exploring graphql APIs.

;;; Code:

(require 'newcomment)
(require 'url)
(require 'cl-lib)
(require 'json)


(defgroup grapihql nil
  "An interactive GraphQL environment for emacs."
  :tag "GraphiQL"
  :group 'tools)

(defcustom graphiql-url nil
  "URL address of the graphql server endpoint."
  :tag "GraphiQL"
  :type 'string
  :group 'graphql)

(defcustom graphiql-extra-headers '()
  "Headers to send to the graphql endpoint."
  :tag "GraphiQL"
  :type 'list
  :group 'graphiql)

(defcustom graphiql-use-lsp nil
  "Use lsp-mode for autocompletion and validation."
  :tag "GraphiQL"
  :type 'boolean
  :group 'graphiql)

(defconst graphiql-introspection-query
  "query IntrospectionQuery {
  __schema {
    queryType {
      name
    }
    mutationType {
      name
    }
    subscriptionType {
      name
    }
    types {
      ...FullType
    }
    directives {
      name
      description
      locations
      args {
        ...InputValue
      }
    }
  }
}

fragment FullType on __Type {
  kind
  name
  description
  fields(includeDeprecated: true) {
    name
    description
    args {
      ...InputValue
    }
    type {
      ...TypeRef
    }
    isDeprecated
    deprecationReason
  }
  inputFields {
    ...InputValue
  }
  interfaces {
    ...TypeRef
  }
  enumValues(includeDeprecated: true) {
    name
    description
    isDeprecated
    deprecationReason
  }
  possibleTypes {
    ...TypeRef
  }
}

fragment InputValue on __InputValue {
  name
  description
  type {
    ...TypeRef
  }
  defaultValue
}

fragment TypeRef on __Type {
  kind
  name
  ofType {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    }
  }
} "
  "Introspection query used to download the schema.")

(defun json-path (json path)
  "Index into JSON by following PATH.
JSON is expected to be an alist representation of a json object,
and PATH is a list of symbols or numbers."
  (cl-loop for item in path do
           (cond
            ((numberp item) (setq json (elt json item)))
            (t (setq json (cdr (assoc item json))))))
  json)

(defun graphiql-locate-config (dir)
  "Locate a graphql config starting in DIR."
  (if-let ((config-dir (locate-dominating-file dir ".graphqlconfig")))
      (concat config-dir ".graphqlconfig")
    (error "Could not find a .graphqlconfig file")))

(defun graphiql-open-config ()
  "Open a graphql config.
See https://github.com/kamilkisiela/graphql-config for more information on the config format."
  (interactive)
  (find-file (graphiql-locate-config ".")))

(defun graphiql-next-query ()
  "Move the point to the next query."
  (interactive)
  (end-of-line)
  (re-search-forward "^query\\|^mutation\\|^subscription" nil t))

(defun graphiql-previous-query ()
  "Move the point to the previous query."
  (interactive)
  (beginning-of-line)
  (re-search-backward "^query\\|^mutation\\|^subscription" nil t))

(defun graphiql--completing-read-endpoint (endpoints)
  "Select an endpoint configuration from a list of ENDPOINTS."
  (completing-read "Select Graphql Endpoint:" (mapcar 'car endpoints)))

(defun graphiql-select-endpoint ()
  "Set parameters based off of the endpoints listed in a .graphqlconfig file."
  (interactive)
  (if-let* ((json-key-type 'string)
            (config (json-read-file (graphiql-locate-config ".")))
            (endpoints (json-path config '("extensions" "endpoints")))
            (selection (graphiql--completing-read-endpoint endpoints)))
      (setq graphiql-url (json-path endpoints (list selection "url"))
            graphiql-extra-headers (json-path endpoints (list selection "headers")))
    (error "Could not load .graphqlconfig")))

(defun graphiql-download-schema ()
  "Download the schema to the location specified by the .graphqlconfig file."
  (interactive)
  (if-let* ((config-file (graphiql-locate-config "."))
            (config (json-read-file config-file))
            (schema-location (json-path config '(schemaPath)))
            (schema-file (expand-file-name schema-location (file-name-directory config-file))))
      (graphiql-request graphiql-introspection-query
                        :operation "IntrospectionQuery"
                        :on-error 'print
                        :on-success (lambda (response)
                                      (write-region (json-encode response) nil schema-file)))
    (error "Could not load .graphqlconfig")))

(defun graphiql-encode-query (query &optional operation variables)
  "Encodes QUERY, OPERATION, and VARIABLES into a json object."
  (let (body)
    (push (cons 'query query) body)
    (when (and operation (not (string= operation "")))
      (push (cons 'operationName operation) body))
    (when variables
      (push (cons 'variables variables) body))
    (json-encode body)))

(cl-defun graphiql-request (query &key operation variables on-success on-error)
  "Perform a graphql QUERY.
OPERATION is the operation to perform when dealing with multi-query documents.
VARIABLES is an alist of variables to provide to the graphql QUERY.
ON-SUCCESS is a callback that will be called with the json result of the
query, encoded as an alist.
ON-ERROR is a callback that will get called when the request errors out,
and will be passed the errors from `url-retrieve'."
  (let ((url-request-extra-headers (append '(("Content-Type" . "application/json")) graphiql-extra-headers))
        (url-request-data (graphiql-encode-query query operation variables))
        (url-request-method "POST"))
    (url-retrieve graphiql-url
                  (lambda (status)
                    (let ((error (plist-get status :error))) ;; TODO: Better error handling
                      (if error
                          (funcall on-error error)
                        (goto-char url-http-end-of-headers)
                        (funcall on-success (json-read))
                        ))))))

(defun graphiql-current-operation ()
  "Return the name of the current definition."
  (save-excursion
    ;; We want to include the current line in our search, so we move to the end of the line.
    (end-of-line)
    (re-search-backward "\\(?:^query\\|^mutation\\|^subscription\\)[[:space:]]+\\([[:alnum:]]*\\)")
    (match-string-no-properties 1)))

(defun graphiql-current-variables ()
  "Get the variables from the '*GraphQL Variables*' buffer."
  (with-current-buffer (get-buffer-create "*GraphiQL Variables*")
    (save-excursion
      (when (eq (point-min) (point-max))
        (insert "{}"))
      (goto-char (point-min))
      (json-read))))

(defun graphiql-resolve-imports ()
  "Return the current buffer as a string with all import statements resolved."
  (let ((buffer (current-buffer))
        file
        cwd)
   (with-temp-buffer
     (let ((tmp-buffer (current-buffer)))
       (with-current-buffer buffer (copy-to-buffer tmp-buffer (point-min) (point-max))))
     (while (re-search-forward "#import \"\\(.*\\)\"$" nil t)
       (save-excursion
         (goto-char (point-max))
         (setq file (expand-file-name (match-string-no-properties 1) cwd))
         (setq cwd (file-name-directory file))
         (insert-file-contents file)))
     (buffer-string))))

(defun graphiql-send-query ()
  "Send the current GraphQL query/mutation/subscription to the server."
  (interactive)
  (let ((query (graphiql-resolve-imports))
        (operation (graphiql-current-operation))
        (variables (graphiql-current-variables)))
    (graphiql-request query
                      :operation operation
                      :variables variables
                      :on-error (lambda (errors) (print errors)) ;; TODO Better error handling
                      :on-success (lambda (response)
                                    (with-current-buffer-window
                                        "*GraphiQL*" '(display-buffer-in-side-window (side . right)) nil
                                      (erase-buffer)
                                      (when (fboundp 'json-mode) (json-mode))
                                      (let ((json-encoding-pretty-print t))
                                        (insert (json-encode response))))
                                    (display-buffer-below-selected
                                     (with-current-buffer "*GraphiQL Variables*"
                                       (erase-buffer)
                                       (when (fboundp 'json-mode) (json-mode))
                                       (if variables
                                           (let ((json-encoding-pretty-print t))
                                             (insert (json-encode variables)))
                                         (insert "{}"))
                                       (current-buffer))
                                     '((window-height . 10)))))))

(defvar graphiql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'graphiql-send-query)
    (define-key map (kbd "C-c C-l") 'graphiql-select-endpoint)
    map)
  "Key binding for GraphiQL.")

(defvar graphiql-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\$ "'" st)
    st)
  "Syntax table for GraphiQL.")

;;;###autoload
(define-derived-mode graphiql-mode prog-mode "GraphiQL"
  "Turns on graphiql-mode."
  (require 'graphiql-font)
  (require 'graphiql-indent)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local font-lock-defaults '(graphiql-font-lock-keywords))
  (setq-local indent-line-function 'graphiql-indent-line)
  (add-hook 'post-self-insert-hook 'graphiql-bracket-post-self-insert-function)
  (when graphiql-use-lsp
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tcp-connection (lambda (port) `("graphql" "server" "-m" "socket" "-p" ,(number-to-string port))))
                      :major-modes '(graphiql-mode)
                      :initialization-options (lambda () `())
                      :server-id 'graphql))
    (add-to-list 'lsp-language-id-configuration '(graphiql-mode . "graphql"))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphiql-mode))

(provide 'graphiql)
;;; graphiql.el ends here
