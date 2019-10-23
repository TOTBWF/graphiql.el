;;; graphiql.el --- GraphQL development environment  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a tool for testing and exploring graphql APIs.

;;; Code:

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

(defun graphiql--completing-read-endpoint (endpoints)
  "Select an endpoint configuration from a list of ENDPOINTS."
  (completing-read "Select Graphql Endpoint:" (mapcar 'car endpoints)))

(defun graphiql-select-endpoint ()
  "Set parameters based off of the endpoints listed in a .graphqlconfig file."
  (interactive)
  (if-let* (
            (json-key-type 'string)
            (config (json-read-file (graphiql-locate-config ".")))
            (endpoints (json-path config '("extensions" "endpoints")))
            (selection (graphiql--completing-read-endpoint endpoints)))
      (setq graphiql-url (json-path endpoints (list selection "url"))
            graphiql-extra-headers (json-path endpoints (list selection "headers")))
    (error "No endpoint configuration in .graphqlconfig")))

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
                    (let ((error (plist-get status :error)))
                      (if error
                          (funcall on-error error)
                        (goto-char url-http-end-of-headers)
                        (funcall on-success (json-read))))))))

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
        (variables (graphiql-current-variables))
        (json-encoding-pretty-print t))
    (graphiql-request query
                      :operation operation
                      :variables variables
                      :on-error (lambda (errors) (print errors))
                      :on-success (lambda (response)
                                    (with-current-buffer-window
                                        "*GraphiQL*" 'display-buffer-in-side-window nil
                                      (erase-buffer)
                                      (when (fboundp 'json-mode) (json-mode))
                                      (insert (json-encode response)))
                                    (display-buffer-below-selected
                                     (with-current-buffer "*GraphiQL Variables*"
                                       (erase-buffer)
                                       (when (fboundp 'json-mode) (json-mode))
                                       (insert (json-encode variables))
                                       (current-buffer))
                                     '((window-height . 10)))))))

(provide 'graphiql)
;;; graphiql.el ends here
