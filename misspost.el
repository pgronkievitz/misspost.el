;;; misspost.el --- Simple package for shitposting -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Patryk Gronkiewicz
;;
;; Author: Patryk Gronkiewicz <patryk@gronkiewicz.dev>
;; Maintainer: Patryk Gronkiewicz <patryk@gronkiewicz.dev>
;; Created: 2023-01-06
;; Modified: 2023-01-06
;; Version: 1.1.0
;; Keywords: tools multimedia
;; Homepage: https://codeberg.org/pgronkievitz/misspost
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package is used for shitposting on the Fediverse using Misskey.
;;
;;
;;; Code:

(require 'request)
(require 'json)

(defgroup misspost nil
  "MISSkey shitPOST."
  :group 'convenience
  :prefix "misspost-")

(defvar misspost-instance
  "misskey.io")

(defun misspost--instance-url ()
  "Misskey instance to use."
  (concat "https://" misspost-instance "/"))

(defvar misspost-use-authinfo
  nil
  "Whether to use authinfo.")

(defvar misspost-api-key
  nil
  "API Key for misskey.")

(defun misspost--success (&key &allow-other-keys)
  "Function ran on post success.
DATA returned data"
  (message "Sent your note!"))

(defun misspost--fail (&key error-thrown &allow-other-keys &rest _)
  "Function ran on post fail.
ERROR-THROWN is HTTP error"
  (message "What a terrible failure! %S" error-thrown))

(defun misspost--get-api-key-from-authinfo ()
  "Get API key from authinfo."
  (require 'auth-source)
  (funcall (plist-get (car (auth-source-search :host misspost-instance)) :secret)))

(defun misspost--generate-payload (content)
  "Generate payload for the misspost.
CONTENT is the text of the post"
  (let* ((misspost--key
          (if misspost-use-authinfo
              (misspost--get-api-key-from-authinfo)
              misspost-api-key)))
  (json-encode `(("visibility" . "public")
                 ("cw" . nil)
                 ("i" . ,misspost--key)
                 ("text" . ,content)))))

(defun misspost--post (content)
  "Post to the fedi.
CONTENT is the text of the post."
  (request
    (concat (misspost--instance-url) "api/notes/create")
    :type "POST"
    :data (misspost--generate-payload content)
    :parser 'json-read
    :success 'misspost--success
    :error 'misspost--fail))

(defun misspost--publish ()
  "Publish contents of the current buffer and close it."
  (interactive)
  (misspost--post (buffer-string))
  (kill-buffer (get-buffer "*new-misskey-note*")))

(defvar misspost-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'misspost--publish)
    map))

(define-derived-mode misspost-mode text-mode "Misspost"
  "Misspost mode. Used for new post buffer.")

(defun misspost-note--compose-buffer ()
  "Create buffer to capture text for a new note."
  (let* ((buffer (get-buffer-create "*new-misskey-note*")))
    (switch-to-buffer-other-window buffer)
    (misspost-mode)))

(defun misspost-note ()
  "Publish a note."
  (interactive)
  (misspost-note--compose-buffer))

(provide 'misspost)
;;; misspost.el ends here
