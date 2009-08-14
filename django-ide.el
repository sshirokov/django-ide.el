(defvar django-servers (make-hash-table :test 'equal))
(defvar django-default-settings (or (getenv "DJANGO_SETTINGS_MODULE") "settings"))
(defvar django-default-name nil)

(defun django-ide-unload-function ()
  "Unload function for django-ide"
  (message "Unloaded django-ide")
  nil)

(defun prompt-string-or-nil (prompt)
  (let* ((string (read-string (concat prompt ": ")))
         (string (if (string= string "") nil string)))
    string))

(defun server-buffer-name (base)
  (let ((base (if (and (stringp base) (string= base ""))
                  nil
                  (and (stringp base) base)))
        (base (or base "default")))
    base))

(defun django-settings (&optional prompt)
  (let* ((settings (and prompt (prompt-string-or-nil "Settings module")))
         (settings (or settings django-default-settings)))
    settings))

(defun start-or-restart-server (name settings)
  t)

(defun make-django-server (prefix)
  (interactive "P")
  (let ((name (server-buffer-name django-default-name))
        (settings (django-settings prefix)))
    (message "Prefix: %s" prefix)
    (message "Name: %s Settings: %s" name settings)))


(provide 'django-ide)