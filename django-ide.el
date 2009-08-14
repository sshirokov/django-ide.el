(defvar django-servers (make-hash-table :test 'equal))
(defvar django-default-settings (or (getenv "DJANGO_SETTINGS_MODULE") "settings"))
(defvar django-default-name "default")

(defun django-ide-unload-function ()
  "Unload function for django-ide"
  (message "Unloaded django-ide")
  nil)

(defun prompt-string-or-nil (prompt)
  (let* ((string (read-string (concat prompt ": ")))
         (string (if (equal string "") nil string)))
    string))

(defun django-server-buffer-name (prompt)
  (let* ((name (and prompt (prompt-string-or-nil "Project name")))
         (name (or name django-default-name)))
    name))

(defun django-settings (&optional prompt)
  (let* ((settings (and prompt (prompt-string-or-nil "Settings module")))
         (settings (or settings django-default-settings)))
    settings))

(defun django-prepare-server (name settings)
  '((:buffer . :TODO-some-buffer-handle)
   (:name . name)
   (:settings . settings)))

(defun django-restart-server (server)
  nil)

(defun django-start-server (server)
  nil)

(defun django-start-or-restart-server (name settings)
  (let* ((existing (gethash name django-servers))
         (server (or existing (django-prepare-server name settings))))
    (cond ((null existing)
           (puthash name server django-servers)
           (django-start-server server))
          ((null server)
           server)
          (t
           (django-restart-server server)))))

(defun make-django-server (prefix)
  (interactive "P")
  (let* ((name (django-server-buffer-name prefix))
        (settings (django-settings prefix))
        (action (let ((result (django-start-or-restart-server name settings)))
                  (cond ((equal result nil) "Failed")
                        ((equal result t) "Restarted")
                        ((stringp result) (concat "Started: " result))))))
    (message "%s DEBUG: Name: %s Settings: %s" action name settings)))


(provide 'django-ide)