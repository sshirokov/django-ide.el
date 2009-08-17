(require 'cl)

(defvar django-servers (make-hash-table :test 'equal))
(defvar django-default-settings (or (getenv "DJANGO_SETTINGS_MODULE") "settings"))
(defvar django-default-name "default")

(defstruct django-server
  name settings host port buffer proc)

(defun hash-table-keys (hash)
  (let ((keys nil)
             (add-key (lambda (k v) (setf keys (cons k keys)))))
         (maphash add-key hash)
         keys))

(defun django-ide-unload-function ()
  "Unload function for django-ide"
  (message "Closing existing servers")
  (let ((kill-and-close (lambda (name server)
                          (message "Killing server: %s => %s" name server)
                          (puthash name (django-stop-server server) django-servers))))
    (maphash kill-and-close django-servers))
  (message "Unloaded django-ide")
  nil)

(defun django-prompt-change-default-name ()
  (interactive)
  (setf django-default-name (or (prompt-string-or-nil "New default name" (django-known-projects) django-default-name)
                                django-default-name)))

(defun prompt-string-or-nil (prompt &optional completions default)
  (let* ((string (completing-read (concat prompt ": ") completions nil nil default))
         (string (if (equal string "") nil string)))
    string))

(defun django-known-projects ()
  (hash-table-keys django-servers))

(defun django-server-buffer-name (prompt)
  (let* ((existing-projects (django-known-projects))
         (name (and prompt (prompt-string-or-nil "Project name" existing-projects (or (car (django-known-projects)) django-default-name))))
         (name (or name django-default-name)))
    name))

(defun django-settings (&optional prompt)
  (let* ((settings (and prompt (prompt-string-or-nil "Settings module")))
         (settings (or settings django-default-settings)))
    settings))

(defun django-stop-server (server)
  (message "Stopping: %s" server)
  (let ((buffer (django-server-buffer server))
        (proc (django-server-proc server)))
    (and proc (kill-process proc))
    (and buffer (kill-buffer buffer))
    (setf (django-server-buffer server) nil
          (django-server-proc server) nil)
    server))


(defun django-restart-server (server)
  (message "Restarting: %s" server)
  (django-start-server (django-stop-server server)))

(defun django-server-running? (server)
  (and (django-server-buffer server) (django-server-proc server)))

(defun django-stop-all-running-servers ()
  (let ((stop-if-running (lambda (name server)
                           (let ((server (or (and (django-server-running? server)
                                                 (django-stop-server server))
                                            server)))
                             (puthash name server django-servers)))))
    (maphash stop-if-running django-servers)))

(defun django-start-server (server)
  (django-stop-all-running-servers)
  (let* ((server (django-stop-server server))
         (bname (concat "*django-server-" (django-server-name server) "*"))
         (buffer (get-buffer-create bname))
         (process (start-process-shell-command bname buffer "django-admin.py" "runserver" "0.0.0.0:8000" (concat "--settings=" settings))))
    (setf (django-server-buffer server) buffer
          (django-server-proc server) process)
    (puthash name server django-servers)
    (message "Started server: %s" server)
    server))

(defun django-start-or-restart-server (name settings)
  (message "Making server with :name %s :settings %s" name settings)
  (let* ((existing (gethash name django-servers))
         (server (or existing (make-django-server :name name :settings settings)))
         (proc (django-server-proc server)))
    (cond ((null existing)
           (message "Starting new: %s" server)
           (puthash name server django-servers)
           (django-start-server server))
          ((null server)
           (message "Don't have server..")
           server)
          (t
           (message "Restarting existing")
           (django-restart-server server)))))

(defun django-spin-server (prefix)
  (interactive "P")
  (let* ((name (django-server-buffer-name prefix))
        (settings (django-settings prefix))
        (action (let ((result (django-start-or-restart-server name settings)))
                  (cond ((equal result nil) (concat "Failed: %s" name))
                        ((equal result t) (concat "Restarted: %s" name))
                        ((django-server-p result) (concat "Started: " name))))))
    (message "%s DEBUG: Name: %s Settings: %s" action name settings)))


(provide 'django-ide)