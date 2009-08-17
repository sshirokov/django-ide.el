(require 'cl)

(defvar django-servers (make-hash-table :test 'equal))
(defvar django-default-settings (or (getenv "DJANGO_SETTINGS_MODULE") "settings"))
(defvar django-default-name "default")
(defvar django-default-listen-host (or (getenv "DJANGO_LISTEN_HOST") "localhost"))

(defstruct django-server
  name
  (settings django-default-settings)
  (host (or (getenv "DJANGO_LISTEN_HOST") "localhost"))
  (port (or (getenv "DJANGO_LISTEN_PORT") 8000))
  buffer
  proc
  console
  console-proc)

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

(defun django-running-server ()
  (let ((running nil)
        (add-if-running (lambda (name server)
                          (cond ((and (django-server-buffer server) (django-server-proc server))
                                 (setf running (cons `(,name . ,server) running)))
                                (t nil)))))
    (maphash add-if-running django-servers)
    ;; Should make sure there's not more than one..
    (message "Running server: %s" running)
    (cdar running)))

(defun django-switch-to-running-server (&optional prefix)
  (interactive "P")
  (let* ((server (cond ((null prefix)
                        (django-running-server))
                       (t
                        (django-spin-server prefix)
                        (django-running-server))))
         (buffer (and server (django-server-buffer server))))
    (and buffer (switch-to-buffer buffer))))

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
    (and buffer proc
         (progn
           (and (equal (process-exit-status proc) 0)
                (kill-process proc))
           (kill-buffer buffer)))
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

(defun* django-start-console (&optional prefix &key (name django-default-name) (settings django-default-settings))
  (interactive "P")
  (let* ((name (django-server-buffer-name prefix))
        (server (or (gethash name django-servers)
                    (make-django-server :name name)))
        (settings (or (and prefix (django-settings prefix))
                      (django-server-settings server)))
        (console (django-server-console server))
        (proc (django-server-console-proc server))
        (console-proc-running? (and console proc (equal (process-exit-status proc) 0))))

    (cond (console-proc-running?
           (message "Console is running, switching")
           (switch-to-buffer console))
          (t
           (message "Console is not running, starting, switching")))
    (message "Will spawn console for: %s %s" name settings)))

(defun django-log-update-hook (beg end length)
  (let ((text (buffer-substring beg end)))
    (message "Beg(%s) End(%s) Length(%s)" beg end length)
    (message "Text: %s" text)
    nil))

(defun django-start-server (server)
  (django-stop-all-running-servers)
  (let* ((server (django-stop-server server))
         (bname (concat "*django-server-" (django-server-name server) "*"))
         (buffer (get-buffer-create bname))
         (process (start-process-shell-command bname buffer "django-admin.py" "runserver"
                                               (format "%s:%s" (django-server-host server) (django-server-port server))
                                               (concat "--settings=" (django-server-settings server) ))))
    (with-current-buffer buffer
      (make-variable-buffer-local 'after-change-functions)
      (add-hook 'after-change-functions 'django-log-update-hook)
      (make-variable-buffer-local 'django-project-name)
      (setf django-project-name (django-server-name server))
      (django-ide-server-mode))
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
    (message "start-or-restart: Server: %s" server)
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

(defun django-reload-current-server (&optional prefix)
  (interactive "P")
  (let ((name (assoc-default 'django-project-name (buffer-local-variables (current-buffer)))))
    (message "Reload: %s" name)
    (django-restart-server (gethash name django-servers))
    (django-switch-to-running-server)))

(define-minor-mode django-ide-server-mode "Mode for running django-ide server instances" nil " *Django-Server*"
  :keymap `(("R" . django-reload-current-server)
            ("q" . (lambda ()
                     (interactive)
                     (switch-to-buffer nil))))

  (cond (django-ide-server-mode
         (message "Setting up django-ide-server-mode"))
        (t
         (message "Tearing down django-ide-server-mode"))))

(global-set-key [(control c) (shift s)] 'django-spin-server)
(global-set-key [(control c) (shift w)] 'django-switch-to-running-server)

(provide 'django-ide)
