(defun convention-get-repl-buffers ()
  "Returns a list of buffers which contain active processes that are connected to
   convention containers"
  (seq-filter
   (function (lambda (x) (string-match-p (regexp-quote "convention") x)))
   (mapcar (function (lambda (x) (buffer-name (process-buffer x) ))) (process-list))))


(defun convention-set-db-params (arg container-name)
  "Sets local variables which contain data required to connect to a database container-name"
    (when (and (convention-is-sql container-name)
               (or arg (not (boundp 'convention-db-password))))
      (setq-local convention-db-port (convention-prompt-for-db-port))
      (setq-local convention-db-password (convention-prompt-for-db-password))
      (setq-local convention-db-username (convention-query-lang-info container-name "root-user-name"))))


(defun convention-prompt-for-repl-buffer ()
  "Returns a string representing the name of a buffer containing a repl
   that is associated with a running convention container"
  (ido-completing-read "Process: " (convention-get-repl-buffers)))


(defun convention-associate-text-buffer-to-container (arg)
  "'Associates' a text buffer - that is a buffer containing LANG code - to a
   running container.  This 'association' constitutes the setting of local
   variables"
  (let ((container-name (if (or arg (not (boundp 'convention-container-target)))
                            (convention-prompt-for-running-container)
                          (message convention-container-target))))
    (setq-local convention-container-target container-name)
    (convention-set-db-params arg container-name)))



(defun convention-associate-text-buffer-to-repl (arg)
  "'Associates' a text buffer - that is a buffer containing LANG code - to a
   buffer which contains an active process that is connected to convention containers.
   This 'association' constitutes the setting of local variables"
  (let ((repl-target (if (or arg
                              (not (boundp 'convention-repl-target))
                              (not (process-live-p (get-buffer-process convention-repl-target))))
                          (convention-prompt-for-repl-buffer)
                        (message convention-repl-target))))
    (setq-local convention-repl-target repl-target)))



(defun convention-associate-text-buffer (arg to)
  "Associates a text buffer TO either a conatiner or process buffer that is connected
   to a convention contaier" 
  (if (equal to "repl")
      (convention-associate-text-buffer-to-repl arg)
    (convention-associate-text-buffer-to-container arg)))


(defun convention-get-bounds (&optional region)
  "Returns a 2 length list representing the bounds for
   either the entire buffer or the REGION"
  (if region
      (if (use-region-p)
          (cons (region-beginning) `(,(region-end)))
        (cons (point-at-bol) `(,(point-at-eol))))
    (cons (point-min) `(,(point-max)))))


(defun convention-exec-in-repl (arg &optional region)
  "Executes code contained in either the entire buffer or a REGION
   in a process buffer connected to a convention container"
  (let* ((bounds (convention-get-bounds region))
         (str (buffer-substring (nth 0 bounds) (nth 1 bounds))))
    (convention-associate-text-buffer arg "repl")
    (if (equal "\n" (substring str (- (length str) 1) (length str)))
        (process-send-string convention-repl-target str)
      (process-send-string convention-repl-target (concat str "\n")))))


(defun convention-exec-region-in-repl (arg)
  "Executes code contained in a region in a process buffer connected to a convention container"
  (interactive "P")
  (convention-exec-in-repl arg t))


(defun convention-exec-buffer-in-repl (arg)
  "Executes code contained in the entire buffer in a process buffer connected to a convention container"
  (interactive "P")
  (convention-exec-in-repl arg))


(defun convention-format-run-lang-cmd (container-name)
  "Returns a string reprenting the command used to execute code contained in a file"
  (let ((run-lang-cmd (convention-query-lang-info container-name "run-lang-cmd")))
    (if (convention-is-sql container-name)
        (s-format run-lang-cmd 'aget `(("password" . ,convention-db-password)
                                       ("username" . ,convention-db-username)
                                       ("port" . ,convention-db-port)))
      (message run-lang-cmd))))



(defun convention-format-exec-as-file-cmd ()
  "Returns a string representing a command which executes code residing in the .convention_code file
   contained in a running container's code directory"
  (let ((run-lang-cmd (convention-format-run-lang-cmd convention-container-target)))
    (s-format convention-docker-command-exec-as-file 'aget
              `(("container" . ,convention-container-target)
                ("run-lang-cmd" . ,run-lang-cmd)))))


(defun convention-exec-as-file (arg &optional region)
  "Executes code contained in either the entire buffer or a REGION in a
   a runing container.  This code is executed as a file int he container"
  (convention-associate-text-buffer arg "container")
  (let ((bounds (convention-get-bounds region))
         (cmd (convention-format-exec-as-file-cmd)))
    (write-region (nth 0 bounds) (nth 1 bounds) "./.convention_code" nil) 
    (async-shell-command cmd)))


(defun convention-exec-region-as-file (arg)
  "Executes code in a region as a file on a running container"
  (interactive "P")
  (convention-exec-as-file arg t))

(defun convention-exec-buffer-as-file (arg)
  "Executes code contained in the enture buffer as a file on a running container"
  (interactive "P")
  (convention-exec-as-file arg))


(provide 'convention-execute)
