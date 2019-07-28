(defun convention-prompt-for-container-name-user-postfix (container-name)
  "Returns a string representing a postfic for a particular container"
  (ido-completing-read (concat "Enter a name for your process buffer: " container-name "-") ()))


(defun convention-format-process-buffer-name (container-name)
  "Returns a string representing the name of a process buffer containing
   a repl connected to a container"
  (let ((container-name-user-postfix (convention-prompt-for-container-name-user-postfix container-name)))
    (s-format convention-docker-process-buffer-name
              'aget `(("container-name" . ,container-name)
                      ("container-name-user-postfix" . ,container-name-user-postfix)))))


(defun convention-prompt-for-container-command (container-name)
  "Returns a string representing the command to be sent to a container
   when docker exec-ing into it"
  (let ((default-repl-list (convention-query-lang-info container-name "default-repl-list")))
    (ido-completing-read
     "Command to send to container: " default-repl-list)))


(defun convention-start-process (container-name) 
  "Starts an ansi term process in which a docker exec ... command will be
   executed"
  (let ((buffer-name (convention-format-process-buffer-name container-name)))
    (split-window-below)
    (windmove-down)
    (ansi-term "bash" buffer-name)))


(defun convention-prompt-for-sql-cli-type ()
  "Returns a string representing a cli type for a database engine.  The options are
   default, which will be the default cli interface (usually comes with the database engine),
   and dbcli, which will be the corresponding dbli tool for the specific database engine"
  (ido-completing-read "Use default cli interface or dbcli? " '("default" "dbcli")))


(defun convention-get-sql-cli-program (container-name)
  "Returns a string representing the cli program to be used with the database
   engine specified"
  (let ((dbcli-p (convention-prompt-for-sql-cli-type)))
    (if (equal dbcli-p "default")
        (convention-query-lang-info container-name "default-cli-program")
      (convention-query-lang-info container-name "dbcli-program"))))


(defun convention-prompt-for-db-port ()
  "Returns a string representing the port on which the database is running (inside
   of the container)"
  (ido-completing-read "Enter the database port: " ()))



(defun convention-format-db-connection-string (container-name)
  "Returns a string representing the connection string for a specific database"
  (let* ((port (convention-prompt-for-db-port))        
         (password (convention-prompt-for-db-password))
         (root-user-name (convention-query-lang-info container-name "root-user-name"))
         (connection-template (convention-query-lang-info container-name "connection-string")))
  (s-format connection-template 'aget `(("username" . ,root-user-name)
                                        ("port" . ,port)
                                        ("password" . ,password)))))


(defun convention-format-connect-to-db (container-name) 
  "Returns a string representing the command used to connect to a specific database
   engine"
  (let* ((sql-cli-program (convention-get-sql-cli-program container-name))
         (connection-string (convention-format-db-connection-string container-name)))
    (concat sql-cli-program " " connection-string)))


(defun convention-format-connect-to-container-cmd (container-name)
  "Returns a string representing the docker command used to connect to a container"
  (let* ((base-cmd (convention-prompt-for-container-command container-name))
         (cmd (if (and (convention-is-sql container-name) (not (equal base-cmd "bash")))
                  (convention-format-connect-to-db container-name)
                (message base-cmd))))
    (s-format convention-docker-command-connect-to-container
              'aget `(("container-name" . ,container-name)
                      ("cmd" . ,cmd)))))

(defun convention-connect-to-container ()
  "Connects to a container.  A 'connection' in this context refers to the act of
   opening a opening a repl connected to a container.  This repl could be bash or
   a language specific cli"
  (interactive)
  (let* ((container-name (convention-prompt-for-running-container))
         (connect-to-container-cmd (convention-format-connect-to-container-cmd container-name)))
    (convention-start-process container-name)
    (term-send-invisible connect-to-container-cmd)))


(provide 'convention-connect-container)
