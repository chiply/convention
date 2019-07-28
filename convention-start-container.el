(defvar convention-container-code-dir ":/workdir/code"
  "A string representing the code directory on the convention container")

(defvar convention-container-data-dir ":/workdir/data"
  "A string representing the data directory on the convention container")

(defvar convention-container-config-dir ":/workdir/.config"
  "A string representing the config directory on the convention container")


(defun convention-user-input-dir (dir-type)
  "Returns a string representing a user selected
   local DIR-TYPE directory"
  (ido-read-directory-name (concat "Local" " " dir-type " " "directory: ") ""))



(defun convention-start-container-format-code-mount-frag ()
  "Returns a string representing the mount fragment for a local code directory"
  (if (equal (ido-completing-read "Mount a code directory? " '("no" "yes")) "yes")
      (concat "-v " (convention-user-input-dir "code") ":" convention-container-code-dir)
    ""))

(defun convention-start-container-format-data-mount-frag ()
  "Returns a string representing the mount fragment for a local data directory"
  (if (equal (ido-completing-read "Mount a data directory? " '("no" "yes")) "yes")
      (concat "-v " (convention-user-input-dir "data") ":" convention-container-data-dir)
    ""))

(defun convention-start-container-format-config-mount-frag ()
  "Returns a string representing the mount fragment for a local config directory"
  (if (equal (ido-completing-read "Mount a config directory? " '("no" "yes")) "yes")
      (concat "-v " (convention-user-input-dir "config") ":" convention-container-config-dir)
    ""))

(defun convention-start-container-format-port-map-frag ()
  "Returns a string representing the port map fragment"
  (if (equal (ido-completing-read "Set up port forwarding? " '("no" "yes")) "yes")
      (concat "-p "
              (ido-completing-read "Local port (the port on your machine): "
                                   ())
              ":" 
              (ido-completing-read "Remote port (the port in the container): "
                                   ())
              " ")
    ""))



(defun convention-start-container-format-name-frag (image)
  "Returns a string representing a name fragment for a container"
  (let ((name-prefix (concat
                      "convention-"
                      (nth 0 (split-string
                              (nth 1 (split-string image  "/" )) "-"))
                      "-")))
    (concat "--name "
            name-prefix
            (ido-completing-read
             (concat "Name the container with " name-prefix)
             ()))))


(defun convention-prompt-for-db-password ()
  "Returns a string representing the password for a database
   specified by user input"
  (ido-completing-read "Enter the database password: " ()))


(defun convention-start-container-format-extra-frag (user-image-name)
  "Returns a string representing extra fragments for the docker start command"
  (let ((password (if (convention-is-sql user-image-name) (convention-prompt-for-db-password) (message "")))
        (extra-frags (convention-query-lang-info user-image-name "extra-frags"))) 
    (if (convention-is-sql user-image-name)
        (s-format extra-frags
                  'aget `(("password" . ,password)))
      ""))) 


(defun convention-start-container-format-db-cmd (user-image-name)
  "Returns a string representing the command used to start a database"
  (if (convention-is-sql user-image-name)
      (convention-query-lang-info user-image-name "start-database-cmd")
    ""))


(defun convention-format-start-container-cmd ()
  "Returns a string representing the docker start command.  The specifics in this
   command are determined in part by the the USER-IMAGE-NAME name"
  (let* ((user-image-name (convention-prompt-for-image))
         (name-frag (convention-start-container-format-name-frag user-image-name))
         (code-mount-frag (convention-start-container-format-code-mount-frag))
         (data-mount-frag (convention-start-container-format-data-mount-frag)) 
         (config-mount-frag (convention-start-container-format-config-mount-frag))
         (port-map-frag (convention-start-container-format-port-map-frag))
         (extra-frag (convention-start-container-format-extra-frag user-image-name))
         (db-cmd (convention-start-container-format-db-cmd user-image-name)))
    (s-format convention-docker-start-container-command
              'aget `(("name-frag" . ,name-frag)
                      ("code-mount-frag" . ,code-mount-frag)
                      ("data-mount-frag" . ,data-mount-frag)
                      ("config-mount-frag" . ,config-mount-frag)
                      ("port-map-frag" . ,port-map-frag)
                      ("extra-frag" . ,extra-frag)
                      ("user-image-name" . ,user-image-name)
                      ("db-cmd" . ,db-cmd)))))


(defun convention-start-container ()
  "Starts a container"
  (interactive)  
  (let ((cmd (convention-format-start-container-cmd)))
    (shell-command cmd)))

(provide 'convention-start-container)
