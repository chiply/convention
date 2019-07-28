(defun convention-detect-all-containers ()
  "Returns a boolean indicating whether there are all containers
   with 'convention' in their name"
  (> (length (shell-command-to-string convention-docker-command-detect-all-containers)) 0))


(defun convention-list-all-containers ()
  "Lists all containers containing 'connvention' in their name"
  (remove "" (split-string (replace-regexp-in-string "/" "" (shell-command-to-string convention-docker-command-list-all-containers)) "\n")))


(defun convention-prompt-for-all-container ()
  "Returns a string representing the name of a user selected container"
  (interactive)
  (if (convention-detect-all-containers)
      (ido-completing-read "Available containers: " (convention-list-all-containers))
    (error "There are no containers! You can start a container with M-x convention-start-container")))


(defun convention-detect-running-containers ()
  "Returns a boolean indicating whether there are any running containers
   with 'convention' in their name"
  (> (length (shell-command-to-string convention-docker-command-detect-running-containers)) 0))


(defun convention-list-running-containers ()
  "Lists all containers containing 'connvention' in their name"
  (remove "" (split-string (replace-regexp-in-string "/" "" (shell-command-to-string convention-docker-command-list-running-containers)) "\n")))


(defun convention-prompt-for-running-container ()
  "Returns a string representing the name of a user selected container"
  (interactive)
  (if (convention-detect-running-containers)
      (ido-completing-read "Available containers: " (convention-list-running-containers))
    (error "There are no running containers! You can start a container with M-x convention-start-container")))


(defun convention-format-stop-and-remove-container-command () 
  "Returns a string representing a docker cli command to stop
   and remove a container"
  (let ((container (convention-prompt-for-all-container)))
    (s-format
     convention-docker-command-stop-and-remove-container
     'aget
     `(("container" . ,container)))))


(defun convention-stop-and-remove-container ()
  "Removes a user specified container"
  (interactive)
  (let ((cmd (convention-format-stop-and-remove-container-command)))
    (shell-command cmd)))


(defun convention-format-stop-container-command () 
  "Returns a string representing a docker cli command to stop a
   running container"
  (let ((container (convention-prompt-for-running-container)))
    (s-format
     convention-docker-command-stop-container
     'aget
     `(("container" . ,container)))))


(defun convention-stop-container ()
  "Stops a running container"
  (interactive)
  (let ((cmd (convention-format-stop-container-command)))
    (shell-command cmd)))

(defun convention-detect-stopped-containers ()
  (> (length (shell-command-to-string convention-docker-command-detect-stopped-containers)) 0))


(defun convention-list-stopped-containers ()
  (remove "" (split-string (replace-regexp-in-string "/" "" (shell-command-to-string convention-docker-command-list-stopped-containers)) "\n")))



(defun convention-prompt-for-stopped-container ()
  (interactive)
  (if (convention-detect-stopped-containers)
      (ido-completing-read "Available containers: " (convention-list-stopped-containers))
    (error "There are no stopped containers!") ))


(defun convention-format-start-a-stopped-container-command () 
  "Returns a string representing a docker cli command to stop a
   running container"
  (let ((container (convention-prompt-for-stopped-container)))
    (s-format
     convention-docker-command-start-a-stopped-container
     'aget
     `(("container" . ,container)))))


(defun convention-start-a-stopped-container ()
  (interactive)
  (let ((cmd (convention-format-start-a-stopped-container-command)))
    (shell-command cmd)))


(provide 'convention-container)
