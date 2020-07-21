;; no template
;; combine container (start, connect)

;; pkg imports
(require 's)


(defun convention-prompt-for-image-search-term-from-user-input ()
  "Returns a string representing a search term specified by user input"
  (read-string "Enter a search term for an image: "))


(defun convention-prompt-for-image-search-term-from-preset ()
  "Returns a a string representing a search term selected from a
   list of presets"
  (ido-completing-read "Select an image: "
                       '("python"
                         "r-base"
                         "julia"
                         "node"
                         "golang"
                         "ruby"
                         "mssql"
                         "mysql"
                         "mariadb"
                         "postgres")))

(defun convention-prompt-for-image-search-term (from-preset)
  "Prompts for a search term for the docker search command"
  (if from-preset
      (convention-prompt-for-image-search-term-from-preset)
    (convention-prompt-for-image-search-term-from-user-input)))


(defun convention-search-base-image-names (from-preset)
  "Returns a list of search results based on user input using
   the docker search utility"
  (let* ((search-term (convention-prompt-for-image-search-term from-preset))
         (cmd (s-format convention-docker-command-search-image-names
                        'aget `(("search-term" . ,search-term)))))
    (split-string (shell-command-to-string cmd))))

(defun convention-prompt-for-base-image-name (from-preset)
  "Returns a string representing a base image name based on user selection"
  (ido-completing-read "Select a base-image-name: " (convention-search-base-image-names from-preset)))



(defun convention-search-image-tags (base-image-name)
  "Returns a list of docker tags for a BASE-IMAGE-NAME"
  (split-string (shell-command-to-string
                 (s-format convention-docker-command-list-image-tags
                           'aget `(("convention-dir" . ,convention-dir)
                                   ("base-image-name" . ,base-image-name))))))


(defun convention-prompt-for-image-tag (base-image-name)
  "Returns a string representing a docker tag for a BASE-IMAGE-NAME
   based on user selection"
  (ido-completing-read "Select a tag: " (convention-search-image-tags base-image-name)))


(defun convention-format-image-name-and-tag (from-preset)
  "Returns a string representing an image:tag name"
  (let* ((base-image-name (convention-prompt-for-base-image-name from-preset))
         (image-tag (convention-prompt-for-image-tag base-image-name)))
    (s-format convention-docker-image-name-and-tag
              'aget `(("base-image-name" . ,base-image-name)
                      ("image-tag" . ,image-tag)))))


(defun convention-format-base-layer (base-image-name-and-tag)
  "This function assembles the base layer for the Dockerfile given a BASE-IMAGE-NAME-AND-TAG"
  (let ((convention-layers-base (convention-query-layers '("base"))))
    (s-format convention-layers-base
              'aget `(("base-image-name-and-tag" . ,base-image-name-and-tag)))))


(defun convention-prompt-for-user-image-postfix (base-image-name)
  "Returns a string representing a user specified name of an image.
   Note that the image name is prefixed with convention/LANG.  This
   is essential to make the image discoverable by convention and to
   ensure the image is being processed properly based on language
   downstream"
  (let ((lang (if (convention-guess-lang-from-image-or-container-name base-image-name)
                  (convention-guess-lang-from-image-or-container-name base-image-name)
                (message base-image-name)
                )))
    (ido-completing-read (concat "Name the image with convention/" lang "-") ())))


(defun convention-format-user-image-name (base-image-name)
  "Returns a string representing an image name based on user input. Note the presence of
   'convention' and LANG are required to be in the name in order for the image to be
   discoverable by convention"
  (let ((lang (if (convention-guess-lang-from-image-or-container-name base-image-name)
                  (convention-guess-lang-from-image-or-container-name base-image-name)
                (message base-image-name)
                ))
        (user-image-postfix (convention-prompt-for-user-image-postfix base-image-name)))
    (s-format convention-docker-user-image-name
              'aget `(("lang" . ,lang)
                      ("user-image-postfix" . ,user-image-postfix)))))




(defun convention-prompt-for-requirements-file ()
  "Returns a string representing the location of a file containing the new-line separated
   names of third-party packages"
  (ido-read-file-name "Choose a requirements file: " ""))


(defun convention-prompt-for-requirements-file-yn ()
  "Returns a yes or no indicating whether the user wants
   to specify a requirements file for their image"
  (ido-completing-read "Install dependencies from a requirements file? " '("no" "yes")))

(defun convention-format-reqs-list (base-image-name-and-tag)
  "Returns a string representing the package list portion of the requirements scripts for a given LANG.
   These requirements come from a user specified file containing newline seperated third-party package names"
  (let* ((default-req-lst (convention-query-install-info base-image-name-and-tag "default-req"))
         (default-req-str (concat (mapconcat 'identity
                                             default-req-lst "\n") "\n"))
         (from-file (convention-prompt-for-requirements-file-yn))
         (filepath (if (equal from-file "yes") (convention-prompt-for-requirements-file)))
         (surround (convention-query-install-info base-image-name-and-tag "surround"))
         (delim (convention-query-install-info base-image-name-and-tag "delim")))
    (with-temp-buffer
      (insert default-req-str)
      (if filepath (insert-file-contents filepath))
      (mapconcat
       (function (lambda (x) (concat surround x surround)))
       (split-string (buffer-string) "\n" t) delim))))


(defun convention-format-layer-req-sql (base-image-name-and-tag)
  "Rerturns a string representing the requirements layer for a container
   running a database engine with the appropriate DBCLI-PROGRAM"
  (let ((req-layer (convention-get-layer-req base-image-name-and-tag))
        (dbcli-program (convention-query-lang-info base-image-name-and-tag "dbcli-program")))
    (s-format req-layer 'aget `(("dbcli-program" . ,dbcli-program)))))


(defun convention-format-layer-req-lang (base-image-name-and-tag)
  "Returns a string representing the requirements layer for a container
   not running a database engine with the appropriate INSTALL-SCRIPT, REQUIREMENTS,
   and CMD-LINE-UTIL"
  (let ((req-layer (convention-get-layer-req base-image-name-and-tag))
        (install-script (convention-query-install-info base-image-name-and-tag "install-script"))
        (requirements (convention-format-reqs-list base-image-name-and-tag))
        (cmd-line-util (convention-query-install-info base-image-name-and-tag "cmd-line-util")))
    (if (> (length requirements) 0) (s-format req-layer 'aget `(("install-script" . ,install-script)
                                                                ("requirements" . ,requirements)
                                                                ("cmd-line-util" . ,cmd-line-util)))
      "")))



(defun convention-format-req-layer (base-image-name-and-tag)
  "Returns a string representing the requirements layer for a given IMAGE.
   Of note, there can be null requirements, or a list of requirements decsribed by a user
   specified file of newline separated third-party package names"
  (if (convention-is-sql base-image-name-and-tag)
      (convention-format-layer-req-sql base-image-name-and-tag)
    (convention-format-layer-req-lang base-image-name-and-tag)))



(defun convention-format-build-command (from-preset)
  "Returns a string representing the docker build command."
  (let* ((base-image-name-and-tag (convention-format-image-name-and-tag from-preset))
         (req-layer (convention-format-req-layer base-image-name-and-tag))
         (user-image-name (convention-format-user-image-name base-image-name-and-tag))
         (base-layer (convention-format-base-layer base-image-name-and-tag)))
    (s-format convention-docker-command-build-image
              'aget `(("user-image-name" . ,user-image-name)
                      ("convention-dir" . ,convention-dir)
                      ("base-layer" . ,base-layer)
                      ("req-layer" . ,req-layer)))))


(defun convention-build-image (from-preset)
  "Build an image with the docker-build command"
  (let ((cmd (convention-format-build-command from-preset)))
    (async-shell-command cmd)))


(defun convention-build-image-from-search-term ()
  (interactive)
  "Builds an image from search term"
  (convention-build-image nil))

(defun convention-build-image-from-preset ()
  "Builds an image from preset"
  (interactive)
  (convention-build-image t))




(defun convention-list-images ()
  "Returns a list of images with 'convention' in the name"
  (remove ""
          (split-string
           (shell-command-to-string convention-docker-command-list-images)
           "\n")))


(defun convention-prompt-for-image ()
  "Returns a string representing an image name, based on a user selection"
  (interactive) 
  (concat (ido-completing-read "Available images: " (convention-list-images)) " "))


(defun convention-format-remove-image-cmd ()
  "Returns a string represnting a docker cli command to remove
   an image specified by the user"
  (let ((user-image-name (convention-prompt-for-image)))
    (s-format convention-docker-command-remove-image
              'aget `(("user-image-name" . ,user-image-name)))))



(defun convention-remove-image ()
  "Removes an image based on user selection"
  (interactive)
  (shell-command (convention-format-remove-image-cmd)))


(provide 'convention-image)
