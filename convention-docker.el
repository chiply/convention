;; use alist here instead?
;; image
(defvar convention-docker-command-search-image-names
  "docker search --format='{{.Name}}' ${search-term}"
  "A pre-formatted string representing a docker cli command that returns a list of images
   mathcing a search term")

(defvar convention-docker-command-list-image-tags
  "${convention-dir}/dockertags.sh ${base-image-name}"
  "A pre-formatted string representing the cli command which runs the
   dockertags.sh script, which returns a list of docker
   tags for a given base-image-name")

;; change to convention docker name ...
(defvar convention-docker-image-name-and-tag
  "${base-image-name}:${image-tag}"
  "A pre-formatted string representing an image:tag name format")

(defvar convention-docker-user-image-name
   "convention/${lang}-${user-image-postfix}")

(defvar convention-docker-command-build-image
  (concat
   "docker build --no-cache -t ${user-image-name} -f- ${convention-dir}/install_scripts/ <<EOF \n"
   "${base-layer}\n"
   "${req-layer}\n"
   "CMD [\"bash\"]\n"
   "EOF")
  "A pre-formatted string representing a docker cli command that
   builds a container")

(defvar convention-docker-command-list-images
  "docker images | grep convention | awk '{print $1}'"
  "A pre-formatted string representing a cli command which lists all
   docker images containing the string 'convention' in their name")

(defvar convention-docker-command-remove-image
  "docker image rm ${user-image-name}"
  "A pre-formatted string representing a docker cli command
   which removes an image")


;; container

(defvar convention-docker-command-detect-all-containers
 "docker ps -a | grep convention | awk '{print $1}'"
 "A string representing a command that returns a list of all contianers
  (stopped or started) with convention in their name")


(defvar convention-docker-command-detect-running-containers
  "docker ps | grep convention | awk '{print $1}'"
 "A string representing a command that returns a list of running contianers
  (stopped or started) with convention in their name"
  )

(defvar convention-docker-command-detect-stopped-containers
  "docker ps --filter \"status=exited\" | grep convention | awk '{print $1}'"
  "A string representing a command that returns a list of stopped contianers
  (stopped or started) with convention in their name" )

(defvar convention-docker-command-list-all-containers
   "docker inspect --format='{{.Name}}' $(docker ps -aq) | grep convention"
   "A pre-formatted string representing a docker cli command which returns a
    list of containers containing the string 'convention' in their name")


(defvar convention-docker-command-list-running-containers
   "docker inspect --format='{{.Name}}' $(docker ps -q) | grep convention"
   "A pre-formatted string representing a docker cli command which returns a
    list of running containers containing the string 'convention' in their name")


(defvar convention-docker-command-list-stopped-containers
  "docker inspect  --format={{.Name}} $(docker ps --filter \"status=exited\" | grep convention | awk '{print $1}')"
  "A string representing a docker command which lists stopped docker containers
   with convention in their name")

(defvar convention-docker-command-stop-container
  "docker stop ${container}"
  "A pre-formatted string representing a docker cli command which stops and removes a
   container by name")

(defvar convention-docker-command-stop-and-remove-container
  "docker stop ${container} || TRUE && docker rm ${container}"
  "A pre-formatted string representing a docker cli command which stops and removes a
   container by name")

(defvar convention-docker-command-start-a-stopped-container
  "docker start ${container}"
  "A pre-formatted string representing a docker cli command which starts a
   convention container that has been stopped")



(defvar convention-docker-start-container-command
  (concat
   "docker run -dt"
   " "
   "${name-frag}"
   " "
   "${code-mount-frag}"
   " "
   "${data-mount-frag}"
   " "
   "${config-mount-frag}"
   " "
   "${port-map-frag}"
   " "
   "${extra-frag}"
   " "
   "${user-image-name}"
   " "
   "${db-cmd}"
   )
  "A pre-formatted string representing the a docker cli command which
   starts a container")



(defvar convention-docker-process-buffer-name
  "${container-name}-${container-name-user-postfix}"
  "A pre-formatted string representing the name of the process
   buffer containing a repl connected to a container")

(defvar convention-docker-command-connect-to-container
  "docker exec -it ${container-name} ${cmd}"
  "A pre-fomratted string representing a docker cli command which
   starts a repl connected to a container")

(defvar convention-docker-command-exec-as-file
  "docker cp ./.convention_code ${container}:/workdir/code && docker exec ${container} /bin/sh -c \"${run-lang-cmd} /workdir/code/.convention_code\""
  "A pre-formatted string representing a command which copies a file named
   .convention_code into a docker container and executes this code in the
   docker container")


(provide 'convention-docker)
