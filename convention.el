;; change make process to make repl


;; pkg imports
(require 's)

;;local imports
(require 'convention-docker)
(require 'convention-utils)
(require 'convention-lang-info)
(require 'convention-layers)
(require 'convention-image)
(require 'convention-container)
(require 'convention-start-container)
(require 'convention-connect-container)
(require 'convention-execute)

;; config
(defvar convention-dir (or (file-name-directory load-file-name)
                           (file-name-directory buffer-file-name))
  "Absolute path of the directory containing convention code on the end user's machine")

;; provide
(provide 'convention)
