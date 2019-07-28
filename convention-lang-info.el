(defvar convention-language-info-alist
      `(
        ("python" . (
                     ("install-info" . (
                                        ("install-script" . "install_python.sh")
                                        ("default-req" . ("ipython"))
                                        ("surround" . "")
                                        ("delim" . " ")
                                        ("cmd-line-util" . "/bin/bash") ; this is effectively bash
                                        ))
                     ("default-repl-list" . ("python" "ipython" "bash"))
                     ("run-lang-cmd" . "python")
                     ))
        ("r-base" . (
                     ("install-info" . (
                                        ("install-script" . "install.R")
                                        ("default-req" . "")
                                        ("surround" . "\\\"")
                                        ("delim" . ", ")
                                        ("cmd-line-util" . "Rscript") ;note the space here.
                                        ))
                     ("default-repl-list" . ("R" "bash"))
                     ("run-lang-cmd" . "Rscript")
                     ))
        ("julia" . (
                     ("install-info" . (
                                        ("install-script" . "install_julia.sh")
                                        ("default-req" . "")

                                        ("surround" . "\\\"")
                                        ("delim" . ", ")
                                        ("cmd-line-util" . "/bin/bash") ;note the space here.
                                        ))
                     ("default-repl-list" . ("julia" "bash"))
                     ("run-lang-cmd" . "julia")
                     ))
        ("node" . (
                   ("install-info" . (
                                      ("install-script" . "install_node.sh")
                                      ("default-req" . "")
                                      ("surround" . "")
                                      ("delim" . " ")
                                      ("cmd-line-util" . "/bin/bash") ;note the space here.
                                      ))
                    ("default-repl-list" . ("node" "bash"))
                    ("run-lang-cmd" . "node")
                    ))
        ("golang" . (
                   ("install-info" . (
                                      ("install-script" . "install_go.sh")
                                      ("default-req" . ("github.com/motemen/gore/cmd/gore"
                                                        "github.com/mdempsky/gocode"
                                                        "github.com/k0kubun/pp"))
                                      ("surround" . "")
                                      ("delim" . " ")
                                      ("cmd-line-util" . "/bin/bash") ;note the space here.
                                      ))
                    ("default-repl-list" . ("gore" "bash"))
                    ("run-lang-cmd" . "go run ")
                    ))
        ("ruby" . (
                   ("install-info" . (
                                      ("install-script" . "install_ruby.sh")
                                      ("default-req" . "")
                                      ("surround" . "")
                                      ("delim" . " ")
                                      ("cmd-line-util" . "/bin/bash") ;note the space here.
                                      ))
                    ("default-repl-list" . ("irb" "bash"))
                    ("run-lang-cmd" . "ruby")
                    ))
        ("mssql" . (
                       ("extra-frags" .,(concat
                                         "-e 'ACCEPT_EULA=Y' "
                                         "-e 'SA_PASSWORD=${password}' "
                                         "-e 'MSSQL_PID=Developer' " ))
                       ("start-database-cmd" . "/opt/mssql/bin/sqlservr ")
                       ("root-user-name" . "sa")
                       ("default-repl-list" . ("sql-cli" "bash"))
                       ("dbcli-program" . "mssql-cli ")
                       ("default-cli-program" . "/opt/mssql-tools/bin/sqlcmd ")
                       ("connection-string" .  "-S localhost -U ${username} -P '${password}'")
                       ("run-lang-cmd" . "/opt/mssql-tools/bin/sqlcmd -S localhost,${port} -U ${username} -P '${password}' -i ")
                       ))
        ("postgres" . (
                       ("extra-frags" .,(concat
                                         "-e POSTGRES_PASSWORD=${password} "))
                       ("start-database-cmd" . "postgres ")
                       ("root-user-name" . "postgres")
                       ("default-repl-list" . ("sql-cli" "bash"))
                       ("dbcli-program" . "pgcli ")
                       ("default-cli-program" . "psql ")
                       ("connection-string" .  "postgres://${username}:${password}@localhost:${port}")
                       ("run-lang-cmd" . "psql postgres://${username}:${password}@localhost:${port} -f ")
                       
                       ))
        ("mysql" . (
                       ("extra-frags" .,(concat
                                         "-e MYSQL_ROOT_PASSWORD=${password} "))
                       ("start-database-cmd" . "mysqld ")
                       ("root-user-name" . "root")
                       ("default-repl-list" . ("sql-cli" "bash"))
                       ("dbcli-program" . "mycli ")
                       ("default-cli-program" . "mysql ")
                       ("connection-string" .  "-h localhost -u ${username} -P ${port} --password=${password} ")
                       ("run-lang-cmd" .  "mysql -h localhost -u ${username} -P ${port} --password=${password} < ")
                       ))
        ("mariadb" . (
                       ("extra-frags" .,(concat
                                         "-e MYSQL_ROOT_PASSWORD=${password} "))
                       ("start-database-cmd" . "mysqld ")
                       ("root-user-name" . "root")
                       ("default-repl-list" . ("sql-cli" "bash"))
                       ("dbcli-program" . "mycli ")
                       ("default-cli-program" . "mysql ")
                       ("connection-string" .  "-h localhost -u ${username} -P ${port} -p${password} ")
                       ("run-lang-cmd" .  "mysql -h localhost -u ${username} -P ${port} -p${password} < ")
                       ))
        ))



(defun convention-list-supported-langs ()
  "Returns a list representing languages supported by convention"
  (mapcar 'car convention-language-info-alist))

(defun convention-guess-lang-from-image-or-container-name (image-or-container-name)
  "Returns a string representing the language associated with an image or
   container based on the image name or container name, respectively"
  (nth 0 (seq-filter (function (lambda (x) (string-match-p (regexp-quote x) image-or-container-name))) 
              (convention-list-supported-langs)) ))

(defun convention-query-lang-info (image-or-container-name label)
  "Queries the convention-lang-info alist and returns the element specified
   by the KEY-TUPLE"
  (let ((lang ( convention-guess-lang-from-image-or-container-name image-or-container-name)))
    (convention-util-recursive-assoc-cdr `(,lang ,label) convention-language-info-alist)))

(defun convention-query-install-info (image-or-container-name label)
  "Queries the convention-lang-info sub-alist labeled by install-info
   and returns the element specified by the given LABEL for a given LANG"
  (let ((lang (convention-guess-lang-from-image-or-container-name image-or-container-name)))
    (convention-util-recursive-assoc-cdr `(,lang "install-info" ,label) convention-language-info-alist)))



(provide 'convention-lang-info)
