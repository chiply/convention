(defvar convention-image-layers
      `(
        ("base" . ,(concat
                    "FROM ${base-image-name-and-tag} \n"
                    "RUN mkdir -p /workdir/code \n"
                    "RUN mkdir -p /workdir/data \n"
                    "WORKDIR /workdir \n"
                    "ENV HOME=/workdir \n"
                    ))
        ("sql-req" . ,(concat
                       "RUN apt-get -yqq update \n"
                       "RUN apt-get -yqq install libpq-dev build-essential \n"
                       "RUN apt-get -yqq install python-pip \n"
                       "RUN pip install ${dbcli-program} \n"
                       ))
        ("req" . ,(concat
                   "RUN apt-get -yqq update \n"
                   "RUN apt-get -yqq install wget \n"
                   "RUN wget https://raw.githubusercontent.com/chiply/convention/master/install_scripts/${install-script} \n"
                   "RUN awk '{gsub(/CONVENTION_REQUIREMENTS/,\"${requirements}\")}1' ${install-script} > temp.txt && mv temp.txt ${install-script} \n"
                   "RUN chmod +x ${install-script} \n"
                   "RUN ${cmd-line-util} ${install-script} \n"
                   "RUN rm -r ${install-script} \n"
                   ))
        
        )
      "An alist contianing layers, grouped and labeled by purpose (ie basse, sql-req, req)
       for the Dockerfile")



(defun convention-query-layers (key-tuple)
  "Queries the convention-lang-info alist for an element specified
   by the KEY-TUPLE"
  (convention-util-recursive-assoc-cdr key-tuple convention-image-layers))


(defun convention-get-layer-req (image-or-container-name)
  "Returns a string representing the requirements layer for the docker
   build command for a given LANG"
  (if (convention-is-sql image-or-container-name)
      (convention-query-layers '("sql-req"))
    (convention-query-layers '("req"))))

(provide 'convention-layers)


