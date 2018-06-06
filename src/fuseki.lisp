;;;; this source code get from madnificent/cl-fuseki
(in-package :cl-user)
(defpackage cl-ignition.fuseki
  (:use :cl :drakma :cl-ppcre :jsown)
  (:export :query))
(in-package :cl-ignition.fuseki)

(defun s+ (&rest strings)
  "Concatenates a set of strings"
  (apply #'concatenate 'string "" strings))

(defvar *standard-prefixes* nil
  "contains all the standard prefixes, as prefix objects")
(defstruct prefix
  (prefix)
  (iri))
(defun is-standard-prefix-p (prefix)
  "Checks whether or not the prefixed string is contained in the current list of standard prefixes.
   Returns non-nil if the prefix string is a known standard prefix."
  (find prefix *standard-prefixes* :key #'prefix-prefix :test #'string=))
(defun add-prefix (prefix iri)
  "Adds a prefix to the set of standard prefixes.  The prefix is the short version, the IRI is the long version.
   eg: (add-prefix \"rdf\" \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\")"
  (when (is-standard-prefix-p prefix)
    (rm-prefix prefix))
  (push (make-prefix :prefix prefix :iri iri)
        *standard-prefixes*))

(defun rm-prefix (prefix)
  "Removes a prefix from the set of standard prefixes.  The prefix is the short version.
   eg: (rm-prefix \"rdf\")"
  (when (is-standard-prefix-p prefix)
    (setf *standard-prefixes*
          (remove-if (lambda (prefix-prefix) (string= prefix prefix-prefix))
                     *standard-prefixes* :key #'prefix-prefix))))

(defun get-prefix-alist ()
  "Returns an alist of prefixes."
  (loop for prefix in *standard-prefixes*
     collect (cons (prefix-prefix prefix)
                   (prefix-iri prefix))))

(defun get-prefix (prefix)
  "Returns the value associated to the supplied prefix."
  (let ((cell (assoc prefix (get-prefix-alist) :test #'string=)))
    (when (consp cell)
      (cdr cell))))

(defun query-update-prefixes (query &key (prefix T prefix-p) &allow-other-keys)
  "Updates the query unless the :prefix keyword has been set to nil."
  (if (or prefix (not prefix-p))
      (s+ (format nil "~{~&PREFIX ~A: <~A>~%~}"
                  (loop for p in *standard-prefixes*
                     append (list (prefix-prefix p) (prefix-iri p))))
          query)
      query))

; add standard prefixes
(add-prefix "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(add-prefix "owl" "http://www.w3.org/2002/07/owl#")

(defparameter *do-postponed-updates* T)

(defun maybe-postpone-update (repository update-string &rest options &key
                              (deposit nil depositp)
                              (revoke nil revokep) &allow-other-keys)
  "performs the update in a postponed fashion if deposit contains a key named deposit.  the update will be executed if a flush-updates function is called, or if a query is executed.  if another query with a revoke of a yet-to-be-executed update with a deposit-key that equals to that key is sent, then neither the query with the equaled deposit key as the query with the equaled revoke key will be executed."
  (if (or (not *do-postponed-updates*)
          (not (or depositp revokep)))
      (update-now repository
                  (apply #'query-update-prefixes
                         update-string
                         options))
      (if depositp
          (setf (gethash deposit
                         (slot-value repository
                                     'postponed-updates))
                update-string)
          (unless (remhash revoke
                           (slot-value repository
                                       'postponed-updates))
            (push update-string
                  (slot-value repository
                              'unnamed-postponed-updates))))))

(defun flush-updates (repository)
  "performs all postponed updates which still need to be executed"
  (let* ((hash (slot-value repository 'postponed-updates))
         (update-list (slot-value repository 'unnamed-postponed-updates))
         (keys (loop for key being the hash-keys of hash
                  collect key)))
    (when (or update-list keys)
      (update-now repository
                  (query-update-prefixes 
                   (format nil "~{~A~^; ~%~} ~[~;;~] ~{~A~^; ~%~}"
                           update-list
                           (or update-list keys)
                           (loop for key in keys collect (gethash key hash)))))
      (setf (slot-value repository 'unnamed-postponed-updates) nil)
      (dolist (key keys)
        (remhash key hash)))))

;; drakma setup
(push (cons nil "x-turtle") drakma:*text-content-types*)
(push (cons nil "sparql-results+json") drakma:*text-content-types*)

(setf drakma:*drakma-default-external-format* :UTF-8)

(defun parse-ntriples-string (string)
  "converts an ntriples string into a list of triples (in which each triple is a list of three strings)"
  (mapcar (lambda (triple)
            (cl-ppcre:split "\\s+" triple))
          (cl-ppcre:split "\\s+\\.\\s+" string)))

;; data types
(defparameter *data-type-bindings* (make-hash-table :test 'eq))

(defun get-data-type-binding (symbol)
  (gethash symbol *data-type-bindings*))

(defun (setf get-data-type-binding) (value symbol)
  (setf (gethash symbol *data-type-bindings*) value))

(mapcar (lambda (k-v)
          (setf (get-data-type-binding (first k-v))
                (second k-v)))
        '((:XML "application/sparql-results+xml")
          (:JSON "application/sparql-results+json")
          (:binary "application/x-binary-rdf-results-table")
          (:RDFXML "application/rdf+xml")
          (:NTriples "text/plain")
          (:Turtle "application/x-turtle")
          (:N3 "text/rdf+n3")
          (:TriX "application/trix")
          (:TriG "application/x-trig")
          (:PlainTextBoolean "text/boolean")))

;; errors
(define-condition sesame-exception (error)
  ((status-code :reader status-code
                :initarg :status-code)
   (response :reader response
             :initarg :response)))

(defmacro remove-key (variable &rest keys)
  (let ((g-keys (gensym "keys")))
    `(let ((,g-keys (list ,@keys)))
       (setf ,variable (loop for (k v) on ,variable by #'cddr
                          unless (find k ,g-keys)
                          append (list k v))))))

(defun send-request (url &rest html-args &key (wanted-status-codes '(200)) &allow-other-keys)
  (remove-key html-args :wanted-status-codes)
  (multiple-value-bind (response status-code)
      (apply #'http-request url html-args)
    (unless (and wanted-status-codes
                 (find status-code wanted-status-codes))
      (error 'sesame-exception
             :status-code status-code
             :response response))
    response))

(defclass server ()
  ((base-url :accessor base-url
             :initarg :base-url
             :initform (error "base url must be supplied")))
  (:documentation "basic semantic web database server"))

(defclass fuseki-server (server)
  ()
  (:documentation "fuseki semantic web database server"))

(defclass virtuoso-server (server)
  ()
  (:documentation "Virtuoso sparql endpoint"))

(defclass repository ()
  ((name :accessor name
         :initarg :name
         :initform "")
   (server :accessor server
           :initarg :server
           :initform (error "server must be supplied"))
   (postponed-updates :initform (make-hash-table :test 'equal))
   (unnamed-postponed-updates :initform nil))
  (:documentation "generic semantic web database repository"))

(defclass fuseki-repository (repository)
  ()
  (:documentation "fuseki sementic web database repository"))

(defclass virtuoso-repository (repository)
  ()
  (:documentation "virtuoso sparql endpoint repository"))

(defgeneric query-endpoint (repository)
  (:documentation "SPARQL query endpoint"))
(defgeneric update-endpoint (repository)
  (:documentation "SPARQL Update language endpoint"))
(defgeneric data-endpoint (repository)
  (:documentation "SPARQL HTTP Update endpoint"))
(defgeneric upload-endpoint (repository)
  (:documentation "file upload endpoint"))

(defgeneric server-query-endpoint-postfix (server)
  (:documentation "postfix for the SPARQL query endpoint of the server"))
(defgeneric server-update-endpoint-postfix (server)
  (:documentation "postfix for the SPARQL Update language endpoint of the server"))
(defgeneric server-data-endpoint-postfix (server)
  (:documentation "postfix for the SPARQL HTTP Update endpoint of the server"))
(defgeneric server-upload-endpoint-postfix (server)
  (:documentation "postfix for the file upload endpoint of the server"))

(defmethod server-query-endpoint-postfix ((server fuseki-server))
  "/query")
(defmethod server-update-endpoint-postfix ((server fuseki-server))
  "/update")
(defmethod server-data-endpoint-postfix ((server fuseki-server))
  "/data")
(defmethod server-upload-endpoint-postfix ((server fuseki-server))
  "/upload")

(defmethod server-query-endpoint-postfix ((server virtuoso-server))
  "/sparql")
(defmethod server-update-endpoint-postfix ((server virtuoso-server))
  "/sparql")
(defmethod server-data-endpoint-postfix ((server virtuoso-server))
  "/sparql")
(defmethod server-upload-endpoint-postfix ((server virtuoso-server))
  "/sparql")

(defmethod query-endpoint ((repos fuseki-repository))
  (let ((server (server repos)))
    (s+ (base-url server) (name repos) (server-query-endpoint-postfix server))))
(defmethod update-endpoint ((repos fuseki-repository))
  (let ((server (server repos)))
    (s+ (base-url server) (name repos) (server-update-endpoint-postfix server))))
(defmethod data-endpoint ((repos fuseki-repository))
  (let ((server (server repos)))
    (s+ (base-url server) (name repos) (server-data-endpoint-postfix server))))
(defmethod upload-endpoint ((repos fuseki-repository))
  (let ((server (server repos)))
    (s+ (base-url server) (name repos) (server-upload-endpoint-postfix server))))

(defmethod query-endpoint ((repos repository))
  (let ((server (server repos)))
    (s+ (base-url server) (server-query-endpoint-postfix server))))
(defmethod update-endpoint ((repos repository))
  (let ((server (server repos)))
    (s+ (base-url server) (server-update-endpoint-postfix server))))
(defmethod data-endpoint ((repos repository))
  (let ((server (server repos)))
    (s+ (base-url server) (server-data-endpoint-postfix server))))
(defmethod upload-endpoint ((repos repository))
  (let ((server (server repos)))
    (s+ (base-url server) (server-upload-endpoint-postfix server))))

(defparameter *query-log-stream* nil
  "non-nil indicates that queries should be logged to the
   supplied stream.")

(defun maybe-log-query (query)
  "Performs query-logging if *query-log-stream* is truethy."
  (when *query-log-stream*
    (format *query-log-stream*
            "~&==Executing query==~%~A~%~%"
            query))
  query)

(defmacro with-query-logging (stream &body body)
  "Executes the following code block with query-logging enabled."
  `(let ((*query-log-stream* ,stream))
     ,@body))

(defmacro without-query-logging (&body body)
  "Executes the following code-block with query logging disabled."
  `(let ((*query-log-stream* nil))
     ,@body))

(defgeneric query-raw (repository query &key &allow-other-keys)
  (:documentation "sends a raw sparql query to the repository.  this is meant to connect to the SPARQL query endpoint.  this version doesn't parse the result.
  see query for a version which returns a jsown parsed object of results"))

(defgeneric query (repository query &key &allow-other-keys)
  (:documentation "sends a sparql query to the repository and returns a jsown-parsed object of results.  calls query-raw for the raw processing."))

(defmethod query-raw ((repos repository) (query string) &rest options &key &allow-other-keys)
  (flush-updates repos)
  (let ((full-query (apply #'query-update-prefixes query options)))
    (maybe-log-query full-query)
    (send-request (query-endpoint repos)
                  :accept (get-data-type-binding :json)
                  :parameters `(("query" . ,full-query)))))

(defmethod query-raw ((repos virtuoso-repository) (query string) &rest options &key &allow-other-keys)
  (flush-updates repos)
  (let ((full-query (apply #'query-update-prefixes query options)))
    (maybe-log-query full-query)
    (send-request (query-endpoint repos)
                  :method :post
                  :accept (get-data-type-binding :json)
                  :parameters `(("query" . ,full-query)))))

(defmethod query ((repos repository) (query string) &rest options &key &allow-other-keys)
  (filter (parse (apply #'query-raw repos query options))
          "results" "bindings"))

(defmacro long-query (repository (&rest options &key &allow-other-keys) &body query-forms) 
  "Provides a nicer visual for for executing a query which contains multiple lines."
  `(query ,repository
          (s+ ,@(loop for query in query-forms
                   append (list query " ")))
          ,@options))
(defgeneric update (repository query &key &allow-other-keys)
  (:documentation "sends a sparql update to the repository."))
(defgeneric update-now (repository query)
  (:documentation "sends a sparql update query to the repository without waiting for anything"))

(defmethod update-now ((repos repository) (update string))
  (maybe-log-query update)
  (send-request (update-endpoint repos)
                :wanted-status-codes '(200 204) ; only 204 is in the spec
                :content-type "application/sparql-update" ; fuseki-specific
                :method :post
                :content update))

(defmethod update ((repos repository) (update string) &rest options &key &allow-other-keys)
  (apply #'maybe-postpone-update 
         repos
         update
         options))

(defmacro long-update (repository (&rest options &key &allow-other-keys) &body query-forms) 
  "Provides a nicer visual for for executing an update query which contains multiple lines."
  `(query ,repository
          (s+ ,@(loop for query in query-forms
                   append (list query " ")))
          ,@options))
(defgeneric ask (repository query &key &allow-other-keys)
  (:documentation "sends a sparql ask query to the repository and returns T if the answer was positive or NIL if the ansewer was negative.  calls query-raw for the raw processing."))

(defmethod ask ((repos repository) (query string) &rest options &key &allow-other-keys)
  (val (parse 
        (apply #'query-raw repos query options))
       "boolean"))
(defmacro insert (repository (&rest options)
                  &body format)
  `(update ,repository
           (format nil "~&INSERT DATA~&{~A~&}"
                   (format nil ,@format))
           ,@options))

(defmacro fuseki-delete (repository (&rest options)
                  &body format)
  `(update ,repository
           (s+ "DELETE DATA { "
               (format nil ,@format)
               " }")
           ,@options))
