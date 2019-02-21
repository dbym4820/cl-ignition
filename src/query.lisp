(in-package :cl-user)
(defpackage cl-ignition.query
  (:use :cl)
  (:import-from :alexandria
		:destructuring-case)
  (:import-from :optima
		:match
		:property)
  (:export :with-prefix
	   :ensure-query
	   :select :vars :distinct :?s :?p :?o :limit
	   :val :lang :data-type
           :extract-key-value :result-filter :get-single-key))
(in-package :cl-ignition.query)


(defun prefix-expand (url-prefix resource)
  (format nil "<~A~A>" url-prefix resource))

(defclass sparql-query ()
  ((spaql-command :initarg :command-name :reader command-name)))

(defclass select (sparql-query)
  ((spaql-command :initform 'select)
   (return-variables :initarg :vars :accessor vars)
   (distinct :initform nil :initarg :distinct :accessor distinct)
   (subject-variable :initarg :subject :accessor ?s)
   (predicate-variable :initarg :predicate :accessor ?p)
   (object-variable :initarg :object :accessor ?o)
   (limit :initform nil :initarg :limit :accessor limit)))

(defgeneric ensure-query (sparql-query))
(defmethod ensure-query ((select select))
  (labels ((query-symbol-p (var)
	     (eq (char (format nil "~A" var) 0) #\?))
	   (var-to-string-or-nil (v)
	     (if v (format nil "~A" v) nil))
	   (variable-format-condition (var)
	     (if (query-symbol-p var)
		 (format nil "~A " var)
		 "")))
    (let* ((dist (distinct select))
	   (var-list (vars select))
	   (sub-var (var-to-string-or-nil (first var-list)))
	   (pred-var (var-to-string-or-nil (second var-list)))
	   (obj-var (var-to-string-or-nil (third var-list)))
	   (sub (?s select))
	   (pre (?p select))
	   (obj (?o select))
	   (lim (limit select)))
      (concatenate 'string
		   "select "
		   (when dist "distinct ")
		   (if (and (not (query-symbol-p sub-var))
			    (not (query-symbol-p pred-var))
			    (not (query-symbol-p obj-var)))
		       "* "
		       (format nil "~A~A~A"
			       (variable-format-condition sub-var)
			       (variable-format-condition pred-var)
			       (variable-format-condition obj-var)))
		   "where { "
		   (cond (sub
			  (format nil "~A " sub))
			 (sub-var
			  (format nil "~A " sub-var))
			 (t "?S"))
		   (cond (pre
			  (format nil "~A " pre))
			 (pred-var
			  (format nil "~A " pred-var))
			 (t "?P "))
		   (cond (obj
			  (format nil "~A " obj))
			 (obj-var
			  (format nil "~A " obj-var))
			 (t "?O "))
		   "} "
		   (when lim (format nil "limit ~A " lim))))))

(defmacro get-proper (proper query)
  "extract property value from query list"
  `(match ,query
     ((property ,proper x)
      (cond ((query-variable-list-p x)
	     x)
	    ((and (not (null x)) (listp x))
	     x)
	    (t
	     x)))))

(defun query-variable-list-p (var)
  (when (and (listp var)
	     (not (null var))
	     (not (string= "" (second var))))
    (let ((?-count (count-if #'(lambda (has-?-or-not)
				 (eq has-?-or-not t))
			     (mapcar #'(lambda (d)
					 (eq (char (format nil "~A" d) 0) #\?))
				     var))))
      (= ?-count 3))))

(defmacro with-prefix ((return-variable) prefix-list query-list &body body)
  (let* ((pref-target (gensym))
	 (ret-val (gensym)))
    (setf ret-val
	  `(labels (,@(loop for pref in prefix-list
			    collect `(,(first pref) (,pref-target)
				      (prefix-expand ,(second pref) ,pref-target))))
	     (list
	      ,@(loop for q-lst in query-list
		      when (eq :select (car q-lst))
			collect `(make-instance 'select
				      :vars ',(get-proper :select q-lst)
				      :distinct ,(get-proper :distinct q-lst)
				      :subject ,(get-proper :subject q-lst)
				      :predicate ,(get-proper :predicate q-lst)
				      :object ,(get-proper :object q-lst)
				      :limit ,(get-proper :limit q-lst))))))
    `(let ((,return-variable ,ret-val))
	   ,@body)))

(defun convert-query (q-lst)
  (format nil "~{~A~^ ~}" (mapcar #'ensure-query q-lst)))

#|
(let ((query
	(ignition:with-prefix (v) ((dbp "http://dbpedia.ja/resource/"))
			     ((:select (nil ?p nil)
			       :subject (dbp "冴えない彼女の育てかた")))
	  (cl-ignition.query::convert-query v))))
  (cl-ignition.fuseki:query query))
|#

(defclass result-data ()
  ((data-type :initarg :data-type :accessor data-type)
   (lang :initarg :lang :accessor lang)
   (val :initarg :val :accessor val)))

(defun make-result-data (type lang val)
  (make-instance 'result-data :data-type type :lang lang :val val))

(defun extract-key-value (data)
  (mapcar #'(lambda (d)
	      (mapcar #'(lambda (d1)
			  (let ((single-data (cddr d1)))
			    (cons (first d1)
				  (make-result-data
				   (cdr (assoc "type" single-data :test #'string=))
				   (or (cdr (assoc "xml:lang" single-data :test #'string=)) "en")
				   (cdr (assoc "value" single-data :test #'string=))))))
			  (cdr d)))
	  data))

(defun result-filter (retrieve-property-list data-list) ;; ("O")
  (loop for x in data-list
	collect (mapcar #'(lambda (d)
			    (assoc d x :test #'string=))
			retrieve-property-list)))

(defun get-single-key (property data)
  (loop for y in (result-filter `(,property) (extract-key-value data))
	collect (mapcar #'(lambda (d)
			    (val (cdr d)))
			 y)))
;;(cl-ignition.query::extract-key-vale (saekano-2))

;; (defun a (retrieve-keyword)
;;   (with-prefix (val)
;; 	       ((dbpedia-jp "http://ja.dbpedia.org/resource/")
;; 		(rdf-schema "http://www.w3.org/2000/01/rdf-schema"))
;; 	       ((:select (nil ?p ?o)
;; 		 :distinct t
;; 		 :subject (dbpedia-jp retrieve-keyword)
;; 		 :predicate (rdf-schema "#comment")))
;;     (get-single-key "O"
;; 		    (request-dbpedia
;; 		     (convert-query val)))))

