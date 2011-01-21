;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: bnf-parser; -*-

;;; This file is part of the atn-parser system. It graphs atn models.

(in-package :atn-parser)

;;
;;
;; GRAPH-ATN-SYSTEM handles the various input forms and prints the system's net definitions to a stream.

(defgeneric graph-atn-system (system &key)
  (:documentation
    "translate an atn system to its net definitions.")
  
  (:method ((grammar-system atn-system)
            &key
            ((:source-package *atn-source-package*) *package*)
            (dot-pathname (error "dot-pathname is required.")))
    "print an atn system."
    (declare (special *atn-source-package*))
    (setq *atn-source-package* (or (find-package *atn-source-package*)
                                 "source package is invalid: ~s." *atn-source-package*))
    (dot:context-put-graph dot-pathname grammar-system grammar-system)
    grammar-system)
  
  (:method  ((*grammar string) &rest keys
             &key ((:register-words *atn-register-words) nil)
             ((:token-package *atn-token-package*) *package*)
             ((:source-package *atn-source-package*) *package*)
             &allow-other-keys)
    "translate a bnf grammar to an atn system and from there write it to a stream."
    (declare (special *grammar))
    (setq *atn-source-package* (or (find-package *atn-source-package*)
                                   "source package is invalid: ~s." *atn-source-package*))
    (setq *atn-token-package* (or (find-package *atn-token-package*)
                                  "token package is invalid: ~s." *atn-token-package*))
    (let ((atn-system (bnf-to-atn *grammar)))
      (cond (atn-system
             (apply #'graph-atn-system atn-system keys))
            (t
             (warn "grammar not parsed: ~s."
                   (subseq *grammar 0 (min (length *grammar) 128)))))))
  
  (:method ((stream stream) &rest keys &key &allow-other-keys)
    "read a bnf grammar from a stream into a buffer and continue processing on that."
    (flet ((stream->string (stream &aux (buffer (make-array 0 :element-type 'character
                                                            :adjustable t
                                                            :fill-pointer 0))
                                   character)
             (loop (unless (setf character (read-char stream nil nil))
                     (return buffer))
                   (vector-push-extend character buffer))))
      (apply #'graph-atn-system (stream->string stream) keys)))
  
  (:method ((*grammar-pathname pathname) &rest keys
            &key (dot-pathname (make-pathname :type "dot" :defaults *grammar-pathname))
            &allow-other-keys)
    "read a bnf grammar from a file and compile it to a parser."
    (declare (special *grammar-pathname))
    (with-open-file (stream *grammar-pathname :direction :input)
      (apply #'graph-atn-system stream
             :dot-pathname dot-pathname
             keys))))


(defgeneric dot-edge-label (edge)
  (:method ((edge atn-edge))
    (with-output-to-string (stream) (write edge :stream stream :pretty t)))
  (:method ((edge cat-atn-edge))
    (format nil "(cat ~A)" (category-name (atn-cat edge))))
  (:method ((object fail-atn-edge))
    "fail")
  (:method ((object jump-atn-edge))
    "jump")
  (:method ((object pop-atn-edge))
    "pop")
  (:method ((object push-atn-edge))
    (format nil "(push ~a)" (atn-start (atn-net-atn object))))
  (:method ((edge test-atn-edge))
    (format nil "(test ~A)" (atn-test edge)))
  (:method ((edge word-atn-edge))
    (format nil "(word |~A|)" (atn-word edge))))


(defmethod dot:context-put-graph ((context setf.dot:stream) (name atn-system) (graph atn-system) &rest args)
  (apply #'dot:context-put-graph context (system-name name) graph args))

(defmethod dot:context-put-graph ((context setf.dot:stream) (name t) (graph atn-system) &rest attributes)
  "Encode an atn system as a .dot graph.
 Encode each ATN as its own subgraph with internal links for end and fail transitions and
 inter-net edges for push nodes."
  
  (flet ((put-atn-graph () 
           (context-put-atn-system-graph context graph)))
    (apply #'dot:context-put-graph context name #'put-atn-graph
           :fontname "courier"
           :edge '(:fontname "courier")
           :node '(:fontname "courier")
           attributes)))


(defmethod dot:context-put-node ((context setf.dot:stream) (object atn) &rest attributes)
  (apply #'dot:context-put-node context (atn-name object) attributes))

(defmethod dot:context-put-node ((context setf.dot:stream) (object atn-node) &rest attributes)
  (apply #'dot:context-put-node context (atn-name object) attributes))

(defmethod dot:context-put-edge ((context setf.dot:stream) (from atn) (to t) &rest attributes)
  (apply #'dot:context-put-edge context (atn-name from) to attributes))

(defmethod dot:context-put-edge ((context setf.dot:stream) (from atn-node) (to t) &rest attributes)
  (apply #'dot:context-put-edge context (atn-name from) to attributes))


(defgeneric context-put-atn-system-graph (context atn-system)
  (:method ((context t) (system atn-system))
    (dot:context-put-node dot:*context* system :label "START")
    (dolist (atn (system-nets system))
      (flet ((put-net-subgraph-nodes ()
               (context-put-atn-nodes context atn)))
        (dot:context-put-graph context (setf (dot:context-id context atn) (gensym "cluster")) #'put-net-subgraph-nodes
                               :strict nil
                               :statement-type 'setf.dot:subgraph)))
    (dolist (atn (system-nets system))
      (context-put-atn-edges context atn))
    ;; these after, in order to use the proper atn id,
    (dot:context-put-edge context system (atn-start (system-main-net system)))))


(defgeneric context-put-atn-nodes (context atn)
  (:method ((context t) (object atn))
    (dot:context-put-node context object)
    (dolist (node (atn-nodes object))
      (dot:context-put-node context node))))

(defgeneric context-put-atn-edges (context atn)
  (:method ((context t) (object atn))
    (dot:context-put-edge context object (atn-start-node object) :label "start")
    (dolist (node (atn-nodes object))
      (context-put-atn-edges context node)))

  (:method ((context t) (object atn-node))
    (dolist (edge (atn-edges object))
      (dot:context-put-edge context object edge))))

(defmethod dot:context-put-edge ((context setf.dot:stream) (node1 t) (node2 atn) &rest args)
  (apply #'dot:context-put-edge context node1 `(dot:subgraph . ,(dot:context-id context node2)) args))

(defmethod dot:context-put-edge ((context setf.dot:stream) (start atn-node) (edge atn-transition) &rest attributes &key (label (dot-edge-label edge)) &allow-other-keys)
  (let ((fail (atn-fail edge))
        (end (atn-end edge)))
    (apply #'dot:context-put-edge context (atn-name start) end :label label attributes)
    (when fail
      (apply #'dot:context-put-edge context (atn-name start) fail :label "fail" attributes))))

(defmethod dot:context-put-edge ((context setf.dot:stream) (start atn-node) (edge push-atn-edge) &rest attributes &key (label (dot-edge-label edge)) &allow-other-keys)
  (let* ((net (atn-net-atn edge))
         (net-start (atn-start-node net))
         (end (atn-end edge))
         (fail (atn-fail edge)))
    (apply #'dot:context-put-edge context start net-start :label label attributes)
    (apply #'dot:context-put-edge context (atn-name start) end :label label attributes)
    (when fail
      (apply #'dot:context-put-edge context (atn-name start) fail :label "fail" attributes))))

(defmethod dot:context-put-edge ((context setf.dot:stream) (start atn-node) (edge pop-atn-edge) &rest attributes)
  (let* ((net (atn-net start)))
    (apply #'dot:context-put-edge context start (format nil "~a.pop" (atn-name net))
           :label "pop"
           attributes)))

(defmethod dot:context-put-edge ((context setf.dot:stream) (start atn-node) (edge or-atn-edge) &rest attributes)
  ;; encode just the fail - the individual dependents are to be encoded on their own
  (let ((fail (atn-fail edge)))
    (when fail
      (apply #'dot:context-put-edge context (atn-name start) fail :label "fail" attributes))))

(defmethod dot:context-put-edge ((context setf.dot:stream) (start atn-node) (edge atn-edge) &rest attributes)
  (declare (ignore attributes))
  ;; by default do nothing for a non-specific edge
  )
