;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: bnf-parser; -*-

(in-package "BNF-PARSER")

;;; Execute the canonical ATN model directly.  The Lisp compiler remains the
;;; specification for edge order, register cardinality and reduction calls.

(defstruct (atn-interpreter-state
             (:constructor make-atn-interpreter-state (&optional registers))
             (:copier nil))
  registers)

(defstruct (atn-interpreter-result
             (:constructor make-atn-interpreter-result (value index)))
  value
  index)

(defvar *atn-interpreter-system*)
(defvar *atn-interpreter-input-function*)
(defvar *atn-interpreter-eof-function*)
(defvar *atn-interpreter-word-predicate*)
(defvar *atn-interpreter-mode*)
(defvar *atn-interpreter-farthest*)
(defvar *atn-interpreter-active-nets*)

(defun copy-atn-interpreter-state (state)
  (make-atn-interpreter-state
   (copy-tree (atn-interpreter-state-registers state))))

(defun atn-interpreter-register (state name)
  (cdr (assoc name (atn-interpreter-state-registers state) :test #'string=)))

(defun (setf atn-interpreter-register) (value state name)
  (let ((entry (assoc name (atn-interpreter-state-registers state)
                      :test #'string=)))
    (if entry
        (setf (cdr entry) value)
        (push (cons name value) (atn-interpreter-state-registers state))))
  value)

(defun atn-interpreter-cardinality (net name)
  (cdr (find name (atn-terms net) :key #'car
             :test (lambda (left right)
                     (or (equal left right)
                         (and (typep left '(or string symbol))
                              (typep right '(or string symbol))
                              (string= left right)))))))

(defun atn-interpreter-node (net designator)
  (etypecase designator
    (atn-node designator)
    (symbol
     (or (find designator (atn-nodes net) :key #'atn-name)
         (error "Node ~s is not defined in net ~s."
                designator (atn-name net))))))

(defun atn-interpreter-net (designator)
  (etypecase designator
    (atn designator)
    ((or symbol string)
     (or (get-atn *atn-interpreter-system* designator)
         (error "Net ~s is not defined in system ~s."
                designator (system-name *atn-interpreter-system*))))))

(defun atn-interpreter-item (index)
  (funcall *atn-interpreter-input-function* index))

(defun atn-interpreter-byte-value (item)
  (etypecase item
    (symbol (char-code (char (string item) 0)))
    (string (char-code (char item 0)))
    (character (char-code item))
    (integer item)))

(defun atn-interpreter-token-value (value)
  (cond ((characterp value) (char-code value))
        ((integerp value) value)
        ((and (stringp value) (> (length value) 2)
              (string-equal "#x" value :end2 2))
         (parse-integer value :start 2 :radix 16))
        ((and (stringp value) (> (length value) 1)
              (string-equal "x" value :end2 1))
         (parse-integer value :start 1 :radix 16))
        ((stringp value) (parse-integer value))
        (t (error "Invalid token value: ~s." value))))

(defun atn-interpreter-token-test (test item)
  (destructuring-bind (operator &rest operands) test
    (let* ((negated (and (consp (first operands))
                         (eq (first (first operands)) 'not)))
           (values (if negated (rest (first operands)) operands))
           (item-value (and item (atn-interpreter-byte-value item)))
           (matched
             (case operator
               (token-range
                (destructuring-bind (minimum maximum) values
                  (and item-value
                       (<= (atn-interpreter-token-value minimum)
                           item-value
                           (atn-interpreter-token-value maximum)))))
               (tokenset
                (and item-value
                     (member item-value values
                             :key #'atn-interpreter-token-value :test #'=)))
               (otherwise
                (error "Unknown compound ATN test: ~s." test)))))
      (if negated (not matched) matched))))

(defgeneric atn-interpreter-category-p (category item)
  (:method ((category atn-builtin-predicate-category) item)
    (and item
         (funcall
          (make-lisp-predicate-name category :if-does-not-exist :error)
          item)))
  (:method ((category atn-category) item)
    (and item
         (some (lambda (element)
                 (atn-interpreter-category-p element item))
               (category-elements category))))
  (:method ((lexem atn-lexem) item)
    (let ((name (atn-name lexem)))
      (cond ((characterp name)
             (and (characterp item) (char= item name)))
            ((stringp name)
             (funcall *atn-interpreter-word-predicate*
                      item (ensure-token name)))
            (t
             (funcall *atn-interpreter-word-predicate* item name)))))
  (:method ((range bnf-tokenrange) item)
    (let* ((tokens (bnf-tokens range))
           (value (and item (atn-interpreter-byte-value item)))
           (matched
             (and value
                  (<= (atn-interpreter-token-value (cdr (first tokens)))
                      value
                      (atn-interpreter-token-value (cdr (second tokens)))))))
      (if (bnf-negation range) (not matched) matched)))
  (:method ((set bnf-tokenset) item)
    (let* ((value (and item (atn-interpreter-byte-value item)))
           (matched
             (and value
                  (member value (bnf-tokens set) :key
                          (lambda (token)
                            (atn-interpreter-token-value (cdr token)))
                          :test #'=))))
      (if (bnf-negation set) (not matched) matched))))

(defun atn-interpreter-test-p (test item)
  (cond ((eq test t) t)
        ((consp test) (atn-interpreter-token-test test item))
        (t
         (let ((predicate
                 (make-lisp-predicate-name test :if-does-not-exist :error)))
           (funcall predicate item)))))

(defun atn-interpreter-eval (form state index)
  "Evaluate an ATN action or argument with the net registers dynamically bound."
  (let* ((registers (atn-interpreter-state-registers state))
         (symbols (mapcar #'car registers))
         (values (mapcar #'cdr registers))
         (index-symbol (intern "INDEX" *atn-source-package*))
         (item-symbol (intern "item" *atn-source-package*))
         result updated-values)
    (progv (append symbols (list index-symbol item-symbol))
           (append values (list index (atn-interpreter-item index)))
      (setf result (eval form)
            updated-values (mapcar #'symbol-value symbols)))
    (loop for entry in registers
          for value in updated-values
          do (setf (cdr entry) value))
    result))

(defun atn-interpreter-run-forms (forms state index)
  (dolist (form forms)
    (atn-interpreter-eval form state index))
  state)

(defun atn-interpreter-procedure-test-p (procedure state index)
  (let ((arguments (atn-proc-arguments procedure)))
    (typecase procedure
      (atn-range-test
       ;; This is the exact form emitted by MAKE-LISP-TEST-FORM.
       (min index (atn-interpreter-eval (first arguments) state index)))
      ((or atn-min-test atn-max-test atn-size-test)
       (funcall (atn-proc-name procedure) index
                (atn-interpreter-eval (first arguments) state index)))
      (otherwise
       (apply (atn-proc-name procedure)
              (mapcar (lambda (argument)
                        (atn-interpreter-eval argument state index))
                      arguments))))))

(defun atn-interpreter-pop-tests-p (net state index)
  (every (lambda (test)
           (atn-interpreter-procedure-test-p test state index))
         (atn-pop-tests net)))

(defun atn-interpreter-constructor-name (name)
  (intern (concatenate 'string (string name) "-Constructor")
          *atn-source-package*))

(defun atn-interpreter-specializer (edge state index)
  (let ((form (atn-constructor-specializer edge)))
    (when form (atn-interpreter-eval form state index))))

(defun atn-interpreter-reduce-item (edge name item state index)
  (let ((specializer (atn-interpreter-specializer edge state index))
        (constructor (atn-interpreter-constructor-name name)))
    (cond ((eq *atn-reduce* t)
           (if specializer
               (atn-reduce-item-with-context constructor specializer item)
               (atn-reduce-item constructor item)))
          ((eq *atn-reduce* 'cons) (list name item))
          (t name))))

(defun atn-interpreter-reduce-structure (edge net state index)
  (let* ((name (atn-register edge))
         (terms (atn-term-names net))
         (structure
           (mapcar (lambda (term)
                     (atn-interpreter-register state term))
                   terms))
         (specializer (atn-interpreter-specializer edge state index))
         (constructor (atn-interpreter-constructor-name name)))
    (cond ((eq *atn-reduce* t)
           (if (fboundp constructor)
               (if specializer
                   (apply #'atn-reduce-structure-with-context
                          constructor specializer structure)
                   (apply #'atn-reduce-structure constructor structure))
               (cons name (delete nil (copy-list structure)))))
          ((eq *atn-reduce* 'cons)
           (cons name (delete nil (copy-list structure))))
          (t name))))

(defun atn-interpreter-store (net state name value &key atomic)
  (case (atn-interpreter-cardinality net name)
    ((1 ?) (when (or atomic value)
             (setf (atn-interpreter-register state name) value)))
    ((+ *) (when (or atomic value)
             (push value (atn-interpreter-register state name)))))
  state)

(defun atn-interpreter-append-results (results new-results)
  (if (eq *atn-interpreter-mode* :single)
      (or results new-results)
      (nconc results new-results)))

(defun atn-interpreter-transition (net edge index state new-index)
  (atn-interpreter-interpret-node
   net (atn-interpreter-node net (atn-end edge)) new-index state))

(defun atn-interpreter-fail-transition (net edge index state)
  (let ((fail (and (typep edge 'atn-transition) (atn-fail edge))))
    (when fail
      (atn-interpreter-interpret-node
       net (atn-interpreter-node net fail) index state))))

(defun atn-interpreter-edge-failed (net edge index state)
  (atn-interpreter-run-forms (atn-fail-actions edge) state index)
  (atn-interpreter-fail-transition net edge index state))

(defgeneric atn-interpreter-interpret-edge (net edge index state)
  (:method (net (edge fail-atn-edge) index state)
    (atn-interpreter-run-forms (atn-actions edge) state index)
    (atn-interpreter-run-forms (atn-fail-actions net) state index)
    nil)

  (:method (net (edge jump-atn-edge) index state)
    (atn-interpreter-run-forms (atn-actions edge) state index)
    (atn-interpreter-transition net edge index state index))

  (:method (net (edge word-atn-edge) index state)
    (let* ((item (atn-interpreter-item index))
           (word (ensure-token (atn-word edge))))
      (if (etypecase word
            (character
             (and (characterp item)
                  (= (char-code item) (char-code word))))
            (symbol
             (funcall *atn-interpreter-word-predicate* item word)))
          (progn
            (setf *atn-term* word)
            (atn-interpreter-run-forms (atn-actions edge) state index)
            (when *atn-register-words
              (let ((register
                      (intern (format nil "Word-~a" word)
                              *atn-source-package*)))
                (atn-interpreter-store
                 net state register
                 (atn-interpreter-reduce-item edge register item state index)
                 :atomic t)))
            (atn-interpreter-run-forms
             (atn-succeed-actions edge) state index)
            (atn-interpreter-transition net edge index state (1+ index)))
          (progn
            (setf *atn-term?* word)
            (or (atn-interpreter-edge-failed net edge index state) nil)))))

  (:method (net (edge cat-atn-edge) index state)
    (let* ((item (atn-interpreter-item index))
           (category (atn-cat edge))
           (name (category-name category)))
      (if (atn-interpreter-category-p category item)
          (progn
            (setf *atn-term* name)
            (atn-interpreter-run-forms (atn-actions edge) state index)
            (atn-interpreter-store
             net state name
             (atn-interpreter-reduce-item edge name item state index)
             :atomic t)
            (atn-interpreter-run-forms
             (atn-succeed-actions edge) state index)
            (atn-interpreter-transition net edge index state (1+ index)))
          (progn
            (setf *atn-term?* name)
            (or (atn-interpreter-edge-failed net edge index state) nil)))))

  (:method (net (edge test-atn-edge) index state)
    (let ((item (atn-interpreter-item index))
          (name (atn-test edge)))
      (if (atn-interpreter-test-p name item)
          (progn
            (setf *atn-term* name)
            (atn-interpreter-run-forms (atn-actions edge) state index)
            (when (atn-interpreter-cardinality net name)
              (atn-interpreter-store
               net state name
               (atn-interpreter-reduce-item edge name item state index)
               :atomic t))
            (atn-interpreter-run-forms
             (atn-succeed-actions edge) state index)
            (atn-interpreter-transition net edge index state (1+ index)))
          (progn
            (setf *atn-term?* name)
            (or (atn-interpreter-edge-failed net edge index state) nil)))))

  (:method (net (edge push-atn-edge) index state)
    (let ((subnet (atn-interpreter-net (atn-net edge))))
      (let ((subresults (atn-interpreter-interpret-net subnet index))
            results)
        (dolist (subresult subresults results)
          (let ((next-state (copy-atn-interpreter-state state))
                (name (atn-name subnet)))
            (setf *atn-term* name)
            (atn-interpreter-run-forms (atn-actions edge) next-state index)
            (atn-interpreter-store
             net next-state name (atn-interpreter-result-value subresult))
            (atn-interpreter-run-forms
             (atn-succeed-actions edge) next-state
             (atn-interpreter-result-index subresult))
            (setf results
                  (atn-interpreter-append-results
                   results
                   (atn-interpreter-transition
                    net edge index next-state
                    (atn-interpreter-result-index subresult))))
            (when (and results (eq *atn-interpreter-mode* :single))
              (return results))))
        (if subresults
            results
            (progn
              (setf *atn-term?* (atn-name subnet))
              (atn-interpreter-edge-failed net edge index state))))))

  (:method (net (edge pop-atn-edge) index state)
    (when (atn-interpreter-pop-tests-p net state index)
      (setf *atn-term* (atn-register edge))
      (atn-interpreter-run-forms (atn-actions edge) state index)
      (let ((value
              (atn-interpreter-reduce-structure edge net state index)))
        (atn-interpreter-run-forms
         (atn-succeed-actions edge) state index)
        (atn-interpreter-run-forms
         (atn-succeed-actions net) state index)
        (list (make-atn-interpreter-result value index)))))

  (:method (net (edge or-atn-edge) index state)
    (let (results)
      (dolist (alternative (atn-edges edge))
        (setf results
              (atn-interpreter-append-results
               results
               (atn-interpreter-interpret-edge
                net alternative index (copy-atn-interpreter-state state))))
        (when (and results (eq *atn-interpreter-mode* :single))
          (return)))
      (or results
          (let ((fail (atn-fail edge)))
            (atn-interpreter-run-forms
             (atn-fail-actions edge) state index)
            (when fail
              (atn-interpreter-interpret-node
               net (atn-interpreter-node net fail) index state)))))))

(defun atn-interpreter-interpret-node (net node index state)
  (setf *atn-interpreter-farthest* (max index *atn-interpreter-farthest*)
        *atn-node* (atn-name node))
  (atn-interpreter-run-forms (atn-initial-actions node) state index)
  (let (results)
    (dolist (edge (atn-edges node) results)
      (let ((edge-state (copy-atn-interpreter-state state)))
        (atn-interpreter-run-forms (atn-initial-actions edge) edge-state index)
        (setf results
              (atn-interpreter-append-results
               results
               (atn-interpreter-interpret-edge
                net edge index edge-state))))
      (when (and results (eq *atn-interpreter-mode* :single))
        (return results)))))

(defun atn-interpreter-interpret-net (net index)
  (let ((active (assoc (atn-name net) *atn-interpreter-active-nets*
                       :test #'string=)))
    (if (or (eq (atn-recursion net) :allow)
            (null active)
            (< (cdr active) index))
        (let* ((*atn-level (1+ *atn-level))
               (*atn-stack (cons (atn-name net) *atn-stack))
               (*atn-interpreter-active-nets*
                 (acons (atn-name net) index *atn-interpreter-active-nets*))
               (state
                 (make-atn-interpreter-state
                  (mapcar (lambda (term) (cons term nil))
                          (atn-term-names net)))))
          (atn-interpreter-run-forms (atn-initial-actions net) state index)
          (atn-interpreter-interpret-node
           net (atn-interpreter-node net (atn-start net)) index state))
        (progn
          (when (eq (atn-recursion net) :report)
            (warn "Recursive grammar: ~s at position ~s: ~s"
                  (atn-name net) index *atn-stack*))
          nil))))

(defun interpret-atn-system
    (system input
     &key
       ((:trace *atn-trace*) *atn-trace*)
       ((:trace-nets *atn-trace-nets*) *atn-trace-nets*)
       ((:start-name *atn-start-name) (system-main-net-name system))
       ((:mode *atn-interpreter-mode*) :multiple)
       ((:reduce *atn-reduce*) t)
       ((:register-words *atn-register-words) nil)
       ((:input-function *atn-interpreter-input-function*) '|input.item|)
       ((:input-eof-function *atn-interpreter-eof-function*) '|input.is-at-end|)
       ((:word-predicate *atn-interpreter-word-predicate*) 'eq)
       ((:source-package *atn-source-package*) *package*)
       ((:token-package *atn-token-package*) *package*)
     &allow-other-keys)
  "Interpret SYSTEM over INPUT and return RESULT, POSITION and COMPLETE-P.

INPUT may be an ATN-INPUT or any Common Lisp sequence.  Reduction constructors
receive the same positional arguments, and constructor specializers receive the
same leading context argument, as parsers emitted by COMPILE-ATN-SYSTEM."
  (check-type system atn-system)
  (check-type *atn-interpreter-mode* (member :single :multiple))
  (setf *atn-source-package*
        (or (find-package *atn-source-package*)
            (error "Invalid source package: ~s." *atn-source-package*))
        *atn-token-package*
        (or (find-package *atn-token-package*)
            (error "Invalid token package: ~s." *atn-token-package*)))
  (intern-atn-system system)
  (let* ((*atn-interpreter-system* system)
         (*atn-input
           (if (typep input 'atn-input)
               input
               (make-instance 'atn-sequence-input :sequence input)))
         (*atn-level 0)
         (*atn-stack (list 'interpret-atn-system))
         (*atn-node* nil)
         (*atn-properties* nil)
         (*atn-class* nil)
         (*atn-interpreter-farthest* 0)
         (*atn-interpreter-active-nets* nil)
         (start (atn-interpreter-net *atn-start-name))
         (results nil))
    (declare (special *atn-input *atn-level *atn-stack *atn-node*
                      *atn-properties* *atn-class*))
    (setf results (atn-interpreter-interpret-net start 0))
    (when (and results (eq *atn-interpreter-mode* :multiple))
      (setf results
            (stable-sort results #'>
                         :key #'atn-interpreter-result-index)))
    (if results
        (let ((result (first results)))
          (values (atn-interpreter-result-value result)
                  (atn-interpreter-result-index result)
                  (funcall *atn-interpreter-eof-function*
                           (atn-interpreter-result-index result))))
        (values nil *atn-interpreter-farthest* nil))))

(defun make-atn-interpreter (system &rest default-arguments)
  "Return a parser function for SYSTEM with INTERPRET-ATN-SYSTEM's interface."
  (lambda (input &rest arguments)
    (apply #'interpret-atn-system system input
           (append arguments default-arguments))))

