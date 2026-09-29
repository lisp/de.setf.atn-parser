;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: bnf-parser; -*-

(in-package "BNF-PARSER")

(defun run-atn-interpreter-tests ()
  (let* ((*atn-source-package* *package*)
         (*atn-token-package* *package*)
         (system
           (bnf-to-atn
            "sentence ::= noun verb
             noun ::= 'cats'
             verb ::= 'sleep'"))
         (noun (intern "noun" *package*))
         (verb (intern "verb" *package*))
         (sentence (intern "sentence" *package*))
         (cats (intern "cats" *package*))
         (sleep (intern "sleep" *package*))
         (noun-constructor
           (intern "noun-Constructor" *package*))
         (verb-constructor
           (intern "verb-Constructor" *package*))
         (sentence-constructor
           (intern "sentence-Constructor" *package*))
         (old-functions
           (mapcar (lambda (name)
                     (and (fboundp name) (symbol-function name)))
                   (list noun-constructor verb-constructor
                         sentence-constructor))))
    (unwind-protect
         (progn
           (multiple-value-bind (value index complete-p)
               (interpret-atn-system system (vector cats sleep)
                                     :reduce 'cons)
             (assert complete-p)
             (assert (= index 2))
             (assert
              (equal value
                     (list sentence
                           (list noun cats)
                           (list verb sleep)))))
           (setf (symbol-function noun-constructor)
                 (lambda (token) (list :noun token))
                 (symbol-function verb-constructor)
                 (lambda (token) (list :verb token))
                 (symbol-function sentence-constructor)
                 (lambda (noun-value verb-value)
                   (list :sentence noun-value verb-value)))
           (multiple-value-bind (value index complete-p)
               (interpret-atn-system system (vector cats sleep)
                                     :reduce t)
             (assert complete-p)
             (assert (= index 2))
             (assert
              (equal value
                     '(:sentence (:noun |cats|) (:verb |sleep|)))))
           (multiple-value-bind (value index complete-p)
               (interpret-atn-system system (vector cats cats)
                                     :reduce 'cons)
             (assert (null value))
             (assert (= index 1))
             (assert (null complete-p)))
           t)
      (loop for name in (list noun-constructor verb-constructor
                              sentence-constructor)
            for old-function in old-functions
            do (if old-function
                   (setf (symbol-function name) old-function)
                   (fmakunbound name))))))

