;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; 20101119 jaa extraced atn parser to independent system

(in-package :common-lisp-user)

(unless (find-class 'atn-file nil)
  (defclass atn-file (asdf:cl-source-file) ()))
(defmethod asdf:source-file-type ((c atn-file) (s asdf:module)) "atn")


(asdf:defsystem :de.setf.atn-parser
  :nicknames (atn-parser)
  :version "0.9"
  :serial t
  :depends-on (:de.setf.utility
               ;; :de.setf.utility.dot
               )
  :components ((:module :clifs
                        :components ((:file "package")
                                     (:file "inference-system-classes" :depends-on ("package"))
                                     (:file "inference-units" :depends-on ("package"))))
               (:file "package" :depends-on (:clifs))
               (:file "atn-parameters" :depends-on ("package"))
               (:file "atn-classes" :depends-on ("atn-parameters"))
               (:file "atn-macros" :depends-on ("atn-classes"))
               (:file "ebnf-tokenizer" :depends-on ("atn-macros"))
               (:file "ebnf-to-atn-translator" :depends-on ("ebnf-tokenizer"))
               (:file "atn-macro-to-canonic-form" :depends-on ("ebnf-to-atn-translator"))
               ;; the bnf grammar is coded in a "atn" syntax lisp file.
               (:atn-file "ebnf-grammar" :depends-on ("atn-macro-to-canonic-form"))
               (:file "atn-runtime" :depends-on ("atn-classes"))
               (:file "conditions" :depends-on ("atn-classes"))
               (:file "atn-lisp-compiler" :depends-on ("atn-runtime" "conditions"))
               (:file "atn-regex" :depends-on ("atn-classes"))
               ;; the java translator is present here as documentation, as of 20010208 it has
               ;; not been reintegrated with the changes to compile to lisp.
               #+bnfp-java "xml:code;atn-parser;atn-java-compiler"))

:de.setf.atn-parser
