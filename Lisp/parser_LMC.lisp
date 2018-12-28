;;;; -*- Mode:Lisp -*-
;;; File per il parsing dell'assembly del LMC
;;; Author: Marco Natali 829843

(defun lmc-load (Filename)
       (with-open-file (input Filename
                           :direction :input)
       (let keywords (create-keyword));;;Crea la lista delle keyword dell'assembly
       (read-file input keywords);;; Ritorna la lista degli elementi in memoria



(defun read-file (input keywords)
       ())


(defun create-keyword ()
       (defstruct (keyword  (:constructor create-keyword (element value)))
                   element value)
       (lists (create-keyword "ADD" 100)
              (make-struct "SUB" 200)
              (make-struct "STA" 300)
              (create-keyword "LDA" 500)
              (create-keyword "BRA" 600)
              (create-keyword "BRZ" 700)
              (create-keyword "BRP" 800)
              (create-keyword "INP" 901)
              (create-keyword "OUT" 902)
              (create-keyword "HLT" 0)
              (create-keyword "DAT" 0)))
