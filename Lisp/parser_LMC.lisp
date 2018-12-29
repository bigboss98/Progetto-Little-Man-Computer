;;;; -*- Mode:Lisp -*-
;;; File per il parsing dell'assembly del LMC
;;; Author: Marco Natali 829843

(defun lmc-load (Filename)
       (with-open-file (input Filename
                           :direction :input)
       (let keywords create-keyword);;;Crea la lista delle keyword dell'assembly
       (read-file input keywords);;; Ritorna la lista degli elementi in memoria


;; Funzione per leggere il file sorgente
(defun read-file (stream keyword)
       (setf (nth memoria counter)
             (read-line stream keyword))
       (if ())
       read-file(stream keyword)

(defun read-line (stream keyword))


(defun create-keyword ()
       (defstruct (keyword  (:constructor create-keyword (element value)))
                   element value)
       (lists (create-keyword "ADD" 100)
              (create-keyword "SUB" 200)
              (create-keyword "STA" 300)
              (create-keyword "LDA" 500)
              (create-keyword "BRA" 600)
              (create-keyword "BRZ" 700)
              (create-keyword "BRP" 800)
              (create-keyword "INP" 901)
              (create-keyword "OUT" 902)
              (create-keyword "HLT" 0)
              (create-keyword "DAT" 0)))

;; Funzione per definire lo split di una stringa
(defun split (string &optional (separator " ") &default counter 0)
       (let letter (char string counter))
       (cond (eq letter separator)
                 ()
             ( letter '\n')

       )
(defun split (string &optional (counter 1) (separator " "))
       (split-string string separator))

(defun split-string (string separator counter &optional ))
      (cond (eq (char string counter) separator)
                 (append    elem )
                 (split-string string separator (+ counter 1) )
             (eq (char string counter) '\n'))
                 (concatenate elem (char string counter))
             ()
