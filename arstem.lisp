;
; This package is a basic stemmer for Arabic words
; It is a direct Common Lisp port of the Lingua::AR::Word::Stem perl module by Andrea Benazzo <andy@slacky.it>
;
; Copyright (c) 2011 Arnaud Betremieux <arno@arnoo.net>
;
; This program is free software; you can redistribute it and/or
; modify it under the same terms as Perl.
; (see http://dev.perl.org/licenses/)

(defpackage :arstem
    (:use     #:cl #:anaphora)
    (:export  #:stem))

(in-package :arstem)

; utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bracket-reader (stream char)
    (declare (ignore char))
      `(elt ,@(read-delimited-list #\} stream t)))

  (set-macro-character #\{ #'bracket-reader)
  (set-macro-character #\} (get-macro-character #\) nil)))

(defun str (&rest parts)
  (apply #'concatenate (cons 'string parts)))

(defun vector-to-list* (object)
  (declare (optimize speed))
  (let ((result (list nil))
        (length (length object)))
       (declare (fixnum length))
       (do ((index 0 (1+ index))
            (splice result (cdr splice)))
           ((= index length) (cdr result))
           (declare (fixnum index))
           (rplacd splice (list (aref object index))))))

(defmacro acond (&rest forms)
  "Anaphoric cond : like a regular cond, except the result of evaluating the condition form can be accessed as <it>"
    (let ((blockname (gensym)))
       `(block ,blockname
            ,@(loop for form in forms
                      collect `(awhen ,(car form) (return-from ,blockname ,(cadr form))))
            nil)))

(defun ~ (re string)
    "Returns nil if regular expression <re> does not match the string <str>
     returns the part of the string that matches <re> and all grouped matches otherwise

     example : (~ \"\w+(\d)/\" \"ab2cc\")"
     (let ((cleanre (cl-ppcre:regex-replace-all "\\s" re "")))
        (cl-ppcre:do-matches (match-start match-end cleanre string)
                             (let ((matches (multiple-value-list (cl-ppcre:scan-to-strings cleanre
                                                                          string
                                                                          :start match-start
                                                                          :end match-end))))
                               (return (cons (car matches) (vector-to-list* (cadr matches))))))))

(defun four (word)
  (acond
    ((~ "(.)(.)(ا|ي|و)(.)"   word) (str {it 1} {it 2} {it 4}))
	  ((~ "(.)(ا|و|ط|ي)(.)(.)" word) (str {it 1} {it 3} {it 4}))
    (t nil)))

(defun five (word)
  (acond
	  ((~ "(.)(.)(ا)(ا)(.)" word)     (str {it 1} {it 2} {it 5}))
	  ((~ "(.)(ت|ي)(.)(ا)(.)" word)   (str {it 1} {it 3} {it 5}))
	  ((~ "(.)(و)(ا)(.)(.)" word)     (str {it 1} {it 4} {it 5}))
	  ((~ "(.)(ا)(.)(ي|و)(.)" word)   (str {it 1} {it 3} {it 5}))
	  ((~ "(.)(.)(.)(ا|ي|و)(.)" word) (four (str {it 1} {it 2} {it 3} {it 5})))
	  ((~ "(.)(.)(ا|ي)(.)(.)" word)   (four (str {it 1} {it 2} {it 4} {it 5})))
    (t nil)))

(defun six (word)
  (acond
	  ((~ "(.)(و)(ا)(.)(ي)(.)" word) (str {it 1} {it 4} {it 6}))
	  ((~ "(.)(.)(ا)(.)(ي)(.)" word) (four (str {it 1} {it 2} {it 4} {it 6})))
    (t nil)))

(defun stem (word)
  ;let's strip down every prefix and suffix I'm aware of.
  ;(actually suffixes relative to people like possessive adjs are NOT chopped)

	(awhen (~ "^(
		[وفب]*ال|
		[بيلمتوسن]*ت|
		[بلوكف]*م|
		[ال]*ل|[
		ولسف]*ي|[
		وفلب]*ا|
		)
		(.*?)
		(
		ات|
		وا|
		تا|
		ون|
		وه|
		ان|
		تي|
		ته|
		تم|
		كم|
		ه[نم]*|
		ها|
		ية|
		تك|
		نا|
		ي[نه]*|
		[ةهيا]|
		)
		$" word)
    (setf word {it 2}))
    
 ; let's strip down all other unnecessary letters according to the length of the word
 (case (length word)
    (3 word)
    (4 (four word))
    (5 (five word))
    (6 (six  word))
    (otherwise nil)))
