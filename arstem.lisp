;
; This package is a basic stemmer for Arabic words
; It is a direct Common Lisp port of the Lingua::AR::Word::Stem perl module by Andrea Benazzo <andy@slacky.it>
;
; Copyright (c) 2011 Arnaud Betremieux <arnaud.betremieux@arnoo.net>
;
; This program is free software; you can redistribute it and/or
; modify it under the same terms as Perl.
; (see http://dev.perl.org/licenses/)

(defpackage :arstem
    (:use     #:cl #:clutch #:anaphora)
    (:shadowing-import-from #:clutch #:while #:join #:in)
    (:export  #:stem))

(in-package :arstem)
(enable-brackets)

(defun stem (word)
  ;let's strip down every prefix and suffix I'm aware of.
  ;(actually suffixes relative to people like possessive adjs are NOT chopped)

	(awhen (~ "/^(
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
		$/x" word)
    (setf word {it 2}))
    

 ; let's strip down all other unnecessary letters according to the length of the word
 (case (length word)
    (3 word)
    (4 (four word))
    (5 (five word))
    (6 (six  word))
    (otherwise nil)))

(defun four (word)
  (acond
    ((~ "/(.)(.)(ا|ي|و)(.)/"   word) (str {it 1} {it 2} {it 4}))
	  ((~ "/(.)(ا|و|ط|ي)(.)(.)/" word) (str {it 1} {it 3} {it 4}))
    (t nil)))

(defun five (word)
  (acond
	  ((~ "/(.)(.)(ا)(ا)(.)/" word)     (str {it 1} {it 2} {it 5}))
	  ((~ "/(.)(ت|ي)(.)(ا)(.)/" word)   (str {it 1} {it 3} {it 5}))
	  ((~ "/(.)(و)(ا)(.)(.)/" word)     (str {it 1} {it 4} {it 5}))
	  ((~ "/(.)(ا)(.)(ي|و)(.)/" word)   (str {it 1} {it 3} {it 5}))
	  ((~ "/(.)(.)(.)(ا|ي|و)(.)/" word) (four (str {it 1} {it 2} {it 3} {it 5})))
	  ((~ "/(.)(.)(ا|ي)(.)(.)/" word)   (four (str {it 1} {it 2} {it 4} {it 5})))
    (t nil)))

(defun six (word)
  (acond
	  ((~ "/(.)(و)(ا)(.)(ي)(.)/" word) (str {it 1} {it 4} {it 6}))
	  ((~ "/(.)(.)(ا)(.)(ي)(.)/" word) (four (str {it 1} {it 2} {it 4} {it 6})))
    (t nil)))
