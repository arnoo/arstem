;
; Copyright 2011 Arnaud Betremieux <arno@arnoo.net>
;
; This program is free software; you can redistribute it and/or
; modify it under the same terms as Perl.
; (see http://dev.perl.org/licenses/)

(in-package :cl-user)

(defpackage :arstem-asd
  (:use :cl :asdf))

(in-package :arstem-asd)

(defsystem :arstem
  :name "arstem"
  :maintainer "Arnaud Betremieux"
  :licence "Perl"
  :serial t
  :version 0.1
  :components ((:file "arstem"))
  :depends-on (:anaphora)
  )
