ArStem
======

AtStem is a basic Common Lisp stemmer for Arabic words.

It is a direct port of the Lingua::AR::Word::Stem Perl module by Andrea Benazzo <andy@slacky.it>

I have no idea of the level of correctness of the original stemmer. The only guarantee I can offer is that at the time of this writing, this port returns the same results as the original Perl module, at least for any word in the Tanzil.net Qu'ran. (with one slight exception: failed stemming returns nil, while the Perl version returns "NotFound").

> (require 'arstem)
> (use-package 'arstem)
> (stem "")


Copyright (c) 2011 Arnaud Betremieux <arnaud.betremieux@arnoo.net>

This program is free software; you can redistribute it and/or modify it under the same terms as Perl (see http://dev.perl.org/licenses/)
