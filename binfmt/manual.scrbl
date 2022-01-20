#lang scribble/manual

@(require (for-label binfmt
                     binfmt/runtime
                     racket/base
                     racket/contract))

@title{@tt{binfmt}: binary format parser generator}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[binfmt]

This package provides a @hash-lang[] for building binary format
parsers with support for limited context-sensitivity.

@section{Example}

Here is a parser definition for the ID3v1 format:

@codeblock|{
#lang binfmt
id3     = magic title artist album year comment genre;
magic   = 'T' 'A' 'G';
title   = u8{30};
artist  = u8{30};
album   = u8{30};
year    = u8{4};
comment = u8{30};
genre   = u8;
}|

Assuming this is saved in a file called "id3v1.b", you can import it
from Racket and apply any of the definitions to an input port in order
to parse it:

@racketblock[
  (require "id3v1.b")
  (magic (open-input-bytes #"TAG"))
  (id3 (open-input-bytes #"TAG..."))
]

@section{Reference}
@defmodule[binfmt/runtime]

@defparam[current-endianness endiannes (or/c 'big 'little) #:value (if (system-big-endian?) 'big 'litte)]{
  Determines in what byte order numeric values are parsed.  Defaults
  to the system endianness.
}
