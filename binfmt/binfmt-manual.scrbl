#lang scribble/manual

@(require scribble/bnf
          (for-label binfmt
                     binfmt/runtime
                     racket/base
                     racket/contract))

@title{@tt{binfmt}: binary format parser generator}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodulelang[binfmt]

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
from Racket and apply any of the definitions to an @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{input port}
in order to parse its contents:

@racketblock[
  (require "id3v1.b")
  (magic (open-input-bytes #"TAG"))
  (id3 (open-input-bytes #"TAG..."))
]


@section{Grammar and Operation}

The grammar for @racketmodname[binfmt] is as follows:

@BNF[
  (list @nonterm{def}
        @BNF-seq[@nonterm{alt} @kleenestar[@BNF-group[@litchar{|} @nonterm{alt}]] @litchar{;}])
  (list @nonterm{alt}
        @kleeneplus{expr})
  (list @nonterm{expr}
        @BNF-alt[@nonterm{term} @nonterm{star} @nonterm{plus} @nonterm{repeat}])
  (list @nonterm{star}
        @BNF-seq[@nonterm{term} @litchar{*}])
  (list @nonterm{plus}
        @BNF-seq[@nonterm{term} @litchar{+}])
  (list @nonterm{repeat}
        @BNF-seq[@nonterm{term} @litchar["{"] @BNF-alt[@nonterm{id} @nonterm{natural}] @litchar["}"]])
  (list @nonterm{term}
        @nonterm{char}
        @nonterm{id})
  (list @nonterm{char}
        @BNF-seq[@litchar{'} @elem{ascii character} @litchar{'}])
  (list @nonterm{id}
        @elem{any identifier})
  (list @nonterm{natural}
        @elem{any natural number})
]

Within an @nonterm{alt}, each @nonterm{expr} is assigned a unique name
based on its @nonterm{id}: the first time an @nonterm{id} appers in an
alt, @litchar{_1} is appended to its name, the second time
@litchar{_2}, and so on.

Alternatives containing two or more @nonterm{expr}s parse to an
association list mapping @nonterm{expr} names (as defined above) to
parse results.  Alternatives containing a single @nonterm{expr}
collapse to the result of the @nonterm{expr}.

The @nonterm{repeat} syntax can either repeat a parser an exact number
of times or it can repeat it based on the result of a previous parser
within the same @nonterm{alt}.  For example, the following parser
parses a @litchar{u8} to determine the length of a string, then parses
that number of @litchar{u8}s following it.

@codeblock|{
#lang binfmt
string = strlen u8{strlen_1};
strlen = u8;
}|


@section{Reference}
@subsection{Runtime}
@defmodule[binfmt/runtime]

The following parsers are built-in:

@itemlist[
  @item{@litchar{nul}}
  @item{@litchar{u8},  @litchar{u16}, @litchar{u32}, @litchar{u64}}
  @item{@litchar{i8},  @litchar{i16}, @litchar{i32}, @litchar{i64}}
  @item{@litchar{f32}, @litchar{f64}}
]

@defparam[current-endianness endiannes (or/c 'big 'little) #:value (if (system-big-endian?) 'big 'litte)]{
  Determines in what byte order numeric values are parsed.  Defaults
  to the system endianness.
}
