#lang scribble/manual

@(require scribble/bnf
          scribble/example
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

@(define (reftech . text)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") text))

Assuming this is saved in a file called "id3v1.b", you can import it
from Racket and apply any of the definitions to an @reftech{input
port} in order to parse its contents:

@(define ev (make-base-eval))
@(ev '(require racket/list racket/port))
@(define-syntax-rule (interactions e ...)
  (examples
   #:eval ev
   #:label #f
   e ...))

@interactions[
  (require "id3v1.b")
]

You can parse the magic header by itself:

@interactions[
  (magic (open-input-bytes #"TAG"))
]

Or a full tag:

@interactions[
  (define data
   (bytes-append
    #"TAGCreative Commons Song         Improbulus                    N"
    #"/A                           2005Take on O Mio Babbino Caro!   g"))
  (define tree
    (id3 (open-input-bytes data)))
]

And inspect the resulting parse tree:

@interactions[
  (map car tree)
  (define ref (compose1 cdr assq))
  (take (ref 'title_1 tree) 8)
  (apply bytes (ref 'title_1 tree))
]

Finally, parsing invalid data results in a syntax error:

@interactions[
  (eval:error
   (id3 (open-input-bytes #"TAG...")))
]

Every definition automatically creates an @deftech{un-parser}.
Un-parsers are functions that take a parse tree as input and serialize
the data to an @reftech{output port}.  They are named by prepending
@litchar{un-} to the name of a definition.

@interactions[
  (define bs
    (call-with-output-bytes
     (lambda (out)
       (un-id3 tree out))))
  (for ([n (in-range 0 (bytes-length bs) 64)])
    (println (subbytes bs n (+ n 64))))
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
        @nonterm{byte}
        @nonterm{char}
        @nonterm{id})
  (list @nonterm{byte}
        @elem{an integer between @litchar{0x00} and @litchar{0xFF}})
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
parses a @litchar{i8} to determine the length of a string, then parses
that number of @litchar{u8}s following it.

@codeblock|{
#lang binfmt
string = strlen u8{strlen_1};
strlen = i8;
}|

Negative length values are allowed, in which case they're treated the
same as @racket{0}.  The parser above would parse @racket#{#"\xFF"} to
an empty string.

The following parsers are built-in:

@itemlist[
  @item{@litchar{TODO}}
  @item{@litchar{u8}, @litchar{u16}, @litchar{u32}, @litchar{u64}, @litchar{u16le}, @litchar{u32le}, @litchar{u64le}, @litchar{u16be}, @litchar{u32be}, @litchar{u64be}}
  @item{@litchar{i8}, @litchar{i16}, @litchar{i32}, @litchar{i64}, @litchar{i16le}, @litchar{i32le}, @litchar{i64le}, @litchar{i16be}, @litchar{i32be}, @litchar{i64be}}
  @item{@litchar{f32}, @litchar{f64}, @litchar{f32le}, @litchar{f64le}, @litchar{f32be}, @litchar{f64be}}
  @item{@litchar{uvarint32}, @litchar{uvarint64}}
  @item{@litchar{varint32}, @litchar{varint64}}
  @item{@litchar{nul}, @litchar{eof}}
]
