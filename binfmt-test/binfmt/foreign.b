#lang binfmt

@foreign-parsers "foreign-parsers.rkt" string;
@foreign-unparsers "foreign-parsers.rkt" {un-string string};

strings = u8 string{u8_1};
