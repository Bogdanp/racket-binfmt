#lang binfmt

@foreign-parsers "foreign-parsers.rkt"
  {string un-string};

strings = u8 string{u8_1};
