#lang binfmt

cstrings = u8 cstring{u8_1};

cstring = nul | u8 cstring;
