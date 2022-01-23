#lang binfmt

cstrings = u8 cstring{u8_1};

cstring = 0x00 | u8 cstring;
