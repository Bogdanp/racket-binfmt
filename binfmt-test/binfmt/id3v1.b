#lang binfmt

id3     = magic title artist album year comment genre;
magic   = 'T' 'A' 'G';
title   = u8{30};
artist  = u8{30};
album   = u8{30};
year    = u8{4};
comment = u8{30};
genre   = u8;
