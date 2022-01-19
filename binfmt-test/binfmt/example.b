#lang binfmt

batch       = header trades eof;

header      = magic version reserved hour offsets;
magic       = 't' 'r' 'a' 'd';
version     = u32;
reserved    = u32;
hour        = i64;

offsets     = num-offsets offset{num-offsets_1};
num-offsets = u16;
offset      = seconds posn num-trades;
seconds     = u16;
posn        = u64;
num-trades  = u64;

trades      = trade*;
trade       = trade-len u8{trade-len_1};
trade-len   = u16;
