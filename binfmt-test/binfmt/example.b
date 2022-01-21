#lang binfmt

batch       = header trades eof;

header      = magic version reserved hour offsets;
magic       = 't' 'r' 'a' 'd';
version     = u32be;
reserved    = u32be;
hour        = i64be;

offsets     = num-offsets offset{num-offsets_1};
num-offsets = u16be;
offset      = seconds posn num-trades;
seconds     = u16be;
posn        = u64be;
num-trades  = u64be;

trades      = trade*;
trade       = trade-len u8{trade-len_1};
trade-len   = u16be;
