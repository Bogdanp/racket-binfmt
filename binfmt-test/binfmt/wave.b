#lang binfmt

wave            = header fmt data;

header          = magic chunksize format;
magic           = 'R' 'I' 'F' 'F';
chunksize       = u32le;
format          = 'W' 'A' 'V' 'E';

fmt             = fmt-magic chunksize audio-format num-channels sample-rate byte-rate block-align bits-per-sample;
fmt-magic       = 'f' 'm' 't' ' ';
audio-format    = i16le;
num-channels    = i16le;
sample-rate     = i32le;
byte-rate       = i32le;
block-align     = i16le;
bits-per-sample = i16le;

data            = data-magic chunksize u8{chunksize_1};
data-magic      = 'd' 'a' 't' 'a';
