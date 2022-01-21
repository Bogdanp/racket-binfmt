#lang binfmt

wave            = header fmt data;

header          = magic chunksize format;
magic           = 'R' 'I' 'F' 'F';
chunksize       = u32;
format          = 'W' 'A' 'V' 'E';

fmt             = fmt-magic chunksize audio-format num-channels sample-rate byte-rate block-align bits-per-sample;
fmt-magic       = 'f' 'm' 't' ' ';
audio-format    = i16;
num-channels    = i16;
sample-rate     = i32;
byte-rate       = i32;
block-align     = i16;
bits-per-sample = i16;

data            = data-magic chunksize;
data-magic      = 'd' 'a' 't' 'a';
