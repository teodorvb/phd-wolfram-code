#!/usr/bin/env wolframscript
(* ::Package:: *)

batches = SmiMakeDBIdBatches[
Join[
SmiReadPositiveIds[conn, 13],
SmiReadNegativeIds[conn, 13]
],
100
];
dataTracks = Flatten[
SmiReadBatch[conn, #, ExtractIntTrackL12Cropped] & /@ batches,
1];

Put[BinaryDeserialize[dataTracks],FileNameJoin[{"data", "2014-lda_lt_0.01-int-l12cropped.bin"}]];
