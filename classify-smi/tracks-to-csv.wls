#!/usr/bin/env wolframscript
(* ::Package:: *)

data = BinaryDeserialize[Get[FileNameJoin[{Directory[], "data", "2014-lda_gt_0.01-int-l12cropped.bin"}]]];


Export[FileNameJoin[{Directory[], "data", "2014-lda_gt_0.01-int-l12cropped.csv"}],
  {BaseEncode@ExportByteArray[Flatten@Keys@#, "Real32"], Values@#}&/@data
]
