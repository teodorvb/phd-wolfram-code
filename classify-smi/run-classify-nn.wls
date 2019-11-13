#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"classify-nn.wls";


rawdata = BinaryDeserialize[Get[FileNameJoin[{"data","2014-lda_gt_0.01-revised-scores.bin"}]]];


sdata = #1->#2&@@@ Transpose[{Standardize[Keys@rawdata, Mean, StandardDeviation], Values@rawdata}];


proj = #1-> #2&@@@Transpose[{Keys[sdata].Transpose[Eigenvectors[Covariance[Keys[sdata]]]], Values@sdata}];


dirname = {
  "results",
  "nn",
  DateString[{"Year","-", "Month", "-", "Day","_", "Hour24","-", "Minute","-","Second"}]
};

If[!DirectoryQ[#], CreateDirectory[#]]&@ FileNameJoin[dirname];

Put[
  "Applying analysis on full data set. Source data is 2014-lda_gt_0.01-revised-scores.bin",
  FileNameJoin[Append[dirname, "info"]]
];

Function[pc,
  Block[{data},
    data = #1-> #2 &@@@ Transpose[{Keys[proj][[All, 1;;pc]], Values[proj]}];
    Put[
      BinarySerialize[Table[classifyTrail[RandomSample[data]], 10]],
      FileNameJoin[Append[dirname, "2014-lda_lt_0.01-revised_scores-pca_"<>ToString[pc]<>".bin"]]
    ]
  ]
]/@ Append[Range[2, 36, 4], 37];
