#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"classify-nn.wls";

rawdata = BinaryDeserialize[Get[FileNameJoin[{"data","2014-lda_gt_0.01-scores.bin"}]]];
sdata = #1->#2&@@@ Transpose[{Standardize[Keys@rawdata, Mean, StandardDeviation], Values@rawdata}];


dirname = {
  "results",
  "nn-undersampling",
  DateString[{"Year","-", "Month", "-", "Day","_", "Hour24","-", "Minute","-","Second"}]
};

If[!DirectoryQ[#], CreateDirectory[#]]&@ FileNameJoin[dirname];

Put[
  "Finding the effect of undersampling on the f1-score.",
  FileNameJoin[Append[dirname, "info"]]
];

Function[per,
  Put[
    BinarySerialize @ Table[
        classifyTrail @ RandomSample @ Join[
            Select[sdata, Values@#===yes&],
            Take[RandomSample@#, Floor[N[per/100] * Length@#]]& @ Select[sdata, Values@#===no&]
	  ],
	10],
    FileNameJoin[Append[dirname, "2014-lda_gt_0.01-scores-undersample_"<>ToString[per]<>".bin"]]
  ]
]/@ {1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100};
