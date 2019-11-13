#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"classify-nn.wls";

rawdata = BinaryDeserialize[Get[FileNameJoin[{"data","2014-lda_gt_0.01-scores.bin"}]]];
sdata = #1->#2&@@@ Transpose[{Standardize[Keys@rawdata, Mean, StandardDeviation], Values@rawdata}];


dirname = {
  "results",
  "nn-undersampling-test-all",
  DateString[{"Year","-", "Month", "-", "Day","_", "Hour24","-", "Minute","-","Second"}]
};

If[!DirectoryQ[#], CreateDirectory[#]]&@ FileNameJoin[dirname];

Put[
  "Finding the effect of undersampling on the f1-score by training
 on undersampled data and testing on full data set",
  FileNameJoin[Append[dirname, "info"]]
];

classes = Union@Values@sdata;

Function[per,
  Put[
    BinarySerialize@Table[
      Function[{train, test},
    
        CloseKernels[];LaunchKernels[8];
        ParallelTable[
	  {{#1["F1Score"][yes], #1["Accuracy"]}, {#2["F1Score"][yes], #2["Accuracy"]}}&@@{ ClassifierMeasurements[#, train], ClassifierMeasurements[#, test]}&@ trainNet[hu, train, classes],
        {hu, 1, 10}]
      
      ] @@ {Join[
        Select[sdata, Values@#===yes&],
        Take[RandomSample@#, Floor[N[per/100] Length@#]] & @Select[sdata, Values@#===no&]
      ], sdata},
      10
    ],
    FileNameJoin[Append[dirname, "2014-lda_gt_0.01-scores-undersample-"<>ToString[per]<>".bin"]]
  ]
] /@ {50, 60, 70, 80, 90}