#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"classify-hmm.wls";


data = Flatten[{
  Select[#, Values@# === yes&],
  Take[RandomSample[Select[#, Values@#===no&]], 2400]
  }, 1]&@BinaryDeserialize[Get[FileNameJoin[{"data","2014-lda_gt_0.01-int-l12cropped.bin"}]]];


dirname = {
  "results",
  "hmm-gfix",
  DateString[{"Year","-", "Month", "-", "Day","_", "Hour24","-", "Minute","-","Second"}]
};

If[!DirectoryQ[#], CreateDirectory[#]]&@ FileNameJoin[dirname];


Put[
  "Trying to find lower threshold to co-variance matrix for Gaussian distribution. 
Positive class have all samples.  Negative class is undersampled with uniform 
random distribution. 
Positive: 299
Negative: 2400
",
  FileNameJoin[Append[dirname, "info"]]
];


CloseKernels[]; LaunchKernels[8];

Install["fit_hmm"];
ParallelEvaluate[Install["fit_hmm"]];

epoch = 100;
hs = 5;
ParallelTable[
    MeasureHMMF1[N[Join[
    First@Select[data, Values@#===yes&],
    First@Select[data, Values@#===no&]
]], TrainHMM[N[data], hs, epoch, N[10^gfix]]],
    {gfix, -10, -1}]
    
ParallelEvaluate[Install["fit_hmm"]];

Put[
  BinarySerialize@ParallelTable[
    MeasureHMMF1[N[data], TrainHMM[N[data], hs, epoch, N[10^gfix]]],
    {gfix, -10, -1}
  ],
  FileNameJoin[Append[dirname, "2014-lda_gt_0.01-int-l12cropped-gfix.bin"]]
]
