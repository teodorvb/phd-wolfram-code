#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"classify-hmm.wls";


data = Flatten[{
  Select[#, Values@# === yes&],
  Take[RandomSample[Select[#, Values@#===no&]], 2400]
  }, 1]&@BinaryDeserialize[Get[FileNameJoin[{"data","2014-lda_gt_0.01-int-l12cropped.bin"}]]];


dirname = {
  "results",
  "hmm",
  DateString[{"Year","-", "Month", "-", "Day","_", "Hour24","-", "Minute","-","Second"}]
};

If[!DirectoryQ[#], CreateDirectory[#]]&@ FileNameJoin[dirname];


Put[
  "Applying analysis on reduced data set. Positive class have all samples. 
Negative class is undersampled with uniform random distribution.
Positive: 299
Negative: 2400
",
  FileNameJoin[Append[dirname, "info"]]
];


CloseKernels[]; LaunchKernels[8];

Install["fit_hmm"];
ParallelEvaluate[Install["fit_hmm"]];

samples = 10;
epoch = 100;
gfix = N[10^-8];

ParallelTable[
  Quiet@MeasureHMMF1[#, Quiet@TrainHMM[#, hs, epoch, N[10^gfix]]] &@ N@Join[
      First@Select[data, Values@#===yes&],
      First@Select[data, Values@#===no&]
    ],
  {gfix, -10, -1}
];
    
ParallelEvaluate[Install["fit_hmm"]];



Function[hs,
  Put[
    BinarySerialize[
      ParallelTable[
        Function[{train, test},
          {MeasureHMMF1[train, #]&, MeasureHMMF1[test, #]}&@TrainHMM[train, hs, epoch, gfix];
        ]@@TakeDrop[RandomSample@N@data, Floor[0.7 Length@data]],
        samples
      ]
    ],
    FileNameJoin[Append[dirname, "2014-lda_gt_0.01-int-l12cropped-hs_"<>ToString[hs]<>".bin"]]
  ]
]/@ Range[2, 10, 2];



