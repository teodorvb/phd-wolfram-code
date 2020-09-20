#!/usr/bin/env wolframscript
(* ::Package:: *)

nnetTrail[train_, test_, hus_, epoch_]:= Block[{},
CloseKernels[];LaunchKernels[];
ParallelTable[
 {ClassifierMeasurements[#, train, "F1Score"][yes],
ClassifierMeasurements[#, test, "F1Score"][yes]
}&@NetTrain[ 
NetInitialize[
NetChain[{LinearLayer[hu], Tanh, LinearLayer[2], SoftmaxLayer[]},
"Input" -> Length@Keys@First@train,
"Output"-> NetDecoder[{"Class", {yes, no}}]
],
Method->{"Xavier", "FactorType"-> "Mean", "Distribution"-> "Normal"}],
train,
Method->"ADAM",
LossFunction-> CrossEntropyLossLayer["Index"],
MaxTrainingRounds->epoch,
TrainingProgressReporting->None
],
{hu, hus}]
]
hus = {1,  5, 10, 15, 20, 25, 30, 40, 50, 60, 80, 100};


randomTrainTest[tracksSimulatedLDMC_, tracksSimulatedLDManual_]:=Block[{trainT, testT},
{trainT, testT} = TakeDrop[
RandomSample[tracksSimulatedLDMC],
Floor[0.7 * Length @tracksSimulatedLDMC]
];

{
labelMC[Join[trainT, tracksSimulatedLDManual], hw],
labelMC[testT, hw]
}
]


whitenData[data_] := whitenData[data, Mean@Keys@data, StandardDeviation@Keys@data];
whitenData[data_, m_, sd_]:=#1-> #2 &@@@ Transpose[{(#-m)/sd&/@Keys@data, Values@data}];


labelManual[tracks_, steps_, hw_]:= Flatten[labelManualTrack[#, steps, hw]&/@ tracks];
labelManualTrack[track_, steps_, hw_]:=Block[{intensity, positive},
positive = steps[track[[{1, 2}]]];
intensity = track[[dataIntI, All, 2]];
intensity[[#-hw;; #+hw]]-> If[Count[positive, #]!= 0, yes, no]&/@Range[hw+1, Length[intensity]-hw]
];
