#!/usr/bin/env wolframscript
(* ::Package:: *)

trainNet[hu_,data_, classes_]:=NetTrain[
NetInitialize[
  NetChain[{
    LinearLayer[hu], Tanh,
    LinearLayer[Length[classes]], SoftmaxLayer[]
  },
  "Input"-> Length@First@First@data,
  "Output"-> NetDecoder[{"Class",classes}]
  ],
Method->{"Xavier", "FactorType"-> "Mean", "Distribution"-> "Normal"}],
data,
LossFunction->CrossEntropyLossLayer["Index"],
Method->"ADAM",
MaxTrainingRounds->1000,
TrainingProgressReporting->None
];

measureNet[hi_, train_, test_, classes_]:= {
{#1["F1Score"][yes], #1["Accuracy"]}, {#2["F1Score"][yes], #2["Accuracy"]}}&@@{ ClassifierMeasurements[#, train], ClassifierMeasurements[#, test]
}&@trainNet[hi, train, classes];

classifyTrail[data_]:=Block[{train, test, classes},
classes =  Union@Values@data;
{train, test} = TakeDrop[data,Floor[0.7 Length[data]]];
CloseKernels[];LaunchKernels[];
Transpose@ParallelTable[measureNet[hu, train, test,classes], {hu, 1, 10}]
];

classifyTrail[data_, hus_]:=Block[{train, test, classes},
  index++;
  classes =  Union@Values@data;
  {train, test} = TakeDrop[RandomSample[data],Floor[0.7 Length[data]]];
  CloseKernels[];LaunchKernels[];
  Transpose@ParallelTable[measureNet[hu, train, test,classes], {hu, hus}]
];
