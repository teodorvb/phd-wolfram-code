#!/usr/bin/env wolframscript
(* ::Package:: *)

nnetMeasure[{trainData_, testData_}, hiddenUnits_, classes_]:={
  ClassifierMeasurements[#,trainData, "Accuracy"],
  ClassifierMeasurements[#,testData, "Accuracy"]
}& @ NetTrain[
	  NetInitialize[
		NetChain[{
			LinearLayer[hiddenUnits],
			Tanh,
			LinearLayer[Length[classes]],
			SoftmaxLayer[]
			}, 
		  "Input" -> Length[Keys[trainData[[1]]]],
		  "Output" -> NetDecoder[{"Class",classes}]],
	   Method -> "Orthogonal"],
	trainData,
	CrossEntropyLossLayer["Target" -> NetEncoder[{"Class", classes}]] ,
	Method-> "RMSProp",
	MaxTrainingRounds->15000,
	TargetDevice-> "CPU",
	TrainingProgressReporting -> None];

NNExperiment[labelled_, {huStart_, huEnd_}, samples_]:= Block[
{classes, randomLabelled},
classes = Union[Values[labelled]];
Table[
  Print["  Working on HUS: ", hus];
  CloseKernels[]; LaunchKernels[16];
  DistributeDefinitions[{labelled, folds, classes, hus, nnetMeasure,crossValidate}];
  ParallelTable[
    nnetMeasure[TakeDrop[RandomSample[labelled], Floor[0.7 * Length[labelled]]], hus, classes],
    samples],
  {hus, huStart, huEnd}]
];

compressed = BinaryDeserialize[<<compressed.bin];

Function[key, Block[{res, fname},
  Print[key];
  res = NNExperiment[compressed[key],{1,30},10];
  fname = StringReplace[key<>".bin", Whitespace ->"-"];
  If[FileExistsQ[fname],
    Print["Updating"]; 
    Put[BinarySerialize[
      Transpose[Join[
        Transpose[res], 
        Transpose[BinaryDeserialize[Get[fname]]] 
      ]]
    ], fname];
    Put[BinarySerialize[res], fname<>".inc"],
  
    Print["Creating New File"];
    Put[BinarySerialize[NNExperiment[compressed[key],{1,30},10]], fname]
  ]  
]] /@ Keys[compressed];
