#!/usr/bin/env wolframscript
(* ::Package:: *)

AppendTo[$Path, FileNameJoin[{Directory[], ".." ,"..", "common"}]];
<<RBM`

labelled = BinaryDeserialize[Get[FileNameJoin[{Directory[], "..", "labelled.bin"}]]];

(* Split the data set into training and testing set and store to disk *)
{acTrain, acTest} = TakeDrop[
  Standardize[N[Keys[RandomSample[labelled]]], Mean, 1&],
  Floor[0.7 * Length[labelled]]
];

BinarySerialize[{acTrain, acTest}]>>acData.bin
data = <||>;
rbms = <||>;

acEpoch = 10000;
epoch = 5000;
lrate = 0.01;
momentum = 0.9;

data[121] = acTrain;

(* Train rbms 121 \[Rule] 50 \[Rule] 25 *)
Print["Training RBM 50"];
rbms[50] = RBMTrain[50, data[121],
  LearningRate-> lrate,
  Momentum-> momentum,
  MaxTrainingRounds-> epoch];

Print["Training RBM 25"];
data[50] = RBMHiddenExpectation[rbms[50], data[121]];
rbms[25] = RBMTrain[25, data[50],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds-> epoch];

data[25] = RBMHiddenExpectation[rbms[25], data[50]];

(* Train rbms 25 \[Rule] 12 \[Rule] 6 \[Rule] 3 *)
Print["Training RBM 12"];
rbms[12] = RBMTrain[12, data[25],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds -> epoch];

Print["Training RBM 6"];
data[12] = RBMHiddenExpectation[rbms[12], data[25]];
rbms[6] = RBMTrain[6, data[12],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds -> epoch];

Print["Training RBM 3"];
data[6] = RBMHiddenExpectation[rbms[6], data[12]];
rbms[3] = RBMTrain[3, data[6],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds -> epoch];

Print["Training RBM 15"];
(* Train rbms 25 \[Rule] 15 \[Rule] 10 \[Rule] 5 \[Rule] 2 *)
rbms[15] = RBMTrain[15, data[25],
  LearningRate-> lrate,
  Momentum -> momentum,
  MaxTrainingRounds-> epoch];

Print["Training RBM 10"];
data[15] = RBMHiddenExpectation[rbms[15], data[25]];
rbms[10] = RBMTrain[10, data[15],
  LearningRate-> lrate,
  Momentum -> momentum,
  MaxTrainingRounds-> epoch];

Print["Training RBM 5"];
data[10] = RBMHiddenExpectation[rbms[10], data[15]];
rbms[5] = RBMTrain[5, data[10],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds-> epoch];

Print["Training RBM 2"];
data[5] = RBMHiddenExpectation[rbms[5], data[10]];
rbms[2] = RBMTrain[2, data[5],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds -> epoch];

(* Train rbms 15 \[Rule] 8 \[Rule] 4 *)
Print["Training RBM 8"];
rbms[8] = RBMTrain[8, data[15],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds -> epoch];

Print["Training RBM 4"];
data[8] = RBMHiddenExpectation[rbms[8], data[15]];
rbms[4] = RBMTrain[4, data[8],
  LearningRate -> lrate,
  Momentum -> momentum,
  MaxTrainingRounds -> epoch];
 
tdata=(#1[[1]]->#1[[2]]&)/@Transpose[{data[121],data[121]}];
ac = <||>;
ac[25] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25]}], "Input" -> 121];
ac[12] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[12]}], "Input" -> 121];
ac[6] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[12], rbms[6]}], "Input" -> 121];
ac[3] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[12], rbms[6], rbms[3]}], "Input" -> 121];
ac[15] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[15]}], "Input" -> 121];
ac[10] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[15], rbms[10]}], "Input" -> 121];
ac[5] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[15], rbms[10], rbms[5]}], "Input" -> 121];
ac[2] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[15], rbms[10], rbms[5], rbms[2]}], "Input" -> 121];
ac[8] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[15], rbms[8]}], "Input" -> 121];
ac[4] = NetChain[RBMAutoEncoder[{rbms[50], rbms[25], rbms[15], rbms[8], rbms[4]}], "Input" -> 121];

Print["Training AutoEncoders"];

SetSharedVariable[ac];
ParallelMap[Function[ key,
  Block[{net, trained},
    net = ac[key];
    trained = NetTrain[net, tdata,
      Method-> "ADAM",
      MaxTrainingRounds->acEpoch,
      TrainingProgressReporting-> None];
    CriticalSection[{nlock}, AssociateTo[ac, key -> <|PreTrained -> net, Trained -> trained|>]];
  ]
], Keys[ac]];

BinarySerialize[ac]>>"ac.bin";
BinarySerialize[Eigenvectors[Covariance[acTrain]]]>>"components.bin"
