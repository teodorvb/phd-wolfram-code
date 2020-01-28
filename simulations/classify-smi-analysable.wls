#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"helpers.wls"


fI = 80;
fB = 30;


If[Length[$ScriptCommandLine] < 2, 
  Print["Need to specify the first batchid"];
  Exit[]
]


batches = 10;
firstBatchId = ToExpression[$ScriptCommandLine[[2]]];


exportData[
NotebookDirectory[],
getDataName["an-batch_"<>numStr[#1],Length[#2], fI,fB],
#2
]&@@@ Transpose[{
Flatten@Table[{i, i}, {i, firstBatchId, firstBatchId + batches-1}],
addNoise[Quiet@generateAnalysable[fI, fB, {
100, 50,
100, 50,
100, 50,
100, 50,
100, 50,
100, 50,
100, 50,
100, 50,
100, 50,
100, 50
}, frameSize, 1]]
}];
