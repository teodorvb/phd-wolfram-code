#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"helpers.wls"


If[Length[$ScriptCommandLine] < 2, 
  Print["Need to specify the first batchid"];
  Exit[]
]


batches = 10;
firstBatchId = ToExpression[$ScriptCommandLine[[2]]];


(* Level Changes with Equal Intensity *)

states = Table[
FluorophoresHMM[400, {{0.98, 0.018, 0.002}, {0.02, 0.98, 0}, {0, 0, 1}}],
 batches];
ndata = addNoise@generateDataSet[states, 60,120,80];
exportSimulatedData["ld-mc-batch_", firstBatchId, ndata, states];


(* Small Intensity Change. *)
states = Table[
FluorophoresHMM[400, {{0.98, 0.018, 0.002}, {0.02, 0.98, 0}, {0, 0, 1}}],
 batches];

ndata = addNoise@generateDataSet[states, 30,80,80];
exportSimulatedData["ld-mc-dim-batch_", firstBatchId, ndata, states];


(* Short Levels *)

states = Table[
FluorophoresHMM[400, {{0.92, 0.078, 0.002}, {0.08, 0.92, 0}, {0, 0, 1}}],
 batches];
 
ndata = addNoise@generateDataSet[states, 60,120,80];
exportSimulatedData["ld-mc-short-batch_",41, ndata, states];
