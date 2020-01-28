#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"helpers.wls"


(* Level Gap Higher Intensity *)
states = {
  Join[
    Table[{1, 0}, 20],
    Table[{0, 1}, 3],
    Table[{0, 0}, 20],
    Table[{0, 1}, 3],
    Table[{1, 0}, 20],
    Table[{1, 1}, 3],
    Table[{0, 0}, 20],
    Table[{1, 1}, 3],
    Table[{1, 0}, 20],
    Table[{0, 0}, 20],
    Table[{1, 1}, 3],
    Table[{0, 1}, 3],
    Table[{0, 0}, 20],
    Table[{0, 1}, 3],
    Table[{1, 1}, 3],
    Table[{0, 0}, 20],
    Table[{0, 1}, 3],
    Table[{1, 0}, 20],
    Table[{1, 1}, 3],
    Table[{1, 0}, 20],
    Table[{0, 1}, 3],
    Table[{0, 0}, 20],
    Table[{0, 1}, 3],
    Table[{1, 0}, 3],
    Table[{1, 1}, 3],
    Table[{1, 0}, 3],
    Table[{0, 1}, 3],
    Table[{0, 0}, 3]
  ]
};

ndata = addNoise@generateDataSet[states, 120,60,80];
exportSimulatedData["ld-lvlgap-batch_", 1, ndata, states];


states = {
 Join[
  Table[{1, 1}, 20],
  Table[{0, 1}, 3],
  Table[{1, 0}, 20],
  Table[{0, 1}, 3],
  Table[{0, 0}, 20],
  Table[{0, 1}, 3],
  Table[{1, 0}, 10],
  Table[{0, 1}, 3],
  Table[{1, 1}, 10],
  Table[{0, 1}, 3],
  Table[{1, 0}, 5],
  Table[{0, 1}, 3],
  Table[{0, 0}, 5]
 ]
};

ndata = addNoise@generateDataSet[states, 120,60,80];
exportSimulatedData["ld-lvlgapLI-batch_", 1, ndata, states];
