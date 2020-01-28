#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"helpers.wls"


fI = 80;
fB = 30;
beads = addNoise@generateBeads[fI,fB, 5, frameSize];
exportBeads[FileNameJoin[{NotebookDirectory[],getDataName["beads-100x100", 5, fI, fB]}],beads[[1]]];
