#!/usr/bin/env wolframscript
(* ::Package:: *)

levelMeanInt[record_, level_]:= Mean@Flatten[record[[dataIntI, #[[1]];;#[[2]], 2]]&/@ level];

plotLevelsGetData[record_,level_]:=Block[{meanInt},
meanInt = levelMeanInt[record, level];
{{#[[1]], meanInt}, {#[[2]], meanInt}}&/@ level
]
plotLevels[record_]:=Block[{levels},
levels = record[[levelsI]];
ListLinePlot[plotLevelsGetData[record, #2],
PlotStyle->ColorData[colorMap][#1]
] & @@@ Transpose[{Range[1, Length@levels], levels}]
]
