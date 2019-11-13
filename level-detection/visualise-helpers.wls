#!/usr/bin/env wolframscript
(* ::Package:: *)

plotTrackLevels[lvls_, track_]:=Block[{maxI},
maxI = Max[track[[3, All, 2]]];
Show[
Prepend[
Function[{id, lvl},Block[{m},
m = Mean[Flatten[track[[3,#1;;#2,2]]&@@@lvl]];
ListLinePlot[{{#1, m}, {#2, m}}&@@@lvl, 
PlotRange-> {All, {0, maxI}},
PlotStyle->ColorData[colorMap][id],
AspectRatio->1/8,
ImageSize->30cm]
]
] @@@Transpose[{Range[1, Length[lvls]], lvls}],
ListLinePlot[track[[3, All, 2]], PlotRange-> All,
AspectRatio->1/8, ImageSize->30cm]
]
]
];


plotTrackLevels[lvls_, track_, size_]:=Block[{maxI},
maxI = Max[track[[3, All, 2]]];
Show[
Prepend[
Function[{id, lvl},Block[{m},
m = Mean[Flatten[track[[3,#1;;#2,2]]&@@@lvl]];
ListLinePlot[{{#1, m}, {#2, m}}&@@@lvl, 
PlotRange-> {All, {0, maxI}},
PlotStyle->ColorData[colorMap][id],
AspectRatio->1/8,
ImageSize->30cm]
]
] @@@Transpose[{Range[1, Length[lvls]], lvls}],
ListLinePlot[track[[3, All, 2]], PlotRange-> All,
AspectRatio->1/8, ImageSize->size]
]
]
];
