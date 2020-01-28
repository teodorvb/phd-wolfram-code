#!/usr/local/bin/wolframscript
(* ::Package:: *)

<<"helpers.wls"


fI = 80;
fB = 30;


If[Length[$ScriptCommandLine] < 2, 
  Print["Need to specify the first batchid"];
  Exit[]
]


batches = 10;
round = 0.001;
firstBatchId = ToExpression[$ScriptCommandLine[[2]]];

(* StepRatio *)

(* In this simulation the ratio between level 2 and level 1 is different from 2.0 *)
ndata = addNoise@Round[#, round]&@Table[
  Block[{
    trackNum = 3,
    trackDist = 25,
    separations = N@{{5nm, 10nm, 15nm}, {20nm, 25nm, 30nm}, {35nm, 40nm, 45nm}},
    fIs,
    bothOn,
    oneOn,
    noOn
    },
    
    fIs = Flatten[{fI, fI * #}&/@Join[
      RandomReal[NormalDistribution[0.5, 0.1], 4],
      RandomReal[NormalDistribution[1.7, 0.1], 5]
      ]
    ];
    
    bothOn = GenerateFrame[Flatten[Table[{1, 1}, trackNum^2]],fIs,
      Flatten[Table[{
        {x*trackDist, y*trackDist}, 
        {x*trackDist+ separations[[x, y]], y*trackDist}
        },
        {x, 1, trackNum},{y, 1, trackNum}], 2], 
      fB, frameSize];
    
    oneOn = GenerateFrame[Flatten[Table[{1, 0}, trackNum^2]],fIs,
      Flatten[Table[{
        {x*trackDist, y*trackDist}, 
        {x*trackDist+ separations[[x, y]], y*trackDist}
        },
        {x, 1, trackNum},{y, 1, trackNum}], 2], 
      fB, frameSize];

    noOn = GenerateFrame[Flatten[Table[{0, 0}, trackNum^2]],fIs,
      Flatten[Table[{
        {x*trackDist, y*trackDist}, 
        {x*trackDist+ separations[[x, y]], y*trackDist}
        },
        {x, 1, trackNum},{y, 1, trackNum}], 2], 
      fB, frameSize];
    
    Join[Table[bothOn, 100], Table[oneOn, 100], Table[noOn, 20]]
],batches];

exportSimulatedData["na-stepratio-batch_", firstBatchId, ndata];



(* Additional Fluorophore 1 *)
(* Level 2 is first and Level 1 is last. This data set has a very dim
 * additional fluorophore in level 1, which is active for few more 
 * frames after the end of level 1 *)
 
ndata= addNoise@Round[#, round]&@Table[
Quiet@Block[{
fnum = 3,
fInt = 80,
fAddInt,
fDist,
fSep = 10nm,
fOn, fI, fP,
on3, on2, on1, on0},
fDist =N@ frameSize/(fnum+1);
fAddInt = RandomReal[UniformDistribution[{20, 40}], fnum^2];
fI = Flatten@Table[{fInt, fInt, fAddInt[[i]]}, {i, 1, fnum^2}];
fP = Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + fSep/2, y*fDist}
}, {x, 1, fnum}, {y, 1, fnum}], 2];
on3= GenerateFrame[Flatten@Table[{1, 1, 1}, fnum^2],fI,fP, fB,frameSize];
on2 = GenerateFrame[Flatten@Table[{1, 0, 1}, fnum^2],fI,fP, fB,frameSize];
on1 = GenerateFrame[Flatten@Table[{0, 0, 1}, fnum^2],fI,fP, fB,frameSize];
on0 = GenerateFrame[Flatten@Table[{0, 0, 0}, fnum^2],fI,fP, fB,frameSize];
Join[Table[on3, 100], Table[on2, 100], Table[on1, 3], Table[on0, 20]]
], batches];

exportSimulatedData["na-add-flfr1-batch_", firstBatchId, ndata];


(* Additional Fluorophore 2 *)
(* Level 1 is first and Level 2 is last. This data set has a very dim 
 * additional fluorophore in level 1, which is active for few more frames
 * before the beginning of level 1.
 *)
 
ndata= addNoise@Round[#, round]&@Table[
Quiet@Block[{
fnum = 3,
fInt = 80,
fAddInt,
fDist,
fSep = 10nm,
fOn, fI, fP,
on3, on2, on1, on0},
fAddInt = RandomReal[UniformDistribution[{20, 40}], fnum^2];
fDist =N@ frameSize/(fnum+1);
fI = Flatten@Table[{fInt, fInt, fAddInt[[i]]}, {i, 1, fnum^2}];
fP = Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + fSep/2, y*fDist}
}, {x, 1, fnum}, {y, 1, fnum}], 2];
on3= GenerateFrame[Flatten@Table[{1, 1, 1}, fnum^2],fI,fP, fB,frameSize];
on2 = GenerateFrame[Flatten@Table[{1, 0, 1}, fnum^2],fI,fP,fB,frameSize];
on1 = GenerateFrame[Flatten@Table[{0, 0, 1}, fnum^2],fI,fP,fB,frameSize];
on0 = GenerateFrame[Flatten@Table[{0, 0, 0}, fnum^2],fI,fP,fB,frameSize];
Join[Table[on0, 20], Table[on1,3], Table[on2, 100], Table[on3, 100]]
], 
batches];

exportSimulatedData["na-add-flfr2-batch_", firstBatchId, ndata];


(* Extrapolated Intensity 1 *)
(* Level 2 is first and Level 1 is last. Intensity after level 1 for 4 tracks
 * is higher than 0. There is additional undetected fluorophore (Possibly very
 * dim). Intensity after level 1 for 5 tracks is lower than 0. There is a 
 * fluorophore in proximity to the feature.
 *)
ndata= addNoise@Round[#, round]&@Table[
Quiet@Block[{
fnum = 3,
fInt = 80,
fDist,
fSep = 10nm,
fOn, fI, fP,
on3, on2, on1, on0,
fIntAdd1, fIntAdd2},
fDist =N@ frameSize/(fnum+1);
fIntAdd1 = RandomReal[UniformDistribution[{5,7}], 4];
fIntAdd2 = RandomReal[UniformDistribution[{80, 90}], 5];
fI = Join[
Flatten@Table[{fInt, fInt, fIntAdd1[[i]]},{i, 1, 4}],
Flatten@Table[{fInt, fInt, fIntAdd2[[i]]},{i, 1, 5}]
];
fP = N@Join[
Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + fSep/2, y*fDist}
}, {x, 1, fnum}, {y, 1, 1}], 2],

Flatten[ {
{{25, 50}, {25 + fSep, 50}, {25 + fSep/2, 50}},
{{50, 50}, {50 + fSep, 50}, {54, 54}},
{{75, 50}, {75 + fSep, 50}, {79, 54}}
}, 1],

Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + 4, y*fDist +4}
}, {x, 1, fnum}, {y, 3, 3}], 2]
];

on3= GenerateFrame[Flatten@Table[{1, 1, 1}, fnum^2],fI,fP, 30, frameSize];
on2 = GenerateFrame[Flatten@Table[{1, 0, 1}, fnum^2],fI,fP,30, frameSize];
on1 = GenerateFrame[Flatten@Table[{0, 0, 1}, fnum^2],fI,fP,30, frameSize];
on0 = GenerateFrame[Flatten@Table[{0, 0, 0}, fnum^2],fI,fP,30, frameSize];
Join[Table[on3, 100],Table[on2, 100], Table[on1, 20],Table[on0, 20]]
], batches];

exportSimulatedData["na-extr-int1-batch_", firstBatchId, ndata];


(* Extrapolated Intensity 2 *)
(* Level 1 is first and Level 2 is last. Intensity before level 1 for 4 tracks
 * is higher than 0. There is additional undetected fluorophore (Possibly very
 * dim). Intensity before level 1 for 5 tracks is lower than 0. There is a
 * fluorophore in proximity to the feature.
 *)
ndata= addNoise@Round[#, round]&@Table[
Quiet@Block[{
fnum = 3,
fInt = 80,
fDist,
fSep = 10nm,
fOn, fI, fP,
on3, on2, on1, on0,
fIntAdd1, fIntAdd2},
fDist =N@ frameSize/(fnum+1);
fIntAdd1 = RandomReal[UniformDistribution[{5,7}], 4];
fIntAdd2 = RandomReal[UniformDistribution[{50, 70}], 5];
fI = Join[
Flatten@Table[{fInt, fInt, fIntAdd1[[i]]},{i, 1, 4}],
Flatten@Table[{fInt, fInt, fIntAdd2[[i]]},{i, 1, 5}]
];
fP = N@Join[
Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + fSep/2, y*fDist}
}, {x, 1, fnum}, {y, 1, 1}], 2],

Flatten[ {
{{25, 50}, {25 + fSep, 50}, {25 + fSep/2, 50}},
{{50, 50}, {50 + fSep, 50}, {54, 54}},
{{75, 50}, {75 + fSep, 50}, {79, 54}}
}, 1],

Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + 4, y*fDist +4}
}, {x, 1, fnum}, {y, 3, 3}], 2]
];

on3= GenerateFrame[Flatten@Table[{1, 1, 1}, fnum^2],fI,fP, 30, frameSize];
on2 = GenerateFrame[Flatten@Table[{1, 0, 1}, fnum^2],fI,fP, 30, frameSize];
on1 = GenerateFrame[Flatten@Table[{0, 0, 1}, fnum^2],fI,fP, 30, frameSize];
on0 = GenerateFrame[Flatten@Table[{0, 0, 0}, fnum^2],fI,fP, 30, frameSize];
Join[Table[on0, 20], Table[on1,20], Table[on2, 100], Table[on3, 100]]
],batches];

exportSimulatedData["na-extr-int2-batch_",firstBatchId, ndata];


(* Interruptions *)
(* Track has too many interruptions. *)
states = {};

ndata = addNoise@Round[#, round]&@Table[
Quiet@Block[{
trackNum = 3,
trackDist,
on11,on10, on01, on00,
fP
},
trackDist = frameSize/(trackNum + 1);
fP = Flatten[Table[{
{x*trackDist, y*trackDist}, 
{x*trackDist+10nm, y*trackDist}
},{x, 1, trackNum},{y, 1, trackNum}], 2];

on11 = GenerateFrame[
Flatten[Table[{1, 1}, trackNum^2]],
Flatten[Table[{fI, fI}, trackNum^2]],
fP,fB, frameSize];

on10 = GenerateFrame[
Flatten[Table[{1, 0}, trackNum^2]],
Flatten[Table[{fI, fI}, trackNum^2]],
fP,fB, frameSize];

on01 = GenerateFrame[
Flatten[Table[{0, 1}, trackNum^2]],
Flatten[Table[{fI, fI}, trackNum^2]],
fP,fB, frameSize];

on00= GenerateFrame[
Flatten[Table[{0, 0}, trackNum^2]],
Flatten[Table[{fI, fI}, trackNum^2]],
fP,fB, frameSize];

states = Join[
FluorophoresHMM[400, {{0.95, 0.045, 0.005}, {0.2, 0.8, 0}, {0, 0, 1}}],
Table[{0, 0}, 20]
];
Switch[#, {1, 1}, on11, {1, 0}, on10, {0, 1}, on01, {0, 0}, on00]&/@states
],
batches];

exportSimulatedData["na-interruptions-batch_",firstBatchId, ndata];


(* Level 1 2 Gap Intensity *)
(* Has intensity in the gap between level 1 and level 2 which is different 
 * from the levels. Have 3 tracks with intermediate intensity, 3 with lower
 * than level 1 and 3 with higher than level 2.
 *)
ndata = addNoise@Round[#, round]&@Table[
Quiet@Block[{
fnum = 3,
fDist = 25,
fSep = 10nm,
fOn, fI, fP,
level2, btwLevels, level1, noLevel,
fAdd1, fAdd2, fAdd3},
fAdd1 = RandomReal[NormalDistribution[320, 10], 3];
fAdd2 = RandomReal[NormalDistribution[80, 10], 3];
fAdd3 = RandomReal[NormalDistribution[40, 5], 3];
fI = Flatten@Join[{
Table[{160, 160, fAdd1[[i]]},{i, 1,3}],
Table[{160, 160,  fAdd2[[i]]},{i, 1,3}],
Table[{160, 160,  fAdd3[[i]]}, {i, 1,3}]
}];
fP = Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist+fSep, y*fDist},
{x*fDist + fSep/2, y*fDist}},
 {x, 1, fnum}, {y, 1, fnum}], 2];
level2 = GenerateFrame[Flatten@Table[{1, 1, 0},9],
fI, fP, 30, frameSize];

btwLevels = GenerateFrame[Flatten@Join[
Table[{0,1,1},6],
Table[{0,0,1}, 3]
], fI, fP, 30, frameSize];
level1 = GenerateFrame[Flatten[Table[{0, 1, 0}, 9]],
fI, fP, 30, frameSize];

noLevel = GenerateFrame[Flatten@Table[{0,0,0}, 9], fI, fP, 30, frameSize];

Join[Table[level2, 100], Table[btwLevels, 3], Table[level1, 100], Table[noLevel, 20] ]
], batches];

exportSimulatedData["na-lvl12-gap-batch_", firstBatchId, ndata];


(* Neighbour *)
(* There is a fluorophore in the neighbourhood of the diffraction limited spot. *)

ndata= addNoise@Round[#, round]&@Table[
Quiet@Block[{
fnum = 3,
fInt = 80,
fDist,
fSep = 10nm,
fOn, fI, fP,
on3, on2, on1, on0, fAddI,
angle},

fDist =N@ frameSize/(fnum+1);
fAddI = RandomReal[NormalDistribution[60, 10],fnum^2];
angle = RandomReal[UniformDistribution[{0, 2*Pi}], fnum^2];
fI = Flatten@Table[{fInt, fInt, fAddI[[i]]},{i, 1, fnum^2}];

fP = N@Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + 5*Cos[angle[[(x-1)*fnum + y]] ], y*fDist +5*Sin[angle[[(x-1)*fnum + y]]]}
}, {x, 1, fnum}, {y, 1, fnum}], 2];

on3= GenerateFrame[Flatten@Table[{1, 1, 1}, fnum^2],fI,fP, 30, frameSize];
on2 = GenerateFrame[Flatten@Table[{1, 0, 1}, fnum^2],fI,fP, 30, frameSize];
on0 = GenerateFrame[Flatten@Table[{0, 0, 0}, fnum^2],fI,fP, 30, frameSize];
Join[ Table[on3, 100],Table[on2, 100], Table[on0, 20]]
], batches];

exportSimulatedData["na-neighbour-batch_", firstBatchId, ndata];


(* Position Shift *)
(* There is a change in the position during level 1. *)

ndata= addNoise@Round[#, round]&@Table[
Quiet@Block[{
fnum = 3,
fInt = 80,
fDist,
fSep = 10nm,
fOn, fI, fP1,fP2,
on2p1, on1p1, on1p2, on0,
offset, offsetI = 1},

fDist =N@ frameSize/(fnum+1);
fI = Join[
Flatten@Table[{fInt, fInt},fnum^2]
];
fP1 = N@Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist}
}, {x, 1, fnum}, {y, 1, fnum}], 2];
offset = {Cos[#2], Sin[#2]}*#1&@@@Transpose[{
RandomReal[NormalDistribution[80nm, 5nm], 9],
RandomReal[UniformDistribution[{0, 2*Pi}], 9]
}];

fP2 = N@Flatten[Table[{
{x*fDist,y*fDist} + offset[[offsetI++]],
{x*fDist + fSep, y*fDist}
}, {x, 1, fnum}, {y, 1, fnum}], 2];

on2p1 = GenerateFrame[Flatten@Table[{1, 1}, fnum^2],fI,fP1, fB, frameSize];

on1p1 = GenerateFrame[Flatten@Table[{1, 0}, fnum^2],fI,fP1, fB, frameSize];
on1p2 = GenerateFrame[Flatten@Table[{1, 0}, fnum^2],fI,fP2, fB, frameSize];

on0 = GenerateFrame[Flatten@Table[{0, 0}, fnum^2],fI,fP2, fB, frameSize];

Join[ Table[on2p1, 100],Table[on1p1, 50], Table[on1p2,50], Table[on0, 20]]
],batches];

exportSimulatedData["na-position-shift-batch_", firstBatchId, ndata];
