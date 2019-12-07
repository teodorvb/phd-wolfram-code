#!/usr/bin/env wolframscript
(* ::Package:: *)

ExtractScores[record_, levelsIndex_]:= Block[{level1, level2},
  {level1, level2} = getLevel12[record, levelsIndex];
  
  Flatten[{
    ctsSLM[record, level1],
    ctsSLM[record, level2],
    ctsLocalSN[record, level1],
    ctsLocalSN[record, level2],
    trackClustering[record, level1, level2],
    ratioRealPoints[record, level1],
    ratioRealPoints[record, level2],
    fragmentation[record, level1],
    fragmentation[record, level2],
    longestRange[record, level1],
    longestRange[record, level2],
    smiBackground[record, level1],
    smiBackground[record, level2],
    featureShape[record, level1],
    featureShape[record, level2],
    stepRatioLvl12[record, level1, level2],
    goesToZero[record],
    signalToNoise[record, level1],
    signalToNoise[record, level2],
    DR12[record, level1, level2],
    semDR12[record, level1, level2],
    posError[record, level1],
    posError[record, level2],
    posSigmaMLS[record, level1, level2],
    posSigmaLocMean[record, level1, level2],
    posDensity[record, level1, level2],
    secLongest[record, level1, level2],
    secMostPoints[record, level1, level2],
    countSigmaMLS[record, level1, level2],
    intBeforeAfterL1[record, level1],
    framesBeforeAfterL1[record, level1],
    level12Gap[record, level1, level2],
    level12OffLevel[record, level1, level2]
  }] -> If[record[[categoryI]] == 1, yes, no]  
];

ExtractIntTrackExtrapolated[record_]:=Block[{
  meanIl1,
  meanXl1,
  meanYl1,
  res,
  lf,
  ts,
  te,
  ff
  },
  ff = First@First@record[[dataIntI]];
  lf = First@Last@record[[dataIntI]];
  ts = record[[trackStartI]];
  te = record[[trackEndI]];

  {meanIl1,meanXl1,meanYl1} =Mean/@Transpose[
    TrackLevel[record[[levelsOldI]][[1]],record[[dataIntI]],First[First[record[[dataIntI]]]]][[All, {2, 4, 5}]]
  ];
Transpose[({#2/meanIl1,
Join[
Table[0,ts - ff],
Norm/@Transpose[{
#4[[ts -ff+1;;te -ff +1]],
#5[[ts -ff+1;;te -ff +1]]
} - {meanXl1, meanYl1}],
Table[0, lf-te]
]
}&)@@Transpose[record[[dataIntI]]]] -> If[record[[categoryI]] == 1, yes, no]

];

ExtractIntTrackL12Cropped[record_]:=Block[{
  meanIl1,
  meanXl1,
  meanYl1,
  res,
  lf,
  ts,
  te,
  ff,
croppedStart,
croppedEnd,
track,
trackEnd,
trackStart
},
  ff = First@First@record[[dataIntI]];
  lf = First@Last@record[[dataIntI]];
  ts = record[[trackStartI]];
  te = record[[trackEndI]];
trackEnd = te - ff+1;
trackStart = ts - ff +1;
If[record[[levelsOrderI]]== "L1First",
croppedStart = Max[ts - 20, ff] -ff + 1;
croppedEnd = Max@Flatten@record[[levelsOldI]][[2]] - ff+1,

croppedStart = Min@Flatten@record[[levelsOldI]][[2]]-ff + 1;
croppedEnd = Min[te + 20, lf] -ff + 1
];

{meanIl1,meanXl1,meanYl1} =Mean/@Transpose[
    TrackLevel[record[[levelsOldI]][[1]],record[[dataIntI]],ff][[All, {2, 4, 5}]]
];
track = ({#2/meanIl1,Norm/@Transpose[{#4,#5} - {meanXl1, meanYl1}]
}&)@@Transpose[record[[dataIntI]][[croppedStart;;croppedEnd]]];
If[record[[levelsOrderI]] == "L1First",
If[trackStart - croppedStart>= 1,track[[2,1;;trackStart - croppedStart]]=0],
If[ trackEnd - croppedStart+2<=croppedEnd,track[[2, trackEnd - croppedStart+2;;-1]]=0]
];
Transpose[track] -> If[record[[categoryI]] == 1, yes, no]
];


minLevelLength[levels_]:= Min[Total[#2 - #1 + 1&@@@#]&/@levels]
