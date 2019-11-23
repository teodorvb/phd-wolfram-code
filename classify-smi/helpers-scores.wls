#!/usr/bin/env wolframscript
(* ::Package:: *)

usedScores = {
"sec_longest",
"sec_most_pts",
"count_sigma_mls_12",
"cts_loc_sn_l1",
"cts_loc_sn_l2",
"cts_slm_l1",
"cts_slm_l2",
"stepratio_lvl12",
"dr12",
"pos_density_12",
"pos_error_1",
"pos_error_2",
"pos_sigma_mls_12",
"pos_sigma_locmean_12",
"semdr12"
};

ExtractScores[record_]:=
  Flatten[{
    N[record[[scoresI]]/@usedScores],
    Level12OffLevel[record],
    IntBeforeAfterL1[record],
    {GoesToZero[record],
     SmiBackgroundL1[record],
     SmiBackgroundL2[record],
     TrackClustering[record],
     FeatureShapeL1[record],
     FeatureShapeL2[record],
     FragmentationL1[record],
     FragmentationL2[record],
     LongestRangeL1[record],
     LongestRangeL2[record],
     Level12Gap[record],
     FramesBeforeAfterL1[record],
     (*RatioRealPointsL1[record],
     RatioRealPointsL2[record], *)
     SignalToNoiseL1[record],
     SignalToNoiseL2[record]}
  }] -> If[record[[categoryI]] == 1, yes, no];

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
