#!/usr/bin/env wolframscript
(* ::Package:: *)

datasetNameI = 1;
trackIdI = 2;
trackStartI = 3;
trackEndI = 4;
categoryI = 5;
levelsOrderI = 6;
scoresI = 7;
levelsI = 8;
dataIntI = 9;
dataImgI = 10;
levelsOldI = 11;


Needs["DatabaseLink`"];


TrackLevel[level_, track_, trackStart_]:= Flatten[Apply[track[[Range[#1-trackStart+1,#2-trackStart+1]]]&,level,{1}], 1];


ExtractFramesAfter[record_, frames_]:=Block[{
  type,
  trackStart,
  trackEnd,
  firstFrameid,
  lastFrameid,
  track},
  
  track = record[[dataIntI]];
  type = record[[levelsOrderI]];
  trackStart = record[[trackStartI]];
  trackEnd = record[[trackEndI]];
  firstFrameid = track[[1, 1]];
  lastFrameid = track[[-1, 1]];
  
  track[[trackEnd- firstFrameid+3;;Min[trackEnd+3 + frames - firstFrameid,lastFrameid-firstFrameid+1 ], {1,2}]]
];

ExtractFramesBefore[record_, frames_]:=Block[{
  type,
  trackStart,
  trackEnd,
  firstFrameid,
  lastFrameid,
  track},
  
  track = record[[dataIntI]];
  type = record[[levelsOrderI]];
  trackStart = record[[trackStartI]];
  trackEnd = record[[trackEndI]];
  firstFrameid = track[[1, 1]];
  lastFrameid = track[[-1, 1]];
  
  track[[Max[trackStart- frames - firstFrameid,1 ];;trackStart-firstFrameid, {1,2}]]
];



FindStepL1Last[levels_]:= Function[ranges,Block[{t},
  t = Length[ranges];
  While[ranges[[t, 1]]== ranges[[t-1, 1]]&& t >1, t--];
  N@{ranges[[t-1, 2,2]], ranges[[t, 2, 1]]}
  ]
]@SortBy[Flatten[Transpose@{Table[#1, Length[#2]], #2}&@@@Transpose@{Range[1, Length[levels]],levels}, 1], #[[2, 1]]&];

FindStepL1First[levels_]:= Function[ranges,Block[{t},
  t = 1;
  While[ranges[[t, 1]]== ranges[[t+1, 1]]&& t <Length[ranges], t++];
  N@{ranges[[t, 2,2]], ranges[[t+1, 2, 1]]}
  ]
]@SortBy[Flatten[Transpose@{Table[#1, Length[#2]], #2}&@@@Transpose@{Range[1, Length[levels]],levels}, 1], #[[2, 1]]&];

FindL12Gap[record_]:= If[record[[levelsOrderI]] == "L1Last",
  FindStepL1Last[record[[levelsI]]],
  FindStepL1First[record[[levelsI]]]
];

FeatureShapeRatios[imgData_, firstFrameId_, r_, imgr_, lvl_]:=N@#1/#2&@@Eigenvalues@Covariance@Flatten[Table[{i, j}*#[[r+1+i, r+1+j]], {i, -r, r}, {j, -r,r}], 1]&@ArrayReshape[#, {2imgr+1, 2imgr+1}
][[imgr+1-r;;imgr+1+r,imgr+1-r;;imgr+1+r]]&/@ TrackLevel[lvl, imgData, firstFrameId];

FeatureShape[record_, lvl_]:=Block[{firstfid,r,imgr},
  firstfid = First@First@record[[dataIntI]];
  imgr = (Sqrt@Length@First@record[[dataImgI]]-1)/2;
  r = 3;
  Mean@Take[#, Floor[0.1Length[#]] ] &@ Sort[FeatureShapeRatios[record[[dataImgI]], firstfid, r, imgr, record[[levelsI]][[lvl]]],Larger]
];

smiBgspsf = 1.2;
smiBgq1I = Flatten@Table[If[# > 3.5smiBgspsf - Sqrt[2] && # < 5.5smiBgspsf + Sqrt[2]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6, ##&[]],
{p, -5, 0}, {q, -5, 0}];
smiBgq2I = Flatten@ Table[If[# > 3.5smiBgspsf - Sqrt[2] && # < 5.5smiBgspsf + Sqrt[2]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6, ##&[]],
{p, -5, 0}, {q, 1, 5}];

smiBgq3I = Flatten@Table[If[# > 3.5smiBgspsf - Sqrt[2] && # < 5.5smiBgspsf + Sqrt[2]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6, ##&[]],
{p, 1, 5}, {q, -5, 0}];
smiBgq4I = Flatten@Table[If[# > 3.5smiBgspsf - Sqrt[2] && # < 5.5smiBgspsf + Sqrt[2]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6,##&[]],
{p, 1, 5}, {q, 1, 5}];
smiBgAll = Join[smiBgq1I, smiBgq2I, smiBgq3I, smiBgq4I];

SmiBackground[seq_]:= Mean@Take[#, 10]&@Sort[Function[rawImg,
    4StandardDeviation[rawImg[[smiBgAll]]]/(
      StandardDeviation[rawImg[[smiBgq1I]]] + 
      StandardDeviation[rawImg[[smiBgq2I]]] +
      StandardDeviation[rawImg[[smiBgq3I]]] +
      StandardDeviation[rawImg[[smiBgq4I]]])
    ]/@ seq
   ];


FramesBeforeAfterL1[record_]:= N@If[record[[levelsOrderI]]=="L1Last",
  record[[trackEndI]] - Max@Flatten@First@record[[levelsI]],
  Min@Flatten@First@record[[levelsI]] - record[[trackStartI]]
];

FeatureShapeL1[record_]:= FeatureShape[record, 1];
FeatureShapeL2[record_]:= FeatureShape[record, 2];

SmiBackgroundL1[record_]:= SmiBackground[TrackLevel[record[[levelsI]][[1]],record[[dataImgI]], First@First@record[[dataIntI]] ]];
SmiBackgroundL2[record_]:= SmiBackground[TrackLevel[record[[levelsI]][[2]],record[[dataImgI]], First@First@record[[dataIntI]] ]];

TrackClustering[record_]:=Block[{l1Int, l2Int, firstFrameid},
  firstFrameid = First@First@record[[dataIntI]];
  l1Int=TrackLevel[record[[levelsI]][[1]],record[[dataIntI]], firstFrameid][[All, 2]];
  l2Int=TrackLevel[record[[levelsI]][[2]],record[[dataIntI]], firstFrameid][[All, 2]];
  (Mean@l2Int - Mean@l1Int)/(StandardDeviation@l1Int + StandardDeviation@l2Int)
];

Level12Gap[record_]:=#2-#1-1&@@FindL12Gap[record];

Level12OffLevel[record_]:= Block[{
l1Mean,
l1Std,
l2Mean,
l2Std,
firstFrameid,
track,
levels},

track = record[[dataIntI]];
firstFrameid = track[[1, 1]];
levels = record[[levelsI]];
{l1Mean, l1Std} = {Mean@#, StandardDeviation@#}&@TrackLevel[levels[[1]], track, firstFrameid][[All, 2]];
{l2Mean, l2Std} = {Mean@#, StandardDeviation@#}&@TrackLevel[levels[[2]], track, firstFrameid][[All, 2]];
If[Level12Gap[record] == 0,
  {0., 0., 0.},
  {Min@#, Max@#, Mean@#}&@( Min/@Transpose[{
    Abs@(# - l1Mean)/l1Std,
    Abs@(# - l2Mean)/l2Std
    }] &@ track[[#1-firstFrameid+2;;#2-firstFrameid]][[All,2]]&@@FindL12Gap[record])
  ]
];

GoesToZero[record_]:=Mean@#/StandardDeviation@#&@If[record[[levelsOrderI]] == "L1Last",
  ExtractFramesAfter[record, 10],
  ExtractFramesBefore[record, 10]
][[All, 2]];

IntBeforeAfterL1[record_]:=Block[{
    firstFrameid,
    d,
    l1Mean,
    l1Std,
    levels,
    track
  },

  levels = record[[levelsI]];
  track = record[[dataIntI]];
  firstFrameid = track[[1, 1]];
  {l1Mean, l1Std} = {Mean@#, StandardDeviation@#}&@ TrackLevel[levels[[1]], track, firstFrameid][[All, 2]];
  If[Length@# == 0, {0., 0., 0.}, {Min@#, Max@#, Mean@#}]&@Abs[(#-l1Mean)]/l1Std&@ If[record[[levelsOrderI]] == "L1Last",
    track[[Max@Flatten@First@levels +2 - firstFrameid ;; record[[trackEndI]] - firstFrameid+1, 2]],
    track[[record[[trackStartI]] - firstFrameid+1 ;;Min@Flatten@First@levels -firstFrameid, 2]]
  ]
];

RatioRealPointsL1[record_]:= N[#1["level1points"]/ #1["level1duration"]&@record[[scoresI]]];
RatioRealPointsL2[record_]:= N[#1["level2points"]/ #1["level2duration"]&@record[[scoresI]]];

FragmentationL1[record_]:=N@Length[record[[levelsI]][[1]]]-1;
FragmentationL2[record_]:=N@Length[record[[levelsI]][[2]]]-1;

LongestRangeL1[record_]:=N@Max[#2-#1+1&@@@record[[levelsI]][[1]]];
LongestRangeL2[record_]:=N@Max[#2-#1+1&@@@record[[levelsI]][[2]]];

SignalToNoiseL1[record_]:= Mean@#/StandardDeviation@#&@TrackLevel[record[[levelsI]][[1]], record[[dataIntI]][[All, 2]], First@First@record[[dataIntI]]];
SignalToNoiseL2[record_]:= Mean@#/StandardDeviation@#&@TrackLevel[record[[levelsI]][[2]], record[[dataIntI]][[All, 2]], First@First@record[[dataIntI]]];
