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


TrackLevel[level_, track_, trackStart_]:= Flatten[Apply[track[[Range[#1-trackStart+1,#2-trackStart+1]]]&,level,{1}], 1];
firstFrameId[record_]:= First@First@record[[dataIntI]];


ExtractFramesAfter[record_, frames_]:=Block[{
  trackStart,
  trackEnd,
  firstFrameid,
  lastFrameid,
  track},
  
  track = record[[dataIntI]];
  trackStart = record[[trackStartI]];
  trackEnd = record[[trackEndI]];
  firstFrameid = track[[1, 1]];
  lastFrameid = track[[-1, 1]];
  
  track[[trackEnd- firstFrameid+3;;Min[trackEnd+3 + frames - firstFrameid,lastFrameid-firstFrameid+1 ], {1,2}]]
];

ExtractFramesBefore[record_, frames_]:=Block[{
  trackStart,
  trackEnd,
  firstFrameid,
  lastFrameid,
  track},
  
  track = record[[dataIntI]];
  trackStart = record[[trackStartI]];
  trackEnd = record[[trackEndI]];
  firstFrameid = track[[1, 1]];
  lastFrameid = track[[-1, 1]];
  
  track[[Max[trackStart- frames - firstFrameid,1 ];;trackStart-firstFrameid, {1,2}]]
];



FindStepL1Last[level1_, level2_]:= Block[{gapBegin, gapEnd},
  gapBegin = Max@Flatten@level2;
  gapEnd = Max@Flatten@level1;
  If[#1 < gapEnd && #1 >= gapBegin, gapEnd = #1]&@@@level1;
{gapBegin, gapEnd}
];

FindStepL1First[level1_, level2_]:= Block[{gapEnd, gapBegin},
  gapEnd = Min@Flatten@level2;
  gapBegin = Min@Flatten@level1;
  If[#2 > gapBegin && #2 <= gapEnd, gapBegin = #2] &@@@level1;
{gapBegin, gapEnd}]

FindL12Gap[record_,level1_, level2_]:= If[record[[levelsOrderI]] == "L1Last",
  FindStepL1Last[level1, level2],
  FindStepL1First[level1, level2]
];

FeatureShapeRatios[imgData_, firstFrameId_, r_, imgr_, lvl_]:=N@#1/#2&@@Eigenvalues@Covariance@Flatten[Table[{i, j}*#[[r+1+i, r+1+j]], {i, -r, r}, {j, -r,r}], 1]&@ArrayReshape[#, {2imgr+1, 2imgr+1}
][[imgr+1-r;;imgr+1+r,imgr+1-r;;imgr+1+r]]&/@ TrackLevel[lvl, imgData, firstFrameId];

smiBgspsf = 1.2;
smiBgq1I = Flatten@Table[If[# > 3.5smiBgspsf - Sqrt[2.] && # < 5.5smiBgspsf + Sqrt[2.]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6, ##&[]],
{p, -5, 0}, {q, -5, 0}];
smiBgq2I = Flatten@ Table[If[# > 3.5smiBgspsf - Sqrt[2.] && # < 5.5smiBgspsf + Sqrt[2.]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6, ##&[]],
{p, -5, 0}, {q, 1, 5}];

smiBgq3I = Flatten@Table[If[# > 3.5smiBgspsf - Sqrt[2.] && # < 5.5smiBgspsf + Sqrt[2.]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6, ##&[]],
{p, 1, 5}, {q, -5, 0}];
smiBgq4I = Flatten@Table[If[# > 3.5smiBgspsf - Sqrt[2.] && # < 5.5smiBgspsf + Sqrt[2.]&@Sqrt[p^2 + q^2],(p+5)11 + q + 6,##&[]],
{p, 1, 5}, {q, 1, 5}];
smiBgAll = Join[smiBgq1I, smiBgq2I, smiBgq3I, smiBgq4I];

SmiBG[seq_]:= Mean@Take[#, Min[Length@#,10]]&@Sort[Function[rawImg,
    4StandardDeviation[rawImg[[smiBgAll]]]/(
      StandardDeviation[rawImg[[smiBgq1I]]] + 
      StandardDeviation[rawImg[[smiBgq2I]]] +
      StandardDeviation[rawImg[[smiBgq3I]]] +
      StandardDeviation[rawImg[[smiBgq4I]]])
    ]/@ seq
   ];
   
posSTD[p_]:= Block[{m},
  m = Mean@p;
  N@Sqrt@Mean[ #.#&@(#-m) &/@p]
];
posSTDLoc[p_,w_]:= N@posSTD[p[[#-Floor[w/2];;#+Floor[w/2]]]] &/@ Range[Floor[w/2]+1, Length[p]-Floor[w/2]];


getLevel12[record_, levelsI_]:=If[record[[levelsOrderI]]== "L1First",
Sort[record[[levelsI]],Min@#1 < Min@#2&][[1;;2]],
Sort[record[[levelsI]],Max@#1 > Max@#2&][[1;;2]]]



(* Implemented Scores *)
level12Gap[record_, level1_, level2_]:=N[#2-#1-1]&@@FindL12Gap[record, level1, level2];

level12OffLevel[record_, level1_, level2_]:= Block[{
l1Mean,
l1Std,
l2Mean,
l2Std,
firstFrameid,
track},

track = record[[dataIntI]];
firstFrameid = track[[1, 1]];
{l1Mean, l1Std} = {Mean@#, StandardDeviation@#}&@TrackLevel[level1, track, firstFrameid][[All, 2]];
{l2Mean, l2Std} = {Mean@#, StandardDeviation@#}&@TrackLevel[level2, track, firstFrameid][[All, 2]];
If[level12Gap[record, level1, level2] == 0,
  {0., 0., 0.},
  {Min@#, Max@#, Mean@#}&@( Min/@Transpose[{
    Abs@(# - l1Mean)/l1Std,
    Abs@(# - l2Mean)/l2Std
    }] &@ track[[#1-firstFrameid+2;;#2-firstFrameid]][[All,2]]&@@FindL12Gap[record, level1, level2])
  ]
];

intBeforeAfterL1[record_, level_]:=Block[{
    firstFrameid,
    d,
    l1Mean,
    l1Std,
    track
  },

  track = record[[dataIntI]];
  firstFrameid = track[[1, 1]];
  {l1Mean, l1Std} = {Mean@#, StandardDeviation@#}&@ TrackLevel[level, track, firstFrameid][[All, 2]];
  If[Length@# == 0, {0., 0., 0.}, {Min@#, Max@#, Mean@#}]&@N@Abs[(#-l1Mean)]/l1Std&@ If[record[[levelsOrderI]] == "L1Last",
    track[[Max@Flatten@level +2 - firstFrameid ;; record[[trackEndI]] - firstFrameid+1, 2]],
    track[[record[[trackStartI]] - firstFrameid+1 ;;Min@Flatten@level -firstFrameid, 2]]
  ]
];

framesBeforeAfterL1[record_, level_]:= N@If[record[[levelsOrderI]]=="L1Last",
  record[[trackEndI]] - Max@Flatten@level,
  N@Min@Flatten@level - record[[trackStartI]]
];

countSigmaMLS[record_, level1_, level2_]:= Block[{i1, i2},
  i1 = TrackLevel[level1, record[[dataIntI, All, 2]], firstFrameId[record]];
  i2 = TrackLevel[level2, record[[dataIntI, All, 2]], firstFrameId[record]];
  Max[
    StandardDeviation[i1]/Mean[MovingMap[StandardDeviation, i1, 10]],
    StandardDeviation[i2]/Mean[MovingMap[StandardDeviation, i2, 10]]
  ]
];
secMostPoints[record_, level1_, level2_]:= Min[
  N@Length@# - Total[#]&@TrackLevel[level1, record[[dataIntI, All, 7]], firstFrameId[record]],
  N@Length@# - Total[#]&@TrackLevel[level2, record[[dataIntI, All, 7]], firstFrameId[record]]
];
secLongest[record_, level1_, level2_]:= Min[
  N@Total[#2- #1 + 1&@@@level1],
  N@Total[#2- #1 + 1&@@@level2]
];
posDensity[record_, level1_, level2_]:= Block[{p1, p2, m},
  p1 = TrackLevel[level1, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  p2 = TrackLevel[level2, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  m = Mean@Join[p1,p2];
  (
  N@Length@p1 - N@Total[TrackLevel[level1, record[[dataIntI, All, 7]], firstFrameId[record] ]] +
  N@Length@p2 - N@Total[TrackLevel[level2, record[[dataIntI, All, 7]], firstFrameId[record] ]]
  )/Max[Norm[#-m,2]&/@Join[p1,p2]]^2 Pi
]
posSigmaLocMean[record_,level1_,level2_]:= Block[{p1, p2},
  p1 = TrackLevel[level1, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  p2 = TrackLevel[level2, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  Max[posSTD[MovingAverage[p1, 10]], posSTD@MovingAverage[p2, 10]]
]
posSigmaMLS[record_, level1_, level2_]:=Block[{
p1, p2,
m},
	p1 = TrackLevel[level1, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
	p2 = TrackLevel[level2, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
	Max[posSTD[p1]/Mean[posSTDLoc[p1, 10]], posSTD[p2]/Mean[posSTDLoc[p2, 10]]]
];
ctsSLM[record_, level_]:= StandardDeviation@MovingAverage[TrackLevel[level, record[[dataIntI, All, 2]], firstFrameId[record]], 10];
ctsLocalSN[record_, level_]:= Mean@MovingMap[
  Mean@#/StandardDeviation@#&, 
  TrackLevel[level, record[[dataIntI, All, 2]], firstFrameId[record]],
  10
];

posError[record_, level_]:= Block[{l, p, m},
  p = TrackLevel[level, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  l = N@Length@p;
  m = N@Mean@p;
  Sqrt[1.0/l] Sqrt[1.0/l Total[ Total[(#-m)^2]&/@p ]]
]

semDR12[record_, level1_, level2_]:= Block[{ps1, ps2, meanP1, meanP2, varP1, varP2},
  ps1 = TrackLevel[level1, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  ps2 = TrackLevel[level2, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  
  meanP1 = Mean@ps1;
  meanP2 = Mean@ps2;
  varP1 = Variance[ps1]/Length[ps1];
  varP2 = Variance[ps2]/Length[ps2];
  Sqrt[(meanP1[[1]] - meanP2[[1]])^2*(varP1[[1]] + varP2[[1]]) + (meanP1[[2]] - meanP2[[2]])^2*(varP1[[2]] + varP2[[2]])]/DR12[record, level1, level2]
];

DR12[record_, level1_, level2_]:= Block[{p1, p2},
  p1 = Mean@TrackLevel[level1, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  p2 = Mean@TrackLevel[level2, record[[dataIntI, All, {4, 5}]], firstFrameId[record]];
  Sqrt[(p1[[1]] - p2[[1]])^2 + (p1[[2]] - p2[[2]])^2]
];

stepRatioLvl12[record_, level1_, level2_]:= Abs[Mean[TrackLevel[level2, record[[dataIntI, All, 2]], firstFrameId[record]]]/Mean[TrackLevel[level1, record[[dataIntI, All, 2]], firstFrameId[record]]] - 2];
trackClustering[record_, level1_, level2_]:=Block[{l1Int, l2Int, firstFrameid},
  firstFrameid = First@First@record[[dataIntI]];
  l1Int=TrackLevel[level1, record[[dataIntI, All, 2]], firstFrameid];
  l2Int=TrackLevel[level2, record[[dataIntI, All, 2]], firstFrameid];
  (Mean@l2Int - Mean@l1Int)/(StandardDeviation@l1Int + StandardDeviation@l2Int)
];

goesToZero[record_]:=Mean@#/StandardDeviation@#&@If[record[[levelsOrderI]] == "L1Last",
  ExtractFramesAfter[record, 10],
  ExtractFramesBefore[record, 10]
][[All, 2]];

ratioRealPoints[record_, level_]:= (Length@# - Total@#)/Length@# &@TrackLevel[level, record[[dataIntI, All, 7]], firstFrameId[record]];
fragmentation[record_, level_]:=N@Length[level]-1;
longestRange[record_, level_]:=N@Max[#2-#1+1&@@@level];
signalToNoise[record_, level_]:= Mean@#/StandardDeviation@#&@TrackLevel[level, record[[dataIntI]][[All, 2]], firstFrameId[record] ];
smiBackground[record_, level_]:= SmiBG[TrackLevel[level,record[[dataImgI]], firstFrameId[record] ]];
featureShape[record_, level_]:=Block[{firstfid,r,imgr},
  firstfid = First@First@record[[dataIntI]];
  imgr = (Sqrt@Length@First@record[[dataImgI]]-1)/2;
  r = 3;
  Mean@Take[#, Ceiling[0.1Length[#]] ] &@ Sort[FeatureShapeRatios[record[[dataImgI]], firstfid, r, imgr, level],Larger]
];
