#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Data Labelling Helpers *)
(* transform[dp_]:=(dp - Mean[dp])/StandardDeviation[dp]; *)
transform[dp_]:= Flatten[dp];
 
labelMCgetStates[track_]:=
BinaryDeserialize@Get@FileNameJoin[{
  "..",
  "simulations",
  "mc-states.bin",
  StringTake[StringSplit[track[[datasetNameI]], "/"][[-1]],{1,-38}]
  }];

labelMCstatesToSteps[states_]:= Block[{steps= {},i},
  For[i = 1, i <= Length@states - 1, i++,
    If[states[[i]]!= states[[i+1]], AppendTo[steps, i]]
  ];
  Append[steps, Length@states]
];

labelMCgetSteps[track_]:= labelMCstatesToSteps[labelMCgetStates[track]];

labelMC[tracks_, hw_]:= Block[{},
  Flatten[
    Function[{d, steps},
     (transform[d[[#1-hw;;#1+hw]]]->If[Count[steps,#1]==0,no,yes]&)/@Range[hw+1,Length[d]-hw]
    ]@@@({#1[[dataIntI]][[All,counts]],labelMCgetSteps[#1]}&)/@tracks,
  1]
];


(*getSteps[network_, track_]:= Function[{d}, Block[{steps = {}},
If[network[Values@#]===yes, AppendTo[steps, Keys@#]]&/@(#-> d[[#-hw;;#+hw]] &/@ Range[hw+1, Length[d]-hw]);
steps
]
]@track[[dataIntI]][[All,counts]];
*)


getSteps[network_, track_]:= Function[{d}, Block[{steps = {}},
If[#1[yes]/#1[no]> 0.01 &@network[Values@#, "Probabilities"], AppendTo[steps, Keys@#]]&/@(#-> d[[#-hw;;#+hw]] &/@ Range[hw+1, Length[d]-hw]);
steps
]
]@track[[dataIntI]][[All,counts]];


stepsToSegments[steps_, track_]:= Block[{i, ranges = {}},
Select[
  If[Length[steps] > 0,
    AppendTo[ranges, {1, steps[[1]]} ];
    For[i =2, i<= Length[steps], i++, 
      AppendTo[ranges, {steps[[i-1]]+1, steps[[i]]}]
    ];
    AppendTo[ranges, {steps[[-1]]+1, Length[track[[dataIntI]] ]}];
    ranges,
    {{1, Length[track[[dataIntI]] ]}}
  ], 

  #[[2]] - #[[1]] > 1&
]
];


shouldMerge[lvl1_, lvl2_, track_]:= Block[{c1, c2},
  c1 = track[[dataIntI]][[lvl1[[1]];;lvl1[[2]], counts]];
  c2 = track[[dataIntI]][[lvl2[[1]];;lvl2[[2]], counts]];
  (*lvl1[[2]]+1== lvl2[[1]]&&Abs[Mean@c1 - Mean@c2]<0.5(StandardDeviation@c1 + StandardDeviation@c2) *)
  lvl1[[2]]+1== lvl2[[1]] && KolmogorovSmirnovTest[c1, c2] > 0.05
];


mergeSegments[lvl_, track_]:=Block[{i, newLevels, oldLevels, changed},
oldLevels = lvl;
newLevels = {};

changed = True;
While[changed,
changed = False;
For[i = 1, i <= Length[oldLevels]-1, i++,
If[shouldMerge[oldLevels[[i]], oldLevels[[i+1]], track],
AppendTo[newLevels, {oldLevels[[i,1]], oldLevels[[i+1, 2]]}];
newLevels = Join[newLevels, oldLevels[[i+2;;-1]]];
changed = True;
Break[],
AppendTo[newLevels, oldLevels[[i]]];
];
];

If[!changed, AppendTo[newLevels, oldLevels[[-1]] ]];

oldLevels = newLevels;
newLevels = {};
];
oldLevels
];


isNotZero[list_]:= Mean[list]/StandardDeviation[list]> 2;
getSegments[network_, track_]:= Block[{steps,i, ranges = {}},
 SortBy[
  Select[
    stepsToSegments[getSteps[network, track], track],
    isNotZero[track[[dataIntI]][[#1;;#2,2]]]&@@#&
  ],
  Mean[track[[dataIntI]][[#1;;#2,2]]]&@@#&
 ]
]


getLevelCounts[lvl_, track_]:= Flatten[track[[dataIntI, #1;;#2,counts]]&@@@lvl];


(*shouldMergeProb[c1_, c2_]:= Erf[Abs[Mean[c1] - Mean[c2]]/Sqrt[2*(Variance[c1] + Variance[c2])]]; *)
shouldMergeProb[c1_, c2_]:= KolmogorovSmirnovTest[c1, c2];


segmentsToLevels[sseg_, track_]:= Block[{lvls, i,j, changed, seg,
 mp, maxProb, mergeI, mergeJ},
  seg = sseg;
  lvls = {};
  
  While[Length[seg] > 0,
    AppendTo[lvls,{seg[[1]]}];
    seg = Delete[seg, 1];
    changed = True;
    While[changed && Length[seg]>0,
      changed = False;
      maxProb = shouldMergeProb[getLevelCounts[lvls[[-1]], track], getLevelCounts[{seg[[1]]}, track] ];
      mergeI = 1;
      For[i = 2, i <= Length[seg], i++,
        mp = shouldMergeProb[getLevelCounts[lvls[[-1]], track], getLevelCounts[{seg[[i]]}, track] ];
        If[maxProb < mp,
          maxProb = mp;
          mergeI = i]
      ];
      
      If[maxProb > 0.05,
        AppendTo[lvls[[-1]], seg[[mergeI]]];
        seg = Delete[seg, mergeI];
        changed = True;
      ]
    ]
  ];
  lvls
]


getLevels[network_, track_]:= SortBy[
  segmentsToLevels[getSegments[network, track],track],
  levelInt[track, #]&
];


levelInt[track_, lvl_]:= Mean@getLevelCounts[lvl, track];
findFrame[frame_,lvls_]:= Block[{levelId = 0},
Function[{id, lvl},
If[AnyTrue[lvl,#1[[1]] <= frame<= #1[[2]] &], levelId = id]
]@@@Transpose[{Range[1, Length[lvls]],lvls}];
levelId
]


(* Calculate Level Identification Accuracy *)
calcAcc[track_, lvls_]:=Block[{actual,predicted, states, 
lvl2intActual,
lvl2intPredicted,
levels},

states = labelMCgetStates[track];

lvl2intActual=<|
#1-> #2&@@@Transpose[{Keys[#], Mean[#[[All, 1]]]&/@Values[#]}]&@ GroupBy[Transpose[{track[[dataIntI, 1;;Length@states, counts]], states}], #[[2]]&]
|>;

actual = lvl2intActual[#]&/@states;

lvl2intPredicted = <|
#1 -> levelInt[track, #2]& @@@ Transpose[{Range[1,Length[lvls ]], lvls}] 
|>;
lvl2intPredicted[0]= no; 
(*lvl2intPredicted[0]= Mean[track[[dataIntI, All, counts]]]; *)
predicted = lvl2intPredicted[findFrame[#1,SortBy[lvls,levelInt[track,#1]&]]]&/@ Range[1, Length@states];

{
Mean[
  Abs[#1 - #2]&@@@Select[Transpose[{actual, predicted}], !(#[[1]]=== no || #[[2]] ===no)&]
],
1 - N@Length@Select[Transpose[{actual, predicted}],
(#[[1]]=== no && #[[2]] ===no)||(!#[[1]]=== no &&!#[[2]] ===no)&] / Length[predicted]
}
];


calcAcc[track_, shiftlvls_, manualLvls_]:= Block[{manualLvlsSorted,
lvl2intManual,
lvl2intPredicted,
actual,
predicted,lvls},
lvls = SortBy[shiftlvls - 1,levelInt[track, #]&];
manualLvlsSorted = SortBy[manualLvls, levelInt[track, #]&];

lvl2intManual = <|
#1 -> levelInt[track, #2]& @@@ Transpose[{Range[1,Length[manualLvlsSorted]], manualLvlsSorted}] 
|>;
lvl2intManual[0]= no; 
actual = lvl2intManual[findFrame[#1,manualLvlsSorted]]&/@ Range[1, Length@track[[dataIntI]]];

lvl2intPredicted = <|
#1 -> levelInt[track, #2]& @@@ Transpose[{Range[1,Length[lvls ]], lvls}] 
|>;

lvl2intPredicted[0]= no;
predicted = lvl2intPredicted[findFrame[#1, lvls]]&/@ Range[1, Length@track[[dataIntI]]];

{
Mean[
Abs[#1 - #2]&@@@Select[Transpose[{actual, predicted}], !(#[[1]]=== no || #[[2]] ===no)&]
],
1 - N@Length@Select[Transpose[{actual, predicted}],
(#[[1]]=== no && #[[2]] ===no)||(!#[[1]]=== no &&!#[[2]] ===no)&] / Length[predicted]
}
]
