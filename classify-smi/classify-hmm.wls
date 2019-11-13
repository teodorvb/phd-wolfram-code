#!/usr/bin/env wolframscript
(* ::Package:: *)

Install["fit_hmm"];
unpackHMM[res_, dataDim_, hiddenStates_] := Block[{begin, end, pi, tr, em},
  begin = 1;
  end = hiddenStates;
  pi = res[[begin;;end]];
  begin = end +1;
  end = begin + hiddenStates^2 -1;tr = ArrayReshape[res[[begin;;end]], {hiddenStates, hiddenStates}];em = {};
  For[i = 0, i < hiddenStates, i++,
    begin = end +1;
    end = begin+(dataDim+1)dataDim-1;AppendTo[em, MultinormalDistribution[res[[begin;; begin+dataDim-1]], ArrayReshape[res[[begin+dataDim;; begin + dataDim +dataDim^2-1]], {dataDim, dataDim}]]]
    ];
  HiddenMarkovProcess[pi,  #/Total[#]&/@(Transpose@tr), em]
];

getLabels[data_, modelY_, modelN_, priorY_, priorN_]:= Block[{logPriorY, logPriorN},
logPriorY = Log[priorY];
logPriorN = Log[priorN];
If[LogLikelihood[modelY, #] + logPriorY > LogLikelihood[modelN, #] + logPriorN, yes, no]&/@data
];
f1Score[lbls_]:=2#1/(2#1+#2 + #3)&@@({Count[#, {yes, yes}], Count[#, {yes, no}], Count[#, {no, yes}]}&@lbls);

TrainHMM[data_, hiddenStates_, epoch_, gfix_]:= Block[{yesD, noD, vdim},
yesD = Keys@Select[data, Values[#]===yes&];
noD = Keys@Select[data, Values[#] === no &];
vdim = Length@First@Keys@First@data;
{ 
  {
    unpackHMM[FitHMM[Flatten[yesD], Length/@yesD, hiddenStates, vdim, epoch, gfix], vdim, hiddenStates],
    N[Length[yesD]/Length[data]]
  },
  {
    unpackHMM[FitHMM[Flatten[noD], Length/@noD, hiddenStates, vdim, epoch, gfix], vdim, hiddenStates],
    N[Length[noD]/Length[data]]
  }
}
];

MeasureHMMF1[data_,models_]:=Block[{py,pn},N[f1Score[Transpose[{getLabels[Keys[data],models[[1,1]],models[[2,1]],models[[1,2]],models[[2,2]]],Values[data]}]]]]
