(* ::Package:: *)

BeginPackage["RBM`"]

RBMAutoEncoder::usage = "RBMAutoEncoder[rbms]"
RBMHiddenExpectation::usage = "RBMHiddenExpectation[{w_, a_, b_}, data_]"
RBMTrain::usage = "RBMTrain[hidden_,data_]"
RBMRandom::usage = "RBMRandom[in_,out_]"
RBMTrainResample::usage = "RBMTrainResample[w_,wt_,a_,b_,v0_]"
RBMTrainDelta::usage = "RBMTrainDelta[w_, a_,b_,data_]"

Begin["Private`"]

RBMRandom[in_,out_]:={RandomReal[NormalDistribution[0,0.3],{out,in}], RandomReal[NormalDistribution[0,0.3],in],RandomReal[NormalDistribution[0,0.3],out]};

RBMTrainResample[w_,wt_,a_,b_,v0_]:=Block[
{v1, h1, h0},
h0=If[#1<#2, 1, 0]&@@@Transpose[{RandomReal[UniformDistribution[{0, 1}], Length[b]],LogisticSigmoid[w.v0+b]}];
v1= wt.h0+a + 0.1* RandomReal[NormalDistribution[], Length[a]];
h1 = If[#1<#2, 1, 0]&@@@Transpose[{RandomReal[UniformDistribution[{0, 1}], Length[b]],LogisticSigmoid[w.v1+b ]}];
{{Outer[Times,h0,v0],v0,h0},{Outer[Times,h1,v1],v1,h1}}
];

RBMTrainDelta[w_, a_,b_,data_]:= Block[{wt},
  wt = Transpose@w;
  (#1-#2&)@@ (Total[RBMTrainResample[w, wt, a,b, #]& /@ data]/Length[data])
];

Options[RBMTrain] = {Momentum-> 0.9, LearningRate -> 0.1, MaxTrainingRounds-> 1000};
RBMTrain[hidden_Integer,data_List,OptionsPattern[]]:=Block[
{visible, delta, w, a, b, \[Theta], \[Epsilon], epoch, err},

visible = Dimensions[data][[2]];
\[Theta] = OptionValue[Momentum];
\[Epsilon] = OptionValue[LearningRate];
epoch = OptionValue[MaxTrainingRounds];

{w,a,b}=RBMRandom[visible,hidden];
Global`errors = {};
delta=\[Epsilon] RBMTrainDelta[w,a,b,data];
Do[
  delta=\[Theta] delta+\[Epsilon] RBMTrainDelta[w,a,b,data];
  {w,a,b}={w,a,b}+delta;
  err = Mean[#.#&/@(data - RBMVisibleExpectation[{w, a, b}, RBMHiddenExpectation[{w, a, b}, data]])];
  AppendTo[Global`errors, err],
  {i, epoch}];
  
{w,a,b}
]

RBMAutoEncoder[rbms_]:={}/;Length[rbms]==0;
RBMAutoEncoder[rbms_]:=Block[
{rbm, in, out},
rbm=First[rbms];
{in,out}=Dimensions[rbm[[1]]];
Join[{LinearLayer[in,"Weights"->rbm[[1]],"Biases"->rbm[[3]]],LogisticSigmoid},RBMAutoEncoder[Rest[rbms]],{LinearLayer[out,"Weights"->Transpose[rbm[[1]]],"Biases"->rbm[[2]]],LogisticSigmoid}]
]/;Length[rbms]>0;

RBMHiddenExpectation[{w_, a_, b_}, data_]:=(LogisticSigmoid/@(w.# + b))&/@data;
RBMVisibleExpectation[{w_,a_, b_}, data_]:=(Transpose[w].# + a)&/@data;
(*
RBMTrainMultiple[encoderSizes_List,data_List,\[Theta]_Real,\[Epsilon]_Real,epoch_Integer]:={}/;Length[encoderSizes]==0;

RBMTrainMultiple[encoderSizes_List,data_List]:=Block[
{rbm},
rbm=RBMTrain[Dimensions[data][[2]],First[encoderSizes]];
Prepend[
RBMTrainMultiple[Rest[encoderSizes],RBMHiddenExpectation[rbm,data]],
rbm]
]/;Length[encoderSizes]>0;

TrainAutoEncoder[encoderSizes_List,data_List,\[Theta]_Real,\[Epsilon]_Real,epoch_Integer]:= Block[
{rbms},
NetChain[RBMAutoEncoder[RBMTrainMultiple[encoderSizes,N[data],N[\[Theta]],N[\[Epsilon]],epoch]], "Input"->Dimensions[data][[2]]]
];
*)

End[]
EndPackage[]









