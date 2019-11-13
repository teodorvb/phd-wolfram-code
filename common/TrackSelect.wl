(* ::Package:: *)

BeginPackage["TrackSelect`"]

roc::usage = "roc[x, positive, negative] calculates FP rate and TP rate for threshold x. This can be used with ParametricPlot in order to generate ROC curves."
OneDClassifierMeasure::usage = "OneDClassifierMeasure[tr, positive, negative] measures F1Score and Accuracy for classifier which uses severail measures with threshold for each measure. In order a data point ot be considered positive the value for each meaure needs ot be lower than it's threshold."
intROC::usage = "intROC[positive, negative, {rStart, rEnd}] calculates the area under a ROC curve for 1D data (one measure), using threshold in the range {rStart, rEnd}"
crossValidate::usage = "crossValidate[measure, data, folds] Runs cross-validation of a classifier on data with number of folds specified by folds. The parameters measure[training, testing] is a function which takes  training and testing data and measures performance of a classifier."
NNExperiment::usage = "NNExperiment[labelled, {huStart, huEnd}, samples, folds] cross-validates a neural network on labelled data set with hidden units from huStart to huEnd. Samples is how many time it repeats the experiment."

AdjustThreshold::usage = "AdjustThreshold[y_,x_,e_,times_] uses labels y\[Element]{-1, 1} to adjust threshold for values x \[Element] \[DoubleStruckCapitalR]. It uses learning parameter e > 0 repeats the algorithm times" 

Begin["Private`"]
AdjustThreshold[y_,x_,e_,times_]:=(res=Mean[x];Do[Do[res=res+If[If[x[[i]]<res,1,-1]!=y[[i]],y[[i]] e,0],{i,1,Length[x]}],times];res);

roc[x_, positive_, negative_]:=Count[#, u_/;u<x ]/Length[#]&/@{negative,positive};
OneDClassifierMeasure[tr_, positive_, negative_]:= (
tp = Count[positive, u_/;And@@Thread[u<tr]];
fp = Count[negative, u_/;And@@Thread[u<tr]];
tn = Count[negative, u_/;Or@@Thread[u>tr]];
fn = Count[positive, u_/;Or@@Thread[u>tr]];
precision = tp/(tp+fp);
recall = tp/(tp+fn);
{(2*precision * recall)/(precision +recall), (tp + tn)/(tp + fp + tn + fn)});

intROC[positive_, negative_, {rStart_, rEnd_}]:=(
step = (rEnd - rStart)/100;
N[Total[(#[[2;;-1, 1]]  - #[[1;;-2, 1]])*#[[1;;-2, 2]]]& @(
	roc[#, positive, negative] &/@ Range[rStart, rEnd, step]
	)]);

crossValidate[measure_, data_, folds_]:=Block[
{prts},
  prts = Partition[data, Floor[Length[data]/folds]];
  Table[
   measure[Flatten[Join[prts[[1;;i-1]], prts[[i+1;;-1]]], 1], prts[[i]] ],
  {i, 1, folds}]
]
	
nnetMeasure[trainData_, testData_, hiddenUnits_, classes_]:={
  ClassifierMeasurements[#,trainData, "Accuracy"],
  ClassifierMeasurements[#,testData, "Accuracy"]
}& @ NetTrain[
	  NetInitialize[
		NetChain[{
			LinearLayer[hiddenUnits],
			Tanh,
			LinearLayer[Length[classes]],
			SoftmaxLayer[]
			}, 
		  "Input" -> Length[Keys[trainData[[1]]]],
		  "Output" -> NetDecoder[{"Class",classes}]],
	   Method -> "Orthogonal"],
	trainData,
	CrossEntropyLossLayer["Target" -> NetEncoder[{"Class", classes}]] ,
	Method-> "ADAM",
	MaxTrainingRounds->10000,
	TargetDevice-> "CPU",
	TrainingProgressReporting -> None];

NNExperiment[labelled_, {huStart_, huEnd_}, samples_, folds_]:= Block[
{classes, randomLabelled},
classes = Union[Values[labelled]];
Table[
  Print["  ", hus];
  CloseKernels[]; LaunchKernels[16];
  DistributeDefinitions[{labelled, folds, classes, hus, nnetMeasure,crossValidate}];
  ParallelTable[
    crossValidate[nnetMeasure[#1, #2, hus, classes] &, RandomSample[labelled], folds],
    samples],
  {hus, huStart, huEnd}]
];

End[]
EndPackage[]
