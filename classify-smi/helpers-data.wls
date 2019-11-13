#!/usr/bin/env wolframscript
(* ::Package:: *)

SmiMakeDBIdBatches[data_, batch_]:= Function[{rem},
If[ rem != 0,
Append[ArrayReshape[#, {Floor[Length@#/batch], batch}], #[[-rem;;-1]]]&@ data,
ArrayReshape[#,{Floor[Length@#/batch], batch}]&@ data
]
] @ Mod[Length@data, batch];

SmiReadPositiveIds[conn_,dataSetId_]:=Flatten@SQLExecute[conn, "select id from smidata
where
    dataset_id = "<>ToString[dataSetId]<> "
and lda_filter > 0.010
and category = 1"];
SmiReadNegativeIds[conn_, dataSetId_]:= Flatten@SQLExecute[conn, "select id from smidata
where
    dataset_id = "<>ToString[dataSetId]<>"
and lda_filter > 0.010
and category = 0"];

SmiReadBatch[conn_, batchIds_, transform_]:= transform/@(
SmiUnpackRecords@@@
  SQLExecute[conn, "select 
source_data_set, 
source_track_id, 
track_start, 
track_end, 
category, 
levels_order, 
track_size,
frame_dim_int, 
frame_dim_img, 
scores, 
levels, 
data_int, 
data_img
from smidata
where 
 id in "<> sqlIdsList[batchIds]
 ]);
 
sqlIdsList[ids_]:="("<>StringTake[#, StringLength[#]-2]&@StringJoin@Flatten@({ToString[#], ", "}&/@ids)<>")";

SmiUnpackRecords  ={
  #1,#2,#3,#4,#5,#6,
  If[!#10 === Null, ToExpression[StringReplace[#10, {"e+" :> "*^", "e-" :> "*^-"}]], {}],
  ToExpression[#11],
  ArrayReshape[ImportByteArray[ByteArray[#12],"Real32"], {#7, #8}],ArrayReshape[ImportByteArray[ByteArray[#13],"Real32"], {#7, #9}],
  ToExpression[#14]
}&;


compressPCA[data_, components_]:= Block[{evect},
  evect = Eigenvectors@Covariance@Keys@data;
  1-> #2&@@@(Transpose[{#1.Transpose[evect[[1;;components]]], #2}] & @@ {Keys@data, Values@data})
]
