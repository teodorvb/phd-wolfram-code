#!/usr/bin/env wolframscript
(* ::Package:: *)

readTracksByIdsGetBatches[ids_]:="("<>StringRiffle[(ToString/@Flatten@#), ","]<>")"&/@Join[Partition[ids, 1000], {ids[[Floor[Length[ids]/1000] *1000+1;;-1]]}];

readTracksByIds[conn_,ids_, levelsIndex_]:=Block[{},
  Flatten[
    (ExtractScores[#,levelsIndex]&/@Select[
      SmiUnpackRecords@@@ SQLExecute[conn, "select 
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
data_img,
levels_old
from smidata
where
   id in "<>#
],
      Length[#[[levelsIndex]]]>1&& minLevelLength[#[[levelsIndex]]]> 10&]
    ) &/@ readTracksByIdsGetBatches[ids]
  ]
]


RemoveShorts[levels_]:= Select[levels, Total[#2 - #1+1&@@@#] > 5&];
SmiUnpackRecords  ={
  #1,#2,#3,#4,#5,#6,
  If[!#10 === Null, ToExpression[StringReplace[#10, {"e+" :> "*^", "e-" :> "*^-"}]], {}],
  RemoveShorts@ToExpression[#11],
  ArrayReshape[ImportByteArray[ByteArray[#12],"Real32"], {#7, #8}],
  ArrayReshape[ImportByteArray[ByteArray[#13],"Real32"], {#7, #9}],
  RemoveShorts@ToExpression[#14]
}&;


compressPCA[data_, components_]:= Block[{evect},
  evect = Eigenvectors@Covariance@Keys@data;
  #1-> #2&@@@(Transpose[{#1.Transpose[evect[[1;;components]]], #2}] & @@ {Keys@data, Values@data})
]


whitenData[data_]:=Block[{m, s},
  m = Mean@Keys@data;
  s = StandardDeviation@Keys@data;
  (#1-m)/s -> #2 &@@@ Transpose[{Keys@data, Values@data}]
]
