#!/usr/bin/env wolframscript
(* ::Package:: *)

datasetNameI = 1;
trackIdI = 2;
dataIntI = 3;
levelsI = 4;
levelsOldI= 5;
levelsNewI = 6;


unpackDBData ={#1, #2, 
ArrayReshape[ImportByteArray[ByteArray[#8],"Real32"], {#3, #4}],

ToExpression[#5],
ToExpression[#6],
ToExpression[#7]
}&;


readDataSet[id_]:= unpackDBData@@@SQLExecute[conn, "select 
source_data_set, 
source_track_id,
track_size,
frame_dim_int, 
levels, 
levels_old,
levels_new,
data_int 

from smidata
where 
    dataset_id = "<>ToString[id]<>"
order by source_data_set, source_track_id"
];


readDataOneOfEach[id_]:=Block[{res,ds},
res = SQLExecute[conn, "select source_data_set, source_track_id from smidata where dataset_id = `1`", {id}];
ds =<|#-> {}&/@Union@res[[All, 1]]|>;
AppendTo[ds[#1] , #2] &@@@ res;

unpackDBData@@@SQLExecute[conn,
"select 
source_data_set, 
source_track_id,
track_size,
frame_dim_int, 
levels, 
levels_old,
levels_new,
data_int 

from smidata
where 
    dataset_id = `1`
and (source_data_set, source_track_id) in "<> StringJoin[
Prepend[
Append[
Riffle["('"<>#1<>"',"<>ToString[#2]<>")"&@@@({#1, First@RandomSample@ds[#1]}&/@ Keys@ds),","],
")"
], "("
]
]<> " order by source_data_set, source_track_id", {id}]
]


counts = 2;
sigCounts = 3;
posX = 4;
posY = 5;
background = 6;
interpolated = 7;
