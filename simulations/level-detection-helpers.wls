#!/usr/bin/env wolframscript
(* ::Package:: *)

exportSimulatedData[prefix_, firstBatchId_, ndata_, states_]:= Block[{},
Put[#2,
  FileNameJoin[{
    NotebookDirectory[],
    "data.bin",
    getDataName[prefix<>numStr[#1], Length[#2], 80, 30]<>".bin"
  }]
] &@@@ Transpose[{
  Range[firstBatchId, firstBatchId-1 + Length@ndata,1],
  ndata
}];

exportData[NotebookDirectory[],
  getDataName[prefix<>numStr[#1], Length[#2], 80, 30],
  #2
] &@@@ Transpose[{
  Range[firstBatchId, firstBatchId-1+Length@ndata,1],
  ndata
}];

Put[BinarySerialize[#2],FileNameJoin[{NotebookDirectory[], "mc-states.bin",StringReplace[getDataName[prefix<>numStr[#1], Length[#2]+20, 80, 30], "."-> "_"]}]
]&@@@ Transpose[{
Range[firstBatchId,firstBatchId-1+Length@states,1],
states
}
];
];
generateDataSet[states_, fI1_, fI2_,fI3_]:=Quiet@Block[{
trackNum = 3,
trackDist,
on11,on10, on01, on00,off,
fP,
bI = 30
},

trackDist = frameSize/(trackNum + 1);
fP = Flatten[Table[{
{x*trackDist, y*trackDist}, 
{x*trackDist+10nm, y*trackDist},
{x*trackDist-10nm, y*trackDist}
},{x, 1, trackNum},{y, 1, trackNum}], 2];

on11 = GenerateFrame[
Flatten[Table[{1, 1, 1}, trackNum^2]],
Flatten[Table[{fI1, fI2, fI3}, trackNum^2]],
fP,bI, frameSize];

on10 = GenerateFrame[
Flatten[Table[{1, 0, 1}, trackNum^2]],
Flatten[Table[{fI1, fI2, fI3}, trackNum^2]],
fP,bI, frameSize];

on01 = GenerateFrame[
Flatten[Table[{0, 1, 1}, trackNum^2]],
Flatten[Table[{fI1, fI2, fI3}, trackNum^2]],
fP,bI, frameSize];

on00= GenerateFrame[
Flatten[Table[{0, 0, 1}, trackNum^2]],
Flatten[Table[{fI1, fI2, fI3}, trackNum^2]],
fP,bI, frameSize];

off = GenerateFrame[
Flatten[Table[{0, 0, 0}, trackNum^2]],
Flatten[Table[{fI1, fI2, fI3}, trackNum^2]],
fP,bI, frameSize];
counter = 0;
msg = "Generating Frames";
total = Length@states;

(counter++;
Join[
Switch[#, {1, 1}, on11, {1, 0}, on10, {0, 1}, on01, {0, 0}, on00]&/@#,
Table[off, 20]
])&/@ states
];
