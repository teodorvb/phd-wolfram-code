#!/usr/bin/env wolframscript
(* ::Package:: *)

airy[r_?NumericQ,\[Beta]_?NumericQ]:=If[r == 0,(\[Pi]-E^(-(2/\[Beta]^2)) \[Pi])/\[Beta]^4,((1-E^(-(2/\[Beta]^2))) BesselJ[1,(2 \[Pi] r)/\[Beta]]^2)/(\[Pi] r^2 \[Beta]^2)];


sqrt2pi = N@Sqrt[2 Pi];


EMCCD[\[Lambda]_,g_,r_,f_]:=ProbabilityDistribution[Exp[-\[Lambda]-(f x)^2/(2 r^2)]/(sqrt2pi r)+Exp[-\[Lambda]-(f x)/g] Sqrt[\[Lambda]/(f x g)] BesselI[1,2 Sqrt[(\[Lambda] f x)/g]],{x,0,\[Infinity]}]


GenerateNoise[signal_, num_]:=Block[{d},
  d = RandomVariate[EMCCD[signal*q + c, g, r, f], Max[1000, num]];
  While[Abs[Mean[d] -(signal*q + c)*g /f] > StandardDeviation[d] || Min@d < 0,
    d =RandomVariate[EMCCD[signal*q + c, g, r, f], Max[1000, num], WorkingPrecision->20]
  ];
  (*Which[signal < 20, MachinePrecision, signal <= 50 && signal >= 20, 40, signal> 50, 10]*)
  d  
];


addNoise[data_]:=Block[{noiseData, noise, noiseIndex, index, idata, i, ix, iy, d,ud,counts},
  noiseData = data;
  d = Flatten@data;
  ud = Union@d;
  total = Length@ud;
  counter = 0;
  msg = "counting";
  
  (* Count how many to draw from each distribution *)
  CloseKernels[]; LaunchKernels[];
  SetSharedVariable[counter];
  counts = ParallelMap[(counter++;{#, Count[d, #]})&, ud];
  msg = "adding noise";
  
  counter = 0;
  noise = <|
   Parallelize[(counter++; msg = ToString[#1] <> " " <> ToString[#2];#1-> GenerateNoise[#1, #2])& @@@ counts]|>;
  noiseIndex = <|
    #-> 1&/@(Keys@noise)
  |>;
  counter = 0;
  total = 0;
  msg = "Modifying data ...";
  For[idata = 1, idata <= Length[data],idata++,
    For[i = 1, i <= Length@data[[idata]], i++,
      For[ix = 1, ix <= Length@data[[idata, i]], ix++,
        For[iy = 1, iy <= Length@data[[idata, i, ix]], iy++,
          index = data[[idata, i, ix, iy]];
          noiseData[[idata, i, ix, iy]] = noise[index][[noiseIndex[index]++]];
        ]
      ]
    ]
  ];
  noiseData
];


GenerateFrame[fs_, Is_, ps_, b_, fsize_]:= Block[{psfs, PSFgen, img},
PSFgen = GaussianPSF;
psfs = PSFgen[#, GaussianSigma]&/@ ps;
(* Generate diffraction limited pattern *)
img = Table[b, fsize, fsize];
If[#1==1, img+= Table[#3[x,y],{x, 0, fsize-1}, {y, 0, fsize-1}]*#2] & @@@ Transpose[{fs, Is,  psfs}];
N@img
];
FluorophoresHMM[nframes_, tr_]:=Transpose@{
If[# == 2 || #== 3, 0, 1]&/@RandomFunction[
DiscreteMarkovProcess[1, tr], {1, nframes}][[2]][[1]][[1]], 
If[# == 2 || #== 3, 0, 1]&/@RandomFunction[
DiscreteMarkovProcess[1, tr], {1, nframes}][[2]][[1]][[1]]
};


toFormattedString[number_]:=StringJoin[Table["0", 5 - Floor[Log[10,number]]]]<> ToString[number];
getFLImPFrameName[number_]:=
"simulated_raw_"<>toFormattedString[number]<>"_sf000001.h5";
getFLImPBiasName[number_]:= 
"simulated_raw_bias_"<>toFormattedString[number]<>"_sf000001.h5";
getFLImPBeadName[number_]:=
"bead_"<>toFormattedString[number]<>"_sf000001.h5";


Install["export_flimp"];


exportBias[dir_,fsize_]:=Block[{bias, biascfg, ix, frameBias},
(* Generate and export bias *)
frameBias = 2;
bias = RandomVariate[EMCCD[15*q + c, g, r, f], {10, fsize, fsize}];
biascfg = "";
If[!DirectoryQ[dir], CreateDirectory[dir]];
For[ix = 1, ix<= 10,ix++,
ExportFLImP[FileNameJoin[{dir,getFLImPBiasName[ix]}],
 Flatten@bias[[ix]],fsize, fsize, ix, frameBias];
biascfg = StringJoin[biascfg,"BiasImage ", getFLImPBiasName[ix], "\n"];
];
Export[FileNameJoin[{dir,  "bias.cfg"}], biascfg, "Text"];
];

exportBeads[dir_, beads_]:= Block[{
beadscfg,
beadframes,
frameBead,
ix,
fSizeX, fSizeY
},
frameBead = 3;
{fSizeY, fSizeX} = Dimensions@First@beads;
If[!DirectoryQ[dir], CreateDirectory[dir]];

(* Export beads *)
beadscfg = "";
beadframes = Length@beads;
For[ix = 1, ix <= beadframes, ix++,
ExportFLImP[FileNameJoin[{dir,getFLImPBeadName[ix]}],
 Flatten@beads[[ix]], fSizeX, fSizeY, ix, 
frameBead];
beadscfg = StringJoin[{beadscfg, "BeadImage ",getFLImPBeadName[ix], "\n"}];
];
Export[FileNameJoin[{dir,"beads.cfg"}], beadscfg, "Text"];
];

exportData[root_, datasetName_, data_]:=Block[{
dir,
dataLength,
ix,
frameSample,
fSizeX,
fSizeY},
{fSizeY, fSizeX} = Dimensions@First@data;
dir = FileNameJoin[{root, datasetName}];

frameSample = 1;
(* Export the data *)
dataLength = Length[data];
If[!DirectoryQ[dir], CreateDirectory[dir]];
 
For[ix=1,ix<=dataLength,ix++,
ExportFLImP[FileNameJoin[{dir,getFLImPFrameName[ix]}],Flatten@ data[[ix]], fSizeX, fSizeY, ix, frameSample]
];

Export[FileNameJoin[{dir, "channelsetup.cfg"}], 
StringJoin[{ 
"ChannelSetupVersion 2.0\n",
"ExperimentType ExperimentType:Undefined\n",
"FluorophoreRole Fluorophore:? Role:?\n",
"NChannels 1\n",
"Dimensions ", ToString[fSizeX], " ", ToString[fSizeY], "\n",
"ReferenceChannel 1\n",
"Channel 1 1 0 0 Fluorophore:? Pi:? Pi:? 1\n",
"Optics 1 None"}],
 "Text"];

Export[FileNameJoin[{dir, "dataset.cfg"}], 
StringJoin[{
"DatasetName ", datasetName, "\n",
"SampleDescription Simulated data\n",
"Filename [DATA]\\", datasetName,"\\dataset.cfg\n",
"Users ffz52431\n",
"InstrumentId Instrument:MSMMSimulate\n",
"SimulatedData\n",
"RunDir [DATA]\\", datasetName, "\n",
"ImagePattern simulated_raw_\\d{6}_sf\\d{6}\\.h5\n",
"ExperimentSetup [RUN_DIR]\\channelsetup.cfg\n",
"BeadDir [RUN_DIR]\\beads\n"}],
"Text"];
];


generateBeads[beadIntensity_, bgIntensity_, frameNum_, frameSize_]:= Block[{
  trackDist = frameSize/4,
  frame,
  data
  },
  frame = GenerateFrame[
    Table[1, 9],
    Table[beadIntensity, 9],
    Flatten[Table[{x*trackDist, y*trackDist}, {x, 1, 3},{y, 1, 3}], 1],
    bgIntensity,
    frameSize
  ];
  data = Table[frame, frameNum];
  addNoise[{data}]
]


numStr[num_]:=StringJoin[Table["0",4 - Floor[Log[10, num]]]]<>ToString[num];
getDataName[prefix_,len_,fI_, fB_]:=StringJoin[{
prefix,
"-lvl_",numStr[len],
"-I_",numStr[fI],
"-fB_",numStr[fB],
"-q_", ToString[q],
"-c_", ToString[c],
"-g_", ToString[g],
"-r_", ToString[r],
"-f_", ToString[f],
"-sigma_", ToString[GaussianSigma]}];


generateAnalysable[fI_, bI_, levelLengths_, frameSize_, case_]:= Block[{
    trackNum = 3,
    trackDist = 25,
    separations = N@{{5nm, 10nm, 15nm}, {20nm, 25nm, 30nm}, {35nm, 40nm, 45nm}},
    bothOn,
    oneOn,
    noOn
  },
  
  bothOn = GenerateFrame[
    Flatten[Table[{1, 1}, trackNum^2]],
    Flatten[Table[{fI, fI}, trackNum^2]], Flatten[Table[{
      {x*trackDist, y*trackDist}, 
      {x*trackDist+ separations[[x, y]], y*trackDist}
    }, {x, 1, trackNum},{y, 1, trackNum}], 2], 
    bI,
    frameSize];
  
  oneOn = GenerateFrame[
    Flatten[Table[{1, 0}, trackNum^2]],
    Flatten[Table[{fI, fI}, trackNum^2]], Flatten[Table[{
      {x*trackDist, y*trackDist}, 
      {x*trackDist+ separations[[x, y]], y*trackDist}
    }, {x, 1, trackNum},{y, 1, trackNum}], 2], 
    bI,
    frameSize];
    
    noOn = GenerateFrame[
      Flatten[Table[{0, 0}, trackNum^2]],
      Flatten[Table[{fI, fI}, trackNum^2]], Flatten[Table[{
        {x*trackDist, y*trackDist}, 
        {x*trackDist+ separations[[x, y]], y*trackDist}
      }, {x, 1, trackNum},{y, 1, trackNum}], 2], 
      bI,
      frameSize];
      
    generateAnalysable::case = "Unknon value for case: `1`";
    Switch[case,
      1, Join[Table[bothOn, #], Table[oneOn, #], Table[noOn, 20]]&/@ levelLengths,
      2, Join[Table[noOn, 20], Table[oneOn, #], Table[bothOn, #]]&/@ levelLengths,
      _, Message[generateAnalysable::case, case]; {}
    ]
]; (* generateAnalysable *)


adjustParams[fI_, fB_]:=exportData[
NotebookDirectory[],
getDataName["adjust-params",Length[#], fI,fB],
#
]&@First@addNoise@generateAnalysable[fI, fB, {100}, frameSize];
