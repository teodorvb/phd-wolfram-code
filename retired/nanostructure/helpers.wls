#!/usr/bin/env wolframscript
(* ::Package:: *)

getPlane1[img_]:=img[[41;;1080,1;;610]];
getPlane2[img_]:=img[[51;;1090, 716;;1325]];
getPlane3[img_]:=img[[51;;1090, 1421;;2030]];


getBeadPlane1[img_]:=img[[491;;1530,11;;620]];


findDataMinMax[imgNames_, getPlane_]:=Block[{min, max, dataLength, img, ix},
{min, max} = {Min@#, Max@#}&@getPlane@ImageData@Import@First@imgNames;
For[ix=2,ix<= dataLength,ix++,
img = getPlane@ImageData@Import@imgNames[[ix]];
min = Min[min, Min@img];
max = Max[max, Max@img];
];
{min, max}
]


toFormattedString[number_]:=StringJoin[Table["0", 5 - Floor[Log[10,number]]]]<> ToString[number];
getFLImPFrameName[number_]:="simulated_raw_"<>toFormattedString[number]<>"_sf000001.h5";
getFLImPBeadName[number_]:= "bead_"<>toFormattedString[number]<>"_sf000001.h5";


Install["export_flimp"];


convertData[root_,dataSetName_, imgNames_, getPlane_]:=Block[{
  img,
  fSizeX, fSizeY,
  dataLength, ix, 
  frameSample, dir
  },
  
  dataLength = Length@imgNames;
  frameSample =1 ;
  dir = FileNameJoin[{root, dataSetName}];
  
  If[!DirectoryQ[dir], CreateDirectory[dir]];
  
  For[ix = 1, ix<= dataLength, ix++,
    img = getPlane@ImageData@Import@imgNames[[ix]];
    {fSizeY, fSizeX} = Dimensions@img;
    ExportFLImP[FileNameJoin[{dir, getFLImPFrameName[ix]}], Flatten@img, fSizeX,fSizeY,ix, frameSample ]
  ];
  
  Export[
    FileNameJoin[{dir, "channelsetup.cfg"}], 
    StringJoin[{ 
      "ChannelSetupVersion 2.0\n",
      "ExperimentType ExperimentType:Undefined\n",
      "FluorophoreRole Fluorophore:? Role:?\n",
      "NChannels 1\n",
      "Dimensions ", ToString[fSizeX], " ", ToString[fSizeY], "\n",
      "ReferenceChannel 1\n",
      "Channel 1 1 0 0 Fluorophore:? Pi:? Pi:? 1\n",
      "Optics 1 None"
    }],
    "Text"
  ];
  
  Export[
    FileNameJoin[{dir, "dataset.cfg"}], 
    StringJoin[{
      "DatasetName ", dataSetName, "\n",
      "SampleDescription Simulated data\n",
      "Filename [DATA]\\", dataSetName,"\\dataset.cfg\n",
      "Users ffz52431\n",
      "InstrumentId Instrument:MSMMSimulate\n",
      "SimulatedData\n",
      "RunDir [DATA]\\", dataSetName, "\n",
      "ImagePattern simulated_raw_\\d{6}_sf\\d{6}\\.h5\n",
      "ExperimentSetup [RUN_DIR]\\channelsetup.cfg\n",
      "BeadDir [RUN_DIR]\\beads\n"
    }],
    "Text"
  ];
  dir
];


convertBeads[dir_, beadNames_, getPlane_]:=Block[{
  frameBead,
  ix, dataLength,
  img,
  fSizeX,
  fSizeY,
  beadscfg},
  
  If[!DirectoryQ[dir], CreateDirectory[dir]];
  
  frameBead = 3;
  dataLength = Length@beadNames;
  beadscfg = "";
  
  For[ix=1, ix<= dataLength, ix++,
    img = getPlane@ImageData@Import@beadNames[[ix]];
    {fSizeY, fSizeX} = Dimensions@img;
    ExportFLImP[FileNameJoin[{dir, getFLImPBeadName[ix]}], Flatten @img, fSizeX, fSizeY, ix, frameBead];
    beadscfg = StringJoin[{beadscfg, "BeadImage ",getFLImPBeadName[ix], "\n"}];
  ];
  
  Export[FileNameJoin[{dir,"beads.cfg"}], beadscfg, "Text"];
  dir
];
