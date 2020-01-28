#!/usr/bin/env wolframscript
(* ::Package:: *)

<<"helpers.wls"


fI = 80;
fB = 30;


round = 0.001;

(* StepRatio *)

(* In this simulation the ratio between level 2 and level 1 is different from 2.0 *)
ndata = addNoise@Round[#, round]&@{
  Block[{
    trackNum = 3,
    trackDist = 25,
    separations = N@{{5nm, 10nm, 15nm}, {20nm, 25nm, 30nm}, {35nm, 40nm, 45nm}},
    fIs,
    bothOn,
    oneOn,
    noOn
    },
    
    fIs = Flatten[{fI, fI * #}&/@Join[
      RandomReal[NormalDistribution[0.5, 0.1], 4],
      RandomReal[NormalDistribution[1.7, 0.1], 5]
      ]
    ];
    
    bothOn = GenerateFrame[Flatten[Table[{1, 1}, trackNum^2]],fIs,
      Flatten[Table[{
        {x*trackDist, y*trackDist}, 
        {x*trackDist+ separations[[x, y]], y*trackDist}
        },
        {x, 1, trackNum},{y, 1, trackNum}], 2], 
      fB, frameSize];
    
    oneOn = GenerateFrame[Flatten[Table[{1, 0}, trackNum^2]],fIs,
      Flatten[Table[{
        {x*trackDist, y*trackDist}, 
        {x*trackDist+ separations[[x, y]], y*trackDist}
        },
        {x, 1, trackNum},{y, 1, trackNum}], 2], 
      fB, frameSize];

    noOn = GenerateFrame[Flatten[Table[{0, 0}, trackNum^2]],fIs,
      Flatten[Table[{
        {x*trackDist, y*trackDist}, 
        {x*trackDist+ separations[[x, y]], y*trackDist}
        },
        {x, 1, trackNum},{y, 1, trackNum}], 2], 
      fB, frameSize];
    
    Join[Table[bothOn, 100], Table[oneOn, 100], Table[noOn, 20]]
]
};

exportSimulatedData["rtsp-stepratio", ndata];


(* Extrapolated Intensity 1 *)
(* Level 2 is first and Level 1 is last. Intensity after level 1 for 4 tracks
 * is higher than 0. There is additional undetected fluorophore (Possibly very
 * dim). Intensity after level 1 for 5 tracks is lower than 0. There is a 
 * fluorophore in proximity to the feature.
 *)
ndata= addNoise@Round[#, round]&@ {
Quiet@Block[{
fnum = 3,
fInt = 80,
fDist,
fSep = 10nm,
fOn, fI, fP,
on3, on2, on1, on0,
fIntAdd1, fIntAdd2},
fDist =N@ frameSize/(fnum+1);
fIntAdd1 = RandomReal[UniformDistribution[{5,7}], 4];
fIntAdd2 = RandomReal[UniformDistribution[{80, 90}], 5];
fI = Join[
Flatten@Table[{fInt, fInt, fIntAdd1[[i]]},{i, 1, 4}],
Flatten@Table[{fInt, fInt, fIntAdd2[[i]]},{i, 1, 5}]
];
fP = N@Join[
Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + fSep/2, y*fDist}
}, {x, 1, fnum}, {y, 1, 1}], 2],

Flatten[ {
{{25, 50}, {25 + fSep, 50}, {25 + fSep/2, 50}},
{{50, 50}, {50 + fSep, 50}, {54, 54}},
{{75, 50}, {75 + fSep, 50}, {79, 54}}
}, 1],

Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + 4, y*fDist +4}
}, {x, 1, fnum}, {y, 3, 3}], 2]
];

on3= GenerateFrame[Flatten@Table[{1, 1, 1}, fnum^2],fI,fP, 30, frameSize];
on2 = GenerateFrame[Flatten@Table[{1, 0, 1}, fnum^2],fI,fP,30, frameSize];
on1 = GenerateFrame[Flatten@Table[{0, 0, 1}, fnum^2],fI,fP,30, frameSize];
on0 = GenerateFrame[Flatten@Table[{0, 0, 0}, fnum^2],fI,fP,30, frameSize];
Join[Table[on3, 100],Table[on2, 100], Table[on1, 20],Table[on0, 20]]
]};

exportSimulatedData["rtsp-extr-int1", ndata];


(* Extrapolated Intensity 2 *)
(* Level 1 is first and Level 2 is last. Intensity before level 1 for 4 tracks
 * is higher than 0. There is additional undetected fluorophore (Possibly very
 * dim). Intensity before level 1 for 5 tracks is lower than 0. There is a
 * fluorophore in proximity to the feature.
 *)
ndata= addNoise@Round[#, round]&@ {
Quiet@Block[{
fnum = 3,
fInt = 80,
fDist,
fSep = 10nm,
fOn, fI, fP,
on3, on2, on1, on0,
fIntAdd1, fIntAdd2},
fDist =N@ frameSize/(fnum+1);
fIntAdd1 = RandomReal[UniformDistribution[{5,7}], 4];
fIntAdd2 = RandomReal[UniformDistribution[{50, 70}], 5];
fI = Join[
Flatten@Table[{fInt, fInt, fIntAdd1[[i]]},{i, 1, 4}],
Flatten@Table[{fInt, fInt, fIntAdd2[[i]]},{i, 1, 5}]
];
fP = N@Join[
Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + fSep/2, y*fDist}
}, {x, 1, fnum}, {y, 1, 1}], 2],

Flatten[ {
{{25, 50}, {25 + fSep, 50}, {25 + fSep/2, 50}},
{{50, 50}, {50 + fSep, 50}, {54, 54}},
{{75, 50}, {75 + fSep, 50}, {79, 54}}
}, 1],

Flatten[Table[{
{x*fDist, y*fDist},
{x*fDist + fSep, y*fDist},
{x*fDist + 4, y*fDist +4}
}, {x, 1, fnum}, {y, 3, 3}], 2]
];

on3= GenerateFrame[Flatten@Table[{1, 1, 1}, fnum^2],fI,fP, 30, frameSize];
on2 = GenerateFrame[Flatten@Table[{1, 0, 1}, fnum^2],fI,fP, 30, frameSize];
on1 = GenerateFrame[Flatten@Table[{0, 0, 1}, fnum^2],fI,fP, 30, frameSize];
on0 = GenerateFrame[Flatten@Table[{0, 0, 0}, fnum^2],fI,fP, 30, frameSize];
Join[Table[on0, 20], Table[on1,20], Table[on2, 100], Table[on3, 100]]
]};

exportSimulatedData["rtsp-extr-int2", ndata];


(* Has additional fluorescence in the Neighbourhood *)

ndata = addNoise@Round[#, round]&@{
  Block[{
    fnum = 3,
    fInt = 80,
    fSep = 10nm,
    fDist,fAddI,angle, 
    fI, fP,
    flfrsOn3, flfrsOn2, flfrsOn0,
    on3, on2, on0},
    
    fDist =N@ frameSize/(fnum+1);
    fAddI = RandomReal[NormalDistribution[60, 10],12];
    angle = RandomReal[UniformDistribution[{0, 2*Pi}], 12];
    
    fI = Flatten@Join[
      Table[{fInt, fInt, fAddI[[i]],  fAddI[[i+1]]},{i, 1, 6, 2}],
      Table[{fInt, fInt, fAddI[[i]]}, {i, 7, 12}]
      ];
    
    fP = N@Flatten[
      Join[
        Table[{
          {x*fDist, y*fDist},
          {x*fDist + fSep, y*fDist},
          {x*fDist + 5*Cos[angle[[(x-1)*fnum +2*y - 1]] ], y*fDist +5*Sin[angle[[(x-1)*fnum + 2*y - 1]]]},
          {x*fDist + 5*Cos[angle[[(x-1)*fnum + 2*y]] ], y*fDist +5*Sin[angle[[(x-1)*fnum + 2*y]]]}
        }, {x, 1, 1}, {y, 1, fnum}],
        
        Table[{
          {x*fDist, y*fDist},
          {x*fDist + fSep, y*fDist},
          {x*fDist + 5*Cos[angle[[(x-1)*fnum +y +3]] ], y*fDist +5*Sin[angle[[(x-1)*fnum + y + 3]]]}
        }, {x, 2, fnum}, {y, 1, fnum}]
      ],
    2];
  
    flfrsOn3 = Flatten@Join[Table[{1, 1, 1, 1}, 3], Table[{1, 1, 1}, 6]];
    flfrsOn2 = Flatten@Join[Table[{1, 0, 1, 1}, 3], Table[{1, 1, 1}, 6]];
    flfrsOn0 = Table[0, 30];
    
    on3=Quiet@ GenerateFrame[flfrsOn3,fI,fP, 30, frameSize];on2 =Quiet@GenerateFrame[flfrsOn2,fI,fP, 30, frameSize];
    on0 =Quiet@GenerateFrame[flfrsOn0,fI,fP, 30, frameSize];
    Join[ Table[on3, 100],Table[on2, 100], Table[on0, 20]]
  ]
}
exportSimulatedData["rtsp-neighbour", ndata];


(* Tracks with one level *)

ndata = addNoise@Round[#, round]&@{
Block[{
fnum = 3,
fInt = 80,
fSep = 10nm,
fI, fP,
on9, on8,on7,on6,on5,on4, on3, on2, on1, on0,
fDist},
fDist =N@ frameSize/(fnum+1);
fI = Table[fInt, fnum^2];

fP = N@Flatten[Table[{x*fDist, y * fDist},{x, 1, fnum}, {y, 1, fnum}], 1];

on9 = Quiet@GenerateFrame[{1, 1, 1, 1, 1, 1, 1, 1, 1}, fI, fP, 30, frameSize];
on8 = Quiet@GenerateFrame[{1, 1, 1, 1, 1, 1, 1, 1, 0}, fI, fP, 30, frameSize];
on7 = Quiet@GenerateFrame[{1, 1, 1, 1, 1, 1, 1, 0, 0}, fI, fP, 30, frameSize];
on6 = Quiet@GenerateFrame[{1, 1, 1, 1, 1, 1, 0, 0, 0}, fI, fP, 30, frameSize];
on5 = Quiet@GenerateFrame[{1, 1, 1, 1, 1, 0, 0, 0, 0}, fI, fP, 30, frameSize];
on4 = Quiet@GenerateFrame[{1, 1, 1, 1, 0, 0, 0, 0, 0}, fI, fP, 30, frameSize];
on3 = Quiet@GenerateFrame[{1, 1, 1, 0, 0, 0, 0, 0, 0}, fI, fP, 30, frameSize];
on2 = Quiet@GenerateFrame[{1, 1, 0, 0, 0, 0, 0, 0, 0}, fI, fP, 30, frameSize];
on1 = Quiet@GenerateFrame[{1, 0, 0, 0, 0, 0, 0, 0, 0}, fI, fP, 30, frameSize];
on0 = Quiet@GenerateFrame[{0, 0, 0, 0, 0, 0, 0, 0, 0}, fI, fP, 30, frameSize];

Join[
Table[on9, 10],
Table[on8, 10], 
Table[on7, 10],
Table[on6, 10],
Table[on5, 10],
Table[on4, 10],
Table[on3, 10],
Table[on2, 10],
Table[on1, 10],
Table[on0, 10]
]
]
}

exportSimulatedData["rtsp-1lvl", ndata];
