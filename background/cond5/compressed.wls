#!/usr/bin/env wolframscript
(* ::Package:: *)

labelled = BinaryDeserialize[Get[FileNameJoin[{Directory[], "..", "labelled.bin"}]]];
ac = BinaryDeserialize[<<"ac.bin"];
components = BinaryDeserialize[<<"components.bin"];

\[Mu] = Mean[N[Keys[labelled]]];

compressed = Join[<| 
Function[{key,en},
("AC " <> ToString[key]) -> (en[Keys[#] - \[Mu]] -> Values[#]&/@labelled)
]@@ {#, Take[ac[#][Trained], Length[NetExtract[ac[#][Trained], All]]/2 ]}&/@Keys[ac]
|> , <|
  Function[key, ("PCA " <> ToString[key]) -> (components[[1;;key]].(Keys[#] - \[Mu]) -> Values[#]&/@labelled)]/@Keys[ac]
|>];

BinarySerialize[compressed]>>"compressed.bin"
