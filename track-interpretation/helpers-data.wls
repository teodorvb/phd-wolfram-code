#!/usr/bin/env wolframscript
(* ::Package:: *)

sourceDataSetI = 1;
sourceTrackIdI = 2;
dataIntI = 3;
dataImgI = 4;
levelsI = 5;

recordExtract[
sourceDataSet_,
sourceTrackId_,
trackSize_,
frameDimInt_,
frameDimImg_,
dataInt_,
dataImg_,
levels_]:= {
sourceDataSet,
sourceTrackId,
ArrayReshape[ImportByteArray[ByteArray[dataInt],"Real32"],{trackSize, frameDimInt}],
ArrayReshape[ImportByteArray[ByteArray[dataImg],"Real32"],{trackSize, frameDimImg}],
ToExpression[levels]
}
extractDataSet[conn_, id_]:= recordExtract@@@SQLExecute[conn,"select 
source_data_set,
source_track_id,
track_size,
frame_dim_int,
frame_dim_img,
data_int,
data_img,
levels
from smidata where
dataset_id = `1`", {id}]
