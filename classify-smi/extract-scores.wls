#!/usr/bin/env wolframscript
(* ::Package:: *)

Needs["DatabaseLink`"];
<<"helpers-scores.wls";
<<"helpers-data.wls";
<<"scores.wls";


conn = OpenSQLConnection[
  JDBC["PostgreSQL","127.0.0.1/teodor"],
  "Username"->"teodor"];


batches = SmiMakeDBIdBatches[
  Join[SmiReadPositiveIds[conn, 13], SmiReadNegativeIds[conn, 13]],
  100];
dataScores = Flatten[SmiReadBatch[conn, #, ExtractScores]&/@  batches];
Put[BinarySerialize[dataScores], FileNameJoin[{"data","2014-lda_gt_0.01-revised-scores.bin"}]];
