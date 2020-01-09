#!/bin/bash
if [ $# -eq 0 ]
then
    echo "no arguments passed"
    exit
fi

dirname=tmp
dircount=1

while [ -d "$dirname$dircount" ]; do
    let "dircount=dircount+1"
done

tmpdir=$dirname$dircount
echo Will use directory $tmpdir
mkdir $tmpdir

echo "
data = BinaryDeserialize[<<\"${1}\"];
Export[\"$tmpdir/data.csv\", Keys@data];
Export[\"$tmpdir/labels.csv\", If[#===yes, {1, 0}, {0, 1}]&/@Values@data];
" | wolfram &>>$tmpdir/wolfram.log

echo data exported to csv

for i in {1..10}; do
    python classify-nn.py $tmpdir/data.csv $tmpdir/labels.csv $tmpdir/res-$i.csv &>$tmpdir/$i.log &
done

function filesMissing() {
    for i in $(seq 1 $1); do
	if [ ! -f $tmpdir/res-$i.csv ]; then
	    return 0
	fi
    done
    return 1
}

echo Workers launched. Waiting to complete ...
while filesMissing 10 ; do
    sleep 2
done

echo Calculation finished. Exporting result...
echo "
BinarySerialize[Import[\"$tmpdir/res-\"<>ToString[#]<>\".csv\"] & /@Range[10]] >> \"results/$(basename #${1})\"
" | wolfram &>>$tmpdir/wolfram.log

rm $tmpdir -r
