#!/bin/bash
if [ $# -eq 0 ]
then
    echo "no arguments passed"
    exit
fi

mkdir tmp/

echo "
data = BinaryDeserialize[<<\"${1}\"];
Export[\"tmp/data.csv\", Keys@data];
Export[\"tmp/labels.csv\", If[#===yes, {1, 0}, {0, 1}]&/@Values@data];
" | wolfram

for i in {1..10}; do
    python classify-nn.py tmp/data.csv tmp/labels.csv tmp/res-$i.csv &>tmp/$i.log &
done

function filesMissing() {
    for i in $(seq 1 $1); do
	if [ ! -f tmp/res-$i.csv ]; then
	    return 0
	fi
    done
    return 1
}

while filesMissing 10 ; do
    sleep 2
done

echo "
BinarySerialize[Import[\"tmp/res-\"<>ToString[#]<>\".csv\"] & /@Range[10]] >> \"results/$(basename ${1})\"
" | wolfram

rm tmp -r
