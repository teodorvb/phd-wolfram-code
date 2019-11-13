#!/usr/bin/python
# -*- coding: utf-8 -*-
"""
Created on Wed May  3 12:30:13 2017

@author: ffz52431
"""

#from Datamodel import Tracks

import track_measures
import msmm
import os
import numpy as np
import scipy as sp
import csv
from scipy import stats

def find_track(tracks, track_id):
    return tracks[track_id]

def extract(track, lvl, key):
    x = [];
    y = [];
    counts = [];
    
    for segment in lvl[key]:
        for i in range(segment[0], segment[1]):
            if (track.map.has_key(i)):
                x.append(track.map[i].x)
                y.append(track.map[i].y)
                counts.append(track.map[i].counts[1])
    return (np.array(counts), np.array(x), np.array(y))

def semdr12(x1, y1, x2, y2):
    xdiff = np.mean(x1) - np.mean(x2)
    ydiff = np.mean(y1) - np.mean(y2)
    diff = np.sqrt(xdiff**2 + ydiff**2)
    return np.sqrt(xdiff**2*(sp.stats.sem(x1)**2 + sp.stats.sem(x2)**2) + ydiff**2*(sp.stats.sem(y1)**2 + sp.stats.sem(y2)**2) )/diff
    
def pos_sem(x, y):
    return np.sqrt(np.sum((x - np.mean(x))**2 + (y - np.mean(y))**2)/(x.size - 1))/np.sqrt(x.size -1)

def pos_std(x, y):
    return np.sqrt(np.sum((x - np.mean(x))**2 + (y - np.mean(y))**2)/(x.size - 1))

def clustering(i1, i2):
    return abs(np.mean(i2) - np.mean(i1))/(np.std(i1) + np.std(i2));

def dr12(x1, y1, x2, y2):
    return np.sqrt( (np.mean(x1) - np.mean(x2))**2 + (np.mean(y1) - np.mean(y2))**2 );


print "reading all.csv"
tracks = track_measures.load_csv('all.csv');
selected = {};
for t in tracks:
    pid = t.project_id()
    if not selected.has_key(pid):
        selected[pid] = {}
    if not selected[pid].has_key(t.track_id()):
        selected[pid][t.track_id()] = {}
    selected[pid][t.track_id()]["level1"] = eval(t.level_repr().split(";")[0].split(":")[1]);
    selected[pid][t.track_id()]["level2"] = eval(t.level_repr().split(";")[1].split(":")[1]);

print "reading ci.csv"
ci = {};
with open('ci.csv', 'rb') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
        if not ci.has_key(row[0]):
            ci[row[0]] = {}
        ci[row[0]][int(row[1])] = (float(row[4]), float(row[2]) + float(row[4]) * float(row[5]));

#ci sep semdr12 pos1_sem pos2_sem pos1_std pos2_std dr12 clustering i1err i2err i1std i2std i1_len i2_len

print "writing scores"
for data_set_id in selected.keys():
    print "reading data set ", data_set_id
    tracks = msmm.read_tracks(os.path.join(data_set_id, 'tracks_Simple.dat'))
    with open('corr.csv', 'a') as f:
        pid = os.path.split(data_set_id)[-1][0:18]
        print "writing data set ", data_set_id
        for track_id in selected[data_set_id].keys():
            print "  writing track id ", track_id
            track = find_track(tracks, track_id)
            lvl = selected[data_set_id][track_id];
            
            try:
                f.write(str(ci[pid][track_id][0]) + ',')    #1 ci
                f.write(str(ci[pid][track_id][1]) + ',')    #2 sep
                
                (i1, x1, y1) = extract(track, lvl, 'level1')
                (i2, x2, y2) = extract(track, lvl, 'level2')
                f.write(str(semdr12(x1, y1, x2, y2)) + ',') #3 SEM in pos12 diff
                
                f.write(str(pos_sem(x1, y1)) + ',')         #4 SEM pos1
                f.write(str(pos_sem(x2, y2)) + ',')         #5 SEM pos2
                f.write(str(pos_std(x1, y1)) + ',')         #6 STD pos1
                f.write(str(pos_std(x2, y2)) + ',')         #7 STD pos2
                
                f.write(str(dr12(x1, y1, x2, y2)) + ',')    #8 pos12 diff
                f.write(str(clustering(i1, i2)) + ',')      #9 clustering
                
                f.write(str(sp.stats.sem(i1)) + ',')        #10 SEM int1
                f.write(str(sp.stats.sem(i2)) + ',')        #11 SEM int2
                f.write(str(np.std(i1)) + ',')              #12 STD int1
                f.write(str(np.std(i2)) + ',')              #13 STD int2
                
                f.write(str(len(i1)) + ',')                 #14 LEN int1
                f.write(str(len(i2)) + ',')                 #15 LEN int2
                
                f.write(str(np.mean(i1)) + ',')             # 16 Mean int1
                f.write(str(np.mean(i2)) + '\n')            # 17 Mean int2
            except:
                print "error with track id ", track_id
                pass
