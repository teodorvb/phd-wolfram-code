#!/usr/bin/python

import numpy as np
import tensorflow as tf
import math as m
import sys as sys

def find_max(predicted):
    res = np.zeros(np.shape(predicted))
    for i in range(len(res)):
        res[i, np.where(predicted[i] == max(predicted[i]))[0][0]] = 1
    return res

def f1_score(actual, predicted):
    conf = np.matrix(actual).T*np.matrix(find_max(predicted))

    TP = conf[0,0];
    FP = conf[1, 0];
    FN = conf[0, 1];
    precision = TP / (TP + FP)
    recall = TP / (TP + FN)
    return 2 * precision * recall / (precision + recall)


def random_split(data, labels, split_point):
    data_label_pairs = list(zip(data, labels))
    np.random.shuffle(data_label_pairs)

    l_labels, d_labels = np.shape(labels);
    l_data, d_data = np.shape(data);

    assert (l_labels == l_data), "Data lenght different from labels length"
    
    train_label_pairs = data_label_pairs[:m.floor(split_point*l_data)]
    test_label_pairs = data_label_pairs[m.floor(split_point*l_data):]

    train_data = np.zeros((len(train_label_pairs), d_data));
    train_labels = np.zeros((len(train_label_pairs), d_labels));

    test_data = np.zeros((len(test_label_pairs), d_data));
    test_labels = np.zeros((len(test_label_pairs), d_labels));

    for i in range(len(train_label_pairs)):
        train_data[i, :] = train_label_pairs[i][0]
        train_labels[i, :] = train_label_pairs[i][1]

    for i in range(len(test_label_pairs)):
        test_data[i, :] = test_label_pairs[i][0]
        test_labels[i, :] = test_label_pairs[i][1]

    return (train_data, train_labels, test_data, test_labels);


def train_network(data, hus, epoch):
    train_data, train_labels, test_data, test_labels = data


    result = np.zeros((len(hus), 2));
    for hus_id in range(len(hus)):
        model = tf.keras.Sequential([
            tf.keras.layers.Dense(hus[hus_id],
                                  input_shape=(np.shape(train_data)[1],),
                                  activation="tanh", kernel_initializer=tf.keras.initializers.GlorotNormal()),
            tf.keras.layers.Dense(2, activation="softmax", kernel_initializer=tf.keras.initializers.GlorotNormal())
        ])


        model.compile(optimizer='adam',
                      loss = 'categorical_crossentropy',
                      metric=['accuracy']);

        model.fit(train_data, train_labels, epochs=epoch, verbose=0);

        result[hus_id, :] = np.array([f1_score(train_labels, model.predict(train_data)),
                                      f1_score(test_labels, model.predict(test_data))])

    return result

hus_num = 10;
epoch = 1000;

hus = np.array(list(range(hus_num))) + 1

data = np.genfromtxt(sys.argv[1], delimiter=',');
labels = np.genfromtxt(sys.argv[2], delimiter=',');
output = sys.argv[3]

result = train_network(random_split(data, labels, 0.7), np.array(list(range(9)))+1, epoch);
np.savetxt(output, result, delimiter=",")
