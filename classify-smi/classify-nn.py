import numpy as np
import tensorflow as tf
import math as m

def f1_score(actual, predicted):
    TP = tf.math.count_nonzero(predicted * actual)
    TN = tf.math.count_nonzero((predicted - 1) * (actual - 1))
    FP = tf.math.count_nonzero(predicted * (actual - 1))
    FN = tf.math.count_nonzero((predicted - 1) * actual)
    
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

def train_network(hus, data, labels, ep):
    train_data, train_labels, test_data, test_labels = random_split(data, labels, 0.7);

    result = np.zeros((len(hus), 2));
    for i in range(len(hus)):
        model = tf.keras.Sequential([
            tf.keras.layers.Dense(hus[i], input_shape=(np.shape(train_data)[1],), activation="tanh", kernel_initializer=tf.keras.initializers.GlorotNormal()),
            tf.keras.layers.Dense(2, activation="softmax", kernel_initializer=tf.keras.initializers.GlorotNormal())
        ])

        model.compile(optimizer='adam',
                      loss = 'categorical_crossentropy',
                      metric=['accuracy']);
        model.fit(train_data, train_labels, epochs=ep);

        result[i, :] = np.array([f1_score(train_labels, model.predict(train_data)), f1_score(test_labels, model.predict(test_data))])

        return result

data = np.genfromtxt('data/flimp-manually-selected-2014_2015-cleaned-data.csv', delimiter=',');
labels = np.genfromtxt('data/flimp-manually-selected-2014_2015-cleaned-labels.csv', delimiter=',');



result = train_network([1, 2, 3], data, labels, 10)
print(result)
