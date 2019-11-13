#ifndef __TRAIN_HMM_H_
#define __TRAIN_HMM_H_

double * train(double * data, long int data_len,
	       int * lengths, long int lengths_len,
	       int frame_dim,
	       int hidden_states,
	       int epoch,
	       double gaussian_fix);


int hmm_params_length(int hidden_states, int frame_dim);


#endif
