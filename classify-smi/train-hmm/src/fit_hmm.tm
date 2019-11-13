#include<wstp.h>
#include<math.h>
#include<stdlib.h>
#include "train_hmm.hpp"

:Begin:
:Function:      fit_hmm
:Pattern:       FitHMM[data:{___Real}, lengths:{___Integer}, hiddenStates_Integer, frameDim_Integer, epoch_Integer, gaussianFix_Real]
:Arguments:     {data, lengths, hiddenStates, frameDim, epoch, gaussianFix }
:ArgumentTypes: { RealList, IntegerList, Integer, Integer, Integer, Real}
:ReturnType:    Manual
:End:
:Evaluate:      FitHMM::usage = "FitHMM[data, lengths, hiddenStates, frameDim, epoch, gaussianFix] fit a hmm to a sequence"



void fit_hmm(double* data, long int data_len,
	     int * lengths, long int lengths_len,
	     int hidden_states,
	     int frame_dim,
	     int epoch,
	     double gaussian_fix) {

  WSPutReal64List(stdlink,
		  train(data, data_len,
		        lengths, lengths_len,
		        frame_dim,
		        hidden_states,
			epoch,
			gaussian_fix),
                   hmm_params_length(hidden_states, frame_dim));
}


int main(int argc, char **argv) {
  return WSMain(argc, argv);
}
