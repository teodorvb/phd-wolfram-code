#include <track-select>
#include <Eigen/Core>
#include <vector>
#include <sstream>
#include <dirent.h>
#include <string>
#include <algorithm>
#include <iterator>
#include <fstream>


track_select::hmm::GHMM train(const std::vector<std::vector<track_select::Vector>>::const_iterator begin,
			      const std::vector<std::vector<track_select::Vector>>::const_iterator end,
			      int hidden_states,
			      int epoch,
			      double gaussian_fix) {
  track_select::common::Gaussian::covar_fix = gaussian_fix;
  int dim = begin->begin()->size();
  
  // Move all observations into an array
  std::vector<track_select::Vector> observed;
  for (auto seq = begin; seq != end; seq++)
    for (auto& frame : *seq)
      observed.push_back(frame);
  int observed_len = observed.size();

  std::ofstream out("/tmp/train-hmm.log");

  out << "observed len: " << observed_len << std::endl;
  
  // Calculate starting point for Gaussian Mixture Models
  double init_sigma = 0.0;
  track_select::Vector mean = track_select::Vector::Zero(dim);
  for(auto& f : observed)
    mean += f/observed_len;
  for(auto& f : observed)
    init_sigma += std::sqrt((f - mean).squaredNorm())/observed_len;


  int init_mean_samples = 0.1 * observed.size();

  out << "Clustering" << std::endl;
  // Apply Gaussian Mixture Model Clustering
  auto mixtures = track_select::clustering::gmm(observed.begin(), observed.end(), hidden_states,
					        init_sigma, init_mean_samples);
  out << "clustering finished" << std::endl;
  std::vector<std::tuple<track_select::Vector, track_select::Matrix>> em;
  for (auto& t : mixtures)
    em.push_back(std::make_tuple(std::get<1>(t), std::get<2>(t)));
  
  // Initialise the Hidden Markov Model
  track_select::Vector pi = track_select::Vector::Ones(hidden_states)/hidden_states;
  track_select::Matrix tr = track_select::Matrix::Ones(hidden_states, hidden_states)/hidden_states;


  track_select::hmm::GHMM model(pi, tr, em);

  out << "Mode: " << model.mode() << std::endl;

  track_select::real old_log_prob = 0;
  for (auto it = begin; it != end; it++)
    old_log_prob += model.p(it->begin(), it->end());

  unsigned int prob_unchanged = 0;

  out << "Training model" << std::endl;
  // Train Hidden Markov Model
  for (int i = 0; i < epoch; i++) {
    model.train(begin, end);
    track_select::real log_prob = 0;
    for (auto it = begin; it != end; it++)
      log_prob += model.p(it->begin(), it->end());

    if (std::abs(log_prob - old_log_prob) < 10e-6)
      prob_unchanged++;
    else
      prob_unchanged = 0;

    if (prob_unchanged == 5)
      break;

    out << "Iteration " << i << "log_prob: " << log_prob << (old_log_prob == log_prob)<< std::endl;
    old_log_prob = log_prob;
    

  }
  out << "Model trained" << std::endl;
  out.close();

  return model;
}

int hmm_params_length(int hidden_states, int frame_dim) {
  return (hidden_states + 1 + (frame_dim + 1) * frame_dim) * hidden_states;
}

double * train(double * data, long int data_len,
	       int * lengths, long int lengths_len,
	       int frame_dim,
	       int hidden_states,
	       int epoch,
	       double gaussian_fix) {


  /* Unpack the serialised data */
  long int total_len = 0;

  for (long int i = 0; i < lengths_len; i++)
    total_len += lengths[i];
  
  if (data_len != total_len*frame_dim) {
    std::stringstream ss;
    ss << "Cannot deserialise data. " << "serialised length: " << data_len
       << " total_len " << total_len << " frame_dim " << frame_dim;
    throw std::runtime_error(ss.str());
  }
  std::vector<std::vector<track_select::Vector>> observations(lengths_len);

  long int data_i = 0;
  for (long int seq_i = 0; seq_i < lengths_len; seq_i++) {
    observations[seq_i] = std::vector<track_select::Vector>(lengths[seq_i], track_select::Vector(frame_dim));
    for (long int frame_i = 0; frame_i < lengths[seq_i]; frame_i++) {
      for (long int dim_i = 0; dim_i < frame_dim; dim_i++) {
	observations[seq_i][frame_i](dim_i) = data[data_i++];
      }
    }
  }
	
  /* Train the HMM */
  auto hmm = train(observations.begin(), observations.end(), hidden_states, epoch, gaussian_fix);

  /* Pack the results */
  double * result = new double[hmm_params_length(hidden_states, frame_dim)];
  int result_i = 0;
  
  for (int i = 0; i < hidden_states; i++)
    result[result_i++] = hmm.pi()(i);

  for (int i = 0; i < hidden_states; i++)
      for (int j = 0; j < hidden_states; j++)
	result[result_i++] = hmm.tr()(i, j);

  for (auto& g : hmm.emParams()) {
    for (int i = 0; i < frame_dim; i++)
      result[result_i++] = g.mean()(i);

    for (int i = 0; i < frame_dim; i++)
      for (int j = 0; j < frame_dim; j++)
	result[result_i++] = g.covar()(i, j);
  }


  return result;
}
