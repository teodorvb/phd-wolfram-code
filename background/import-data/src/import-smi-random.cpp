#include<iostream>
#include<string>
#include<sstream>
#include<fstream>
#include<vector>

#include <datamodel.hpp>
#include <msmm_project.hpp>
#include <counter++.h>

#include <H5Cpp.h>

#define IMAGE_CUT_SIZE 5



template<typename Out>
void split(const std::string &s, char delim, Out result) {
  std::stringstream ss;
  ss.str(s);
  std::string item;
  while (std::getline(ss, item, delim)) {
    *(result++) = item;
  }
}


std::vector<std::string> split(const std::string &s, char delim) {
  std::vector<std::string> elems;
  split(s, delim, std::back_inserter(elems));
  return elems;
}



std::vector<float> extract_point(MSMMProject::Project& project, int track_id, int frame_id) {
  std::vector<float> dp;

  const MSMMDataModel::Track& tr = project.get_tracks()[track_id];
  const MSMMDataModel::Feat& feature = tr[frame_id];
  const MSMMDataModel::ImFrame frame = project.stacks().proc_frames()[frame_id];
  MSMMDataModel::ChId channel_id = project.get_channel_config().getId(0);

  for (int i = -IMAGE_CUT_SIZE; i <= IMAGE_CUT_SIZE; i++)
    for (int j = -IMAGE_CUT_SIZE; j <= IMAGE_CUT_SIZE; j++)
      dp.push_back(frame.getImage(channel_id).at(std::floor(feature.getX())+i, std::floor(feature.getY())+j));

  return dp;
}

unsigned int chooseFrameId(const MSMMProject::Project& p, MSMMDataModel::Track t) {
  const MSMMDataModel::ChannelConfig cc = p.get_channel_config();

  MSMMDataModel::TrackVectors<MSMMDataModel::ImType, double> tr_vec;
  t.create_vectors(cc, tr_vec, true);
  t.generate_levels(cc, tr_vec);

  std::vector<MSMMDataModel::Level> levels = t.get_levels();
  if (!levels.size()) throw std::runtime_error("No Frames");
  std::vector<unsigned int> frames;

  for (auto& level : levels)
    for (auto& range : level.get_ranges())
      for (unsigned int i = range.min(); i <= range.max(); i++)
        frames.push_back(i);


  return frames[(rand() % frames.size())];
}

int main(int argc, char** argv) {

  if (argc < 2) return -1;

  std::ifstream file(argv[1]);

  std::vector<std::string> datasets;
  while(file.good()) {

    std::string line;
    file >> line;

    if (line == "") continue;
    datasets.push_back(line);
  } // endwhile

  file.close();

  std::vector<std::vector<float>> features;

  for (const auto& ds : datasets) {
    // Prepare the project
    CppUtil::FracCounter counter;
    MSMMProject::Project project(ds);
    project.set_counter(counter);

    for (const auto& track : project.get_tracks()) {
      try {
	features.push_back(extract_point(project,
					 track.first,
					 chooseFrameId(project, track.second)));
      } catch (std::out_of_range e) {
	continue;
      } catch (std::runtime_error e) {
	continue;
      }
    }

  }

  std::ofstream data("features.csv");

  for (const auto& f : features) {
    for (unsigned int i = 0; i < f.size() - 1; i++)
      data << f[i]<<",";
    data << f.back() << std::endl;
  }
  data.close();

  return 0;
}
