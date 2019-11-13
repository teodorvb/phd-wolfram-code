#include <H5Cpp.h>
#define SAMPLE 1
#define BIAS 2
#define BEAD 3

void createStringAttribute(H5::DataSet& ds,
			   const char* name,
			   const char* value) {
  ds.createAttribute(name,
		     H5::StrType(H5T_STRING, H5T_VARIABLE),
		     H5::DataSpace(H5S_SCALAR))
    .write(H5::StrType(H5T_STRING, H5T_VARIABLE),
	   H5std_string(value)); 

}

void createIntegerAttribute(H5::DataSet& ds,
			    const char* name,
			    long int value) {
  ds.createAttribute(name,
		     H5::PredType::NATIVE_INT64,
		     H5::DataSpace(H5S_SCALAR))
    .write(H5::PredType::NATIVE_INT64, &value);
}

void export_hdf5(const char* fname,
		 double* frame,
		 int frame_size_x,
		 int frame_size_y,
		 long int frameid,
		 int frameType) {

  H5::H5File file(fname, H5F_ACC_TRUNC);
  H5::Group root = file.openGroup("/");
  float *frame_float = new float[frame_size_x*frame_size_y];
  for (int i = 0; i < frame_size_x*frame_size_y; i++)
    frame_float[i] = frame[i];
  
  hsize_t attr_dims[2];
  attr_dims[0] = frame_size_y;
  attr_dims[1] = frame_size_x;
  
  H5::DataSet image = root.createDataSet("image",
					 H5::PredType::NATIVE_FLOAT,
					 H5::DataSpace(2, attr_dims));

  image.write(frame_float, H5::PredType::NATIVE_FLOAT);
  
  createIntegerAttribute(image, "FrameId", frameid);
  if (frameType == SAMPLE)
    createStringAttribute(image, "FrameRole", "FrameRole:Sample");
  else if (frameType == BIAS)
    createStringAttribute(image, "FrameRole", "FrameRole:Bias");
  else if (frameType == BEAD)
    createStringAttribute(image, "FrameRole", "FrameRole:Bead");

  createStringAttribute(image, "MSMM_HDF5_Raw_Version", "01.005");  
  createStringAttribute(image, "MSMM_HDF5_Type", "RawImage");  
  createIntegerAttribute(image, "SubFrameId", (long int)1);

  file.close();

}
