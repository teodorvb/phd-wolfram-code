#include<wstp.h>
#include<math.h>
#include<stdlib.h>
#include "export_hdf5.hpp"

:Begin:
:Function:      export_flimp
:Pattern:       ExportFLImP[fname_String, frame:{___Real}, frameSizeX_Integer, frameSizeY_Integer, frameid_Integer, frameType_Integer]
:Arguments:     {fname, frame, frameSizeX, frameSizeY, frameid, frameType}
:ArgumentTypes: {UCS2String, RealList, Integer, Integer, Integer, Integer}
:ReturnType:    Integer
:End:
:Evaluate:      ExportFLImP::usage = "Exports FLImP frame to HDF5"



int export_flimp(unsigned short const* fname,
    		 int fname_size,
     		  double* frame,
		  long int datasize,
		  int frameSizeX,
		  int frameSizeY,
		  int frameid,
		  int frameType) {
  if (datasize != frameSizeX*frameSizeY) throw("error");

  char * fnameC = new char[fname_size];
  for (int i = 0; i < fname_size; i++) {
    fnameC[i] = fname[i];
  }

  export_hdf5(fnameC,
	      frame,
	      frameSizeX,
	      frameSizeY,
	      frameid,
	      frameType);
  return 0;
}


int main(int argc, char **argv) {
  return WSMain(argc, argv);
}
