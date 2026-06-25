#include "stdafx.h"

NumericVector depthCurveTukey(List objects, List data, int nDirs = 100,
                              bool subs = true,
                              double fracInt = 0.5, double fracEst = 0.5, 
                              double parConst1 = 0, double parConst2 = 1, 
                              int parMode = 1,
                              Nullable<List> subsamples = R_NilValue,
                              bool exactEst = true,
                              double minMassObj = 0, double minMassDat = 0);
