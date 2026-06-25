//*--------------------------------------------------------------------------*//
//  File:               depthsSetHfsp.cpp
//  Created by:         Pavlo Mozharovskyi and Myriam Vimond
//  First released:     05.11.2021
//
//  Contains R-level Rcpp-interfaced functions for computing Tukey set depth
//  and all required for this routines.
//
//  For a description of the algorithms, see:
//  Lafaye De Micheaux, P., Mozharovskyi, P. and Vimond, M. (2018).
//  Depth for curve data and applications.
//
//  Subsequent changes are listed below:
//*--------------------------------------------------------------------------*//

// [[Rcpp::depends(RcppArmadillo)]]

#include "stdafx.h"
#include "depthsCurveHfsp.h"

// [[Rcpp::export(sample.sets)]]
List setsSubsample(List sets, IntegerVector ptsPerSet =
  IntegerVector::create(500), bool randomPoints = true){
  // 0. Check input data
  int dim = 2; // WARNING: Only dimension 2 implemented by now
  // TODO: Implement for dim > 2
  // Rcout << ptsPerSet << "\n";
  // 1. Calculate number of points for each set
  List res = List(); // the structure to collect all results
  int* nsPoints = new int[sets.length()];
  if (ptsPerSet.length() == sets.length()){ // if given, ...
    for (int i = 0; i < sets.length(); i++){
      nsPoints[i] = ptsPerSet(i); // just copy
    }
  }else{ // if not given, then ...
    // first entry of 'ptsPerSet' is the average number of points per set
    for (int i = 0; i < sets.length(); i++){
      nsPoints[i] = ptsPerSet(0);
    }
  }
  // Rcout << "Calculated the numbers of points to be generated.\n";
  // 2. Draw points on sets
  for (int i = 0; i < sets.length(); i++){ // for each input set
    // 2.1. Copy data to a quick array (and find maximal value)
    NumericMatrix setImageVals = as<List>(sets(i))["image"];
    int dim1 = setImageVals.nrow();
    int dim2 = setImageVals.ncol();
    double* doubleCurSet = new double[dim1 * dim2];
    int maxCurSet = 0;
    for (int iDim1 = 0; iDim1 < dim1; iDim1++){
      for (int iDim2 = 0; iDim2 < dim2; iDim2++){
        doubleCurSet[iDim1 * dim2 + iDim2] = setImageVals(iDim1, iDim2);
        if (doubleCurSet[iDim1 * dim2 + iDim2] > maxCurSet){
          maxCurSet = doubleCurSet[iDim1 * dim2 + iDim2];
        }
      }
    }
    // Rcout << "Copied the data.\n";
    double offsetDim1 = as<NumericVector>(as<List>(sets(i))["offset"])(0);
    double offsetDim2 = as<NumericVector>(as<List>(sets(i))["offset"])(1);
    double stepDim1 = as<NumericVector>(as<List>(sets(i))["step"])(0);
    double stepDim2 = as<NumericVector>(as<List>(sets(i))["step"])(1);
    // Rcout << "Read offsets and steps.\n";
    // 2.2. Create a structure for saving generated points
    NumericMatrix curPoints = NumericMatrix(nsPoints[i], dim);
    // 2.3. Draw and store the points
    if (randomPoints){
      Environment stats_env("package:stats");
      Function stats_runif = stats_env["runif"];
      Environment base_env("package:base");
      Function base_sampleint = base_env["sample.int"];
      int curNPoints = 0;
      // Rcout << "Just before the generation loop.\n";
      while (curNPoints < nsPoints[i]){ // Draw points by batches
        // 2.3.1. Draw next batch of points
        int batchSize = nsPoints[i] - curNPoints; // how many points to draw now
        NumericVector rcppPointsDens(batchSize); // intensity
        rcppPointsDens = stats_runif(batchSize, 0, maxCurSet);
        IntegerVector rcppPointsDim1(batchSize); // dim1
        rcppPointsDim1 = base_sampleint(dim1, batchSize, true);
        IntegerVector rcppPointsDim2(batchSize); // dim2
        rcppPointsDim2 = base_sampleint(dim2, batchSize, true);
        for (int k = 0; k < batchSize; k++)
          // 2.3.2. Check whether to keep the point
          if (rcppPointsDens[k] < 
            doubleCurSet[(rcppPointsDim1[k] - 1) * dim2 + (
                rcppPointsDim2[k] - 1)]){
            // 2.3.3. Save the point
            curPoints(curNPoints, 0) = offsetDim1 + 
              (rcppPointsDim1[k] - 1) * stepDim1;
            curPoints(curNPoints, 1) = offsetDim2 + 
              (rcppPointsDim2[k] - 1) * stepDim2;
            curNPoints++;
          }
      }
    }else{
      // TODO: What to do in this case? Is it even necessary?
    }
    // 2.4. Add the retained point to the list
    List curCurve = List();
    curCurve.push_back(curPoints, "coords");
    res.push_back(curCurve);
    // 2.5. Release temporary memory structures
    delete[] doubleCurSet;
  }
  delete[] nsPoints;
  return res;
}

// [[Rcpp::export(sample.pointsSets)]]
List pointsSetsSubsample(List sets, IntegerVector ptsPerSet =
  IntegerVector::create(500)){
  // 0. Read input data and prepare output structures
  NumericMatrix rcppCurveVals = as<List>(sets(0))["coords"];
  int dim = rcppCurveVals.ncol(); // space dimension
  // 1. Calculate number of points for each set
  List res = List(); // the structure to collect all results
  int* nsPoints = new int[sets.length()];
  if (ptsPerSet.length() == sets.length()){ // if given, ...
    for (int i = 0; i < sets.length(); i++){
      nsPoints[i] = ptsPerSet(i); // just copy
    }
  }else{ // if not given, then ...
    // first entry of 'ptsPerSet' is the average number of points per set
    for (int i = 0; i < sets.length(); i++){
      nsPoints[i] = ptsPerSet(0);
    }
  }
  // Rcout << "Calculated the numbers of points to be generated.\n";
  Environment base_env("package:base");
  Function base_sampleint = base_env["sample.int"];
  // 2. Draw points on sets
  for (int i = 0; i < sets.length(); i++){ // for each input set
    // Rcout << "Srating for set" << i << ".\n";
    NumericMatrix setPoints = as<List>(sets(i))["coords"];
    int nSetPoints = setPoints.nrow();
    // 2.1. Create a structure for saving generated points
    NumericMatrix curPoints = NumericMatrix(nsPoints[i], dim);
    // 2.2. Draw and store the points
    IntegerVector rcppDrawnIndices(nsPoints[i]);
    // Rcout << "Points samples.\n";
    rcppDrawnIndices = base_sampleint(nSetPoints, nsPoints[i], true);
    for (int j = 0; j < nsPoints[i]; j++){
      for (int k = 0; k < dim; k++){
        curPoints(j, k) = setPoints(rcppDrawnIndices[j] - 1, k);
      }
    }
    // Rcout << "Points saved.\n";
    // 2.3. Add the retained point to the list
    List curCurve = List();
    curCurve.push_back(curPoints, "coords");
    res.push_back(curCurve);
  }
  delete[] nsPoints;
  return res;
}

// [[Rcpp::export(depths.Tukey)]]
NumericVector depthSTukey(List objects, List data, String inputType = "points", 
                          int nDirs = 100, bool subs = true, int m = 500,
                          double fracInt = 0.5, double fracEst = 0.5,
                          bool exactEst = true, 
                          double parConst1 = 0, double parConst2 = 1, 
                          int parMode = -1, 
                          double minMassObj = 0, double minMassDat = 0,
                          bool randObj = true, bool randDat = true){
  // !ALWAYS! keep "subs = false" in the call of the internal function
  if (inputType == "points"){
    if (subs){
      List tmpObj = pointsSetsSubsample(objects, IntegerVector(1, m / fracEst *
        (fracInt + fracEst)));
      List tmpDat = pointsSetsSubsample(data, IntegerVector(1, m));
      return depthCurveTukey(tmpObj, tmpDat, nDirs, subs = false, fracInt, fracEst, 
                             parConst1, parConst2, parMode, 
                             R_NilValue, exactEst, minMassObj, minMassDat);
    }else{
      return depthCurveTukey(objects, data, nDirs, subs = false, fracInt, fracEst, 
                             parConst1, parConst2, parMode, 
                             R_NilValue, exactEst, minMassObj, minMassDat);
    }
  }
  if (inputType == "image"){
    List tmpObj = setsSubsample(objects, IntegerVector(1, m / fracEst *
      (fracInt + fracEst)), randObj);
    List tmpDat = setsSubsample(data, IntegerVector(1, m), randDat);
    return depthCurveTukey(tmpObj, tmpDat, nDirs, subs = false, fracInt, fracEst,
                           parConst1, parConst2, parMode, 
                           R_NilValue, exactEst, minMassObj, minMassDat);
  }
  return "Unknown data type";
}
