//
//  OptClust.hpp
//  OptClust_Log_Linear
//
//  Created by Tathagata Debnath on 6/15/20.
//  Copyright © 2020 Tathagata Debnath. All rights reserved.
//


//
//  OptClust.h
//  OptClust_Log_Linear
//
//  Created by Tathagata Debnath on 6/15/20.
//  Copyright © 2020 Tathagata Debnath. All rights reserved.
//



#include <stdio.h>
#include <vector>
#include <climits>
#include <algorithm>
#include <cmath>
#include <cassert>
#include <string>
#include <cstring>
#include <list>
#include <iostream>
using namespace std;

struct frame_info
{
    double ssq = 1.79769e+308;
    int Frame_ID = -1;

};

struct clustering
{
  int Frame_ID = -1;
  double ssq = 1.79769e+308;
  std::vector<int> Borders;
  std::vector<double> centers;
  std::vector<double> size;
  double totss = 1.79769e+308;
  std::vector<double> withinss;


};

clustering MFC( std::vector<double> & Data_Points,
                 int width, int K,
                 int First, int Last,
                 int Prev, int Next);

frame_info BDP( int width, int K,
                int First, int Last,
                int Prev, int Next,
                std::vector< std::vector< double > > &  S,
                std::vector< std::vector< int > > & J,
                const std::vector<double> & sum_x,
                const std::vector<double> & sum_x_sq,
                std::vector< std::vector<int> > & Cluster_Border);


void linear_clustering( std::vector< std::vector< double > > & S,
                        std::vector< std::vector< int > > & J,
                        int Prev, int Next, int Middle_Frame,
                        const std::vector<double> & sum_x,
                        const std::vector<double> & sum_x_sq,
                        std::vector< std::vector<int> > & Cluster_Border
);


void fill_row_k(int imin, int imax, int k, int Middle_Frame,
                int jmin, int jmax,
                std::vector< std::vector<double> > & S,
                std::vector< std::vector<int> > & J,
                const std::vector<double> & sum_x,
                const std::vector<double> & sum_x_sq);


/*
void fill_row_q_2020_07_22(int imin, int imax, int q, int imin_0, int imax_0, int Middle_Frame,
                           int jmin, int jmax,
                           std::vector< std::vector<double> > & S,
                           std::vector< std::vector<int> > & J,
                           const std::vector<double> & sum_x,
                           const std::vector<double> & sum_x_sq);
*/


double ssq(const int j, const int i, const int Middle_Frame,
           const std::vector<double> & sum_x, // running sum of xi
           const std::vector<double> & sum_x_sq // running sum of xi^2
);




void backtrack (std::vector< std::vector<int> > & J,
                std::vector<int> & B,
                int K, int N);






