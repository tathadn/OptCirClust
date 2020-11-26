# NEWS

## Version 0.0.1
    
    2020-11-25
  To-do:
  
  1. Define the meaning of frame.width in the user document and the details 
  section. 

    2020-11-24

  1. Updated the package title and description. 
    


    

  
  

  
  
    2020-10-06
  
   1. Created a unified function for framed clustering 
  FramedClust(
    X, K, frame.width,     
    first.frame = 1,
    last.frame = length(X)-frame.width+1, 
    method = c("linear.polylog", "kmeans", "Ckmeans.1d.dp")
  )
  
  2. create an R interface to lin_polylog_framed_clust 
  
  (do we need this seperate function, we are handeling all the framed clusters in the FramedClust function)
  
  lin.polylog.framed.clust(X, K, frame.width, first.frame, last.frame), inside which you call lin_polylog_framed_clust()
  
  For lin.polylog.framed.clust, the indices must be 1-based to be consistent with R;
  For lin_polylog_framed_clust, the indices must be 0-based to be consistent with C/C++.
  
  
    2020-09-29
  
  1. Do not export lin_polylog_framed_clust(), 
     kmeans.framed.clust(), quad.framed.clust() functions. Thus, we do not have to maintain three manuals that look almost identical.
     
  2. Reorder/rename the arguments to
  
  lin_polylog_framed_clust(X, K, frame_width, first_frame, last_frame, prev, next)
  kmeans.framed.clust(X, K, frame.width, first.frame, last.frame)
  quad.framed.clust(X, K, frame.width, first.frame, last.frame)
  
  
  
    2020-09-13
  
  1. Completed the manual for function lin_polylog_framed_clust.
  2. Wrote the description of the package.
  3. Created a README.md file.
  4. Tested the kmeans.framed.clust() function.
    
    
    2020-08-11
    
  1. Package name changed to OptCirClust to avoid confusion with a
     similar R package.
  
    2020-08-10
  
  1. Added more tests for function CirClust(), such as irregular input data.
  2. Changed the for-loops in CirClust() to vectorized operations.
  3. Corrected the mean in OCC.
  
  
    2020-08-08
  
  1. Created CirClust version 0.0.1 from optCclust version 0.1.0
  2. Renamed function Cir.Clust() to CirClust() to reduce confusion.
     This function returns an object of class "CirClust".
  3. Renamed C/C++ function FOCC_Interface() to 
     lin_polylog_framed_clust().
  4. Renamed file "FOCC_Interface.cpp" to 
     "lin_polylog_framed_clust.cpp"
  5. Exported lin_polylog_framed_clust as a function for the package.

  
  
  2020-08-08
  
  1. Shall we remove the following two arguments  (Yes)
       prev_k_f = -1,
       next_k_f = -1
       from FramedClust? I would think the user will never
       use them. Is so, will they cause confusion to the user?
  
  
