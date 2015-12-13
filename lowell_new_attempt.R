
# generates comparison matrix for Serial Rank, Rank Centrality, and Least Squares algorithms
generateComparisonMatrixForGPA <- function (minGPA, maxGPA, forSerialRank = TRUE, reducer = normalize) {
  # this generates DF and listStud
  # DF is a mapping of ID's to GPA's. (there are several GPA's of 4.3 wtf)
  # listStud is a jagged mapping of ID's to course orderings.
  load(file = 'ID_GPA_LIST.data');

  # necessary. maybe something about string matching. without this, adding the nrC column to DF fails.
  rownames(DF) <- as.character(DF$ID);

  # creates a mapping of ID's to the number of courses taken by the student with that ID
  nrCoursesStud = sapply(listStud, length);
  DFNRC <- data.frame(nrC = nrCoursesStud);
  rownames(DFNRC) <- names(listStud);
  DF$nrC <- DFNRC[rownames(DF), 'nrC']

  # generate a subset of DF with only A range students who have taken at least 7 math courses
  relevantSubset <- subset(DF, GPA >= minGPA & GPA <= maxGPA & nrC >= 5);
  remIDs <- as.character(relevantSubset$ID);

  # subsets listStud for desired GPA and nrc
  listStud <- listStud[remIDs];

  # generate requisite metadata:
  # * unqCourses, a sorted and unique list of all courses in the dataset
  # * nrUnqCourses, the number of unique courses in the data set (length of unqCourses)
  # * nrRemIDs, the number of students in the subset of students selected
  AllCourses <- NULL;
  for( stud in remIDs){
    AllCourses <- c(AllCourses, listStud[[stud]]);
  }
  unqCourses <- sort(unique(AllCourses));
  nrUnqCourses <- length(unqCourses);
  nrRemIDs <- length(remIDs);

  #initialize 3D-array of size nrCourses x nrCourses x nrStudents
  COMPTD <- array(0, dim = c(nrUnqCourses, nrUnqCourses, nrRemIDs));
  dimnames(COMPTD) <- list(unqCourses, unqCourses, remIDs);
  EmptyComp <- array(0, dim = c(nrUnqCourses, nrUnqCourses));
  dimnames(EmptyComp) <- list(unqCourses, unqCourses);

  # Build the 3D-array of size nrCourses x nrCourses x nrStudents
  for(stud in remIDs){
    COMPTD[,,stud] <- generateComparisonMatrix(listStud[[stud]], EmptyComp, forSerialRank);
  }

  # normalize the COMPTD prism to a matrix
  return(reducer(COMPTD, EmptyComp));
}

normalize = function (prism, emptyComparisonMatrix) {
  normalized <- emptyComparisonMatrix;
  dimensions <- dim(prism)
  for (i in 1:dimensions[1]) {
    for (j in 1:dimensions[2]) {
      values <- vector()
      for (k in 1:dimensions[3]) {
        if (prism[i,j,k] != 0) {
          values <- c(values, prism[i,j,k])
        }
      }
      mn <- mean(values);
      if (is.nan(mn)) {
        normalized[i,j] <- 0;
      } else {
        normalized[i,j] <- mn;
      }
    }
  }
  return(normalized);
}

flatten = function (prism, emptyComparisonMatrix) {
  flattened <- emptyComparisonMatrix;
  dimensions <- dim(prism)
  for (i in 1:dimensions[1]) {
    for (j in 1:dimensions[2]) {
      values <- vector()
      for (k in 1:dimensions[3]) {
        if (prism[i,j,k] != 0) {
          values <- c(values, prism[i,j,k])
        }
      }
      flattened[i,j] <- sum(values);
    }
  }
  return(flattened);
}

serialRank = function(nmatrix) {
  names = colnames(nmatrix)
  n<-dim(nmatrix)[1]
  C = data.matrix(nmatrix)
  
  diag(C) <- 1
  
  S = matrix(data=NA, ncol=n, nrow=n);
  
  onesMatrix = matrix(data=1, nrow=n, ncol=n);
  S = (1/2)*(n*(onesMatrix) + (C %*% t(C)));
  
  D = matrix(data=0, nrow=n, ncol=n);
  for (i in 1:n) {
    sum = 0
    for (j in 1:n) {
      sum = sum + S[i, j]
    }
    D[i,i] = sum
  }
  
  L = D - S;
  
  eigenL = eigen(L);
  nonzeroEigenvalue = eigenL$values[eigenL$values != 0];
  Fiedler = eigenL$vectors[,length(nonzeroEigenvalue)-1,drop=FALSE];
  sortedFiedler = sort(Fiedler)
  
  final = matrix(data=0, nrow=n, ncol=1)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (sortedFiedler[i] == Fiedler[j]) {
        final[i, 1] = names[j]
      }
    }
  }
  return(as.vector(final));
}

rankCentrality = function(nmatrix) {
  names = colnames(nmatrix)
  n<-dim(nmatrix)[1]
  A <- data.matrix(nmatrix) # Aij = the number of times j occurs before i
  #dmax = (dim(A)[1] - 1)  #not sure if this should be the max of a particular node of or of all nodes
                          #assuming I can just use the dim - 1
  
  dmax = matrix(data=0, nrow=n, ncol=1)
  rowoutdegree = matrix(data=0, nrow=n, ncol=1)
  coloutdegree = matrix(data=0, nrow=n, ncol=1)

  for (i in 1:n){ # get row out degree
    count = 0
    for (j in 1:n){
      if (A[i,j] != 0){
        count = count+1
      }
    }
    rowoutdegree[i,1] = count-1
  }
  for (i in 1:n){ #get col out degree
    count = 0
    for (j in 1:n){
      if (A[j,i] != 0){
        count = count+1
      }
    }
    coloutdegree[i,1] = count-1
  }
  
  for (i in 1:n){ #get dmax
    dmax[i,1] = max(rowoutdegree[i,1],coloutdegree[i,1])
  }
  
  Pmat = matrix(data=0, nrow=n, ncol=n)
  rownames(Pmat) = names
  colnames(Pmat) = names
  P <- data.matrix(Pmat)
  
  for (i in 1:n){  #divide everything by dmax
    for (j in 1:n){
    P[i,j] <- A[i,j] / dmax[i,1]
    }
  }
  
  #P <- (1/dmax) * A
  for (j in 1:n) {
    P[j,j] <- 1 - sum(P[,j]);
  }
  

  #we need the top left eigenvector, not sure if this is right
  #P_eigen <- eigen(t(P))#left eigenvectors
  P_eigen <- eigen(P)#right eigenvectors
  #nonzeroEigenvalue = P_eigen$values[20];
  nonzeroEigenvalue = P_eigen$values[P_eigen$values != 0]
  #P_TopLeftEigenvector = P_eigen$vectors[,20]
  P_TopLeftEigenvector = P_eigen$vectors[,length(nonzeroEigenvalue)]
  #P_TopLeftEigenvector = P_eigen$vectors[,4]
  sortedP_LeftEigenVector = sort(P_TopLeftEigenvector)
  
  final = matrix(data=0, nrow=n, ncol=1)
  
  for (i in 1:n) { #just sorting the eigenvector and assign
    for (j in 1:n) {
      if (sortedP_LeftEigenVector[i] == P_TopLeftEigenvector[j]) {
        final[i, 1] = names[j]
      }
    }
  }
  return(final)
}

compare = function (this, that, courses) {
  thisPosition <- match(this, courses);
  thatPosition <- match(that, courses);

  if (is.na(thisPosition) | is.na(thatPosition)) {
    return(0);
  } else if (thisPosition < thatPosition){
    return(1);
  } else {
    return(-1);
  }
}

# courses: a list of unique strings. e.g.,  ["10A", "32B",  "33A", ...]
# emptyComparisonMatrix: an empty comparison matrix to fill
generateComparisonMatrix = function (courses, emptyComparisonMatrix, forSerialRank=TRUE) {

  # Our comparison matrix, C, initialized to empty.
  C <- emptyComparisonMatrix;
  n <- dim(emptyComparisonMatrix)[1]

  cols <- colnames(C)
  rows <- rownames(C)

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        if (forSerialRank) {
          C[i,j] <- 1;
        } else {
          C[i,j] <- 0;
        }
      } else {
        comparison <- compare(cols[i], rows[j], courses);
        if (!forSerialRank & comparison == -1) {
          comparison <- 0;
        }
        C[i,j] <- comparison;
      }
    }
  }
  return(C);
}

matrixSubset = function (m, headers) {
  columnsRemoved <- subset(m, select=headers);
  rowsRemoved <- subset(t(columnsRemoved), select=headers);
  return(t(rowsRemoved));
}

# computes the [normalized] kendall distance for two orderings
# firstOrdering is assumed to be a permutation of secondOrdering
kendall = function (firstOrdering, secondOrdering, normalize = TRUE) {
  nDisagreements <- 0;
  len <- length(firstOrdering);
  
  # for all pairs of courses
  for (i in 1:(len-1)) {
    for (j in (i+1):len) {
      
      thisElement <- firstOrdering[i];
      thatElement <- firstOrdering[j];
      
      thisPositionFirstOrdering <- match(thisElement, firstOrdering);
      thatPositionFirstOrdering <- match(thatElement, firstOrdering);
      thisPositionSecondOrdering <- match(thisElement, secondOrdering);
      thatPositionSecondOrdering <- match(thatElement, secondOrdering);
      
      a <- thisPositionFirstOrdering < thatPositionFirstOrdering;
      b <- thisPositionSecondOrdering > thatPositionSecondOrdering;
      c <- thisPositionFirstOrdering > thatPositionFirstOrdering;
      d <- thisPositionSecondOrdering < thatPositionSecondOrdering;
      
      if ((a & b)
          | (c & d)) {
        nDisagreements <- nDisagreements + 1;
      }
    }
  }
  
  if (normalize) {
    nDisagreements <- nDisagreements/(len*(len-1)/2);
  }
  
  return(nDisagreements);
}

leastSquaresRanking = function(preferenceMatrix) {
  n = dim(preferenceMatrix)[1];
  X = matrix(data=0, ncol=n, nrow=n*(n-1)/2);
  y = matrix(data=0, ncol=1, nrow=n*(n-1)/2);
  colnames(X) <- colnames(preferenceMatrix)
  k = 1
  for (i in 1:n) {
    j <- i+1
    while (j <= n) {
      y[k] <- abs(preferenceMatrix[i, j] - preferenceMatrix[j, i])
      if (preferenceMatrix[i, j] >= preferenceMatrix[j, i]) {
        X[k, i] <- -1
        X[k, j] <- 1
      }
      else {
        X[k, i] <- 1
        X[k, j] <- -1
      }
      k <- k+1
      j <- j+1
    }
  }
  
  XTX = t(X) %*% X
  XTy = t(X) %*% y
  
  for (i in 1:n) {
    XTX[n, i] <- 1;
  }
  y[n] <- 0
  r = solve(XTX, XTy)
  return(rownames(r[order(-r), , drop=FALSE]));
}

from_a_to_A = function (a) {
  A <- a;
  len <- dim(A)[1];
  
  for (i in 1:len) {
    for (j in 1:len) {
      denominator <- (a[i,j] + a[j,i]);
      if (denominator == 0) {
        A[i,j] <- 0;
      } else {
        A[i,j] <- a[i,j] / denominator;
      }
    }
  }
  return(A);
}

main = function () {
  A <- generateComparisonMatrixForGPA(3.7, 4.0);
  C <- generateComparisonMatrixForGPA(1.7, 2.3);
  commonCourses <- intersect(rownames(A), rownames(C));
  A_pruned <- matrixSubset(A, commonCourses);
  C_pruned <- matrixSubset(C, commonCourses);
  differenceMatrix <- A_pruned - C_pruned;
  
  k <- kendall(serialRank(A_pruned), serialRank(C_pruned));
  
  a <- generateComparisonMatrixForGPA(1.7, 2.3, forSerialRank = FALSE, reducer = flatten);
  LSR = leastSquaresRanking(a)
  print(LSR)
  A <- from_a_to_A(a);
}

#main();

doSynthetic = function () {
  
  # create synthetic comparison matrix
  courses <- c("33A", "33B", "115A", "164");
  C <- data.frame(a = 1:4, b = 5:8, c = 3:4, d = 6:9);
  rownames(C) <- colnames(C) <- courses;
  
  # serial rank
  sr <- serialRank(C);
  
  # rank centrality
  rc <- rankCentrality(C);
  
  # least squares
  ls <- leastSquaresRanking(C);
  
  sr; rc; ls;
  
  kendall(sr, rc);
  kendall(sr, ls);
  kendall(rc, ls);
}

doReal = function () {
  
  # compute rankings and requisite intermediate comparison matrices for A range students
  
  a_A <- generateComparisonMatrixForGPA(3.7, 4.0, forSerialRank = FALSE, reducer = flatten);
  LS_A <- leastSquaresRanking(a_A)
  
  A_A <- from_a_to_A(a_A);
  RC_A <- rankCentrality(A_A);
  
  C_A <- generateComparisonMatrixForGPA(3.7, 4.0, forSerialRank = TRUE, reducer = normalize);
  SR_A <- serialRank(C_A);
  
  # compute rankings and requisite intermediate comparison matrices for C range students
  
  a_C <- generateComparisonMatrixForGPA(1.7, 2.3, forSerialRank = FALSE, reducer = flatten);
  LS_C <- leastSquaresRanking(a_C)
  
  A_C <- from_a_to_A(a_C);
  RC_C <- rankCentrality(A_C);
  
  C_C <- generateComparisonMatrixForGPA(1.7, 2.3, forSerialRank = TRUE, reducer = normalize);
  SR_C <- serialRank(C_C);
  
  # compute kendall tau distances between each of the three rankings, by GPA bucket
  
  # for A bucket
  kendall_ls_sr_A <- kendall(LS_A, SR_A);
  # kendall_ls_rc_A <- kendall(LS_A, RC_A); # was causing problems
  kendall_rc_sr_A <- kendall(RC_A, SR_A);
  
  # for C bucket
  kendall_ls_sr_A <- kendall(LS_C, SR_C);
  kendall_ls_rc_A <- kendall(LS_C, RC_C);
  kendall_rc_sr_A <- kendall(RC_C, SR_C);
  
}

#doSynthetic();

doReal();
