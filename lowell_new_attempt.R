# Lowell's attempt to copy and annotate the functionally necessary subset of Mihai's original code

# a main function that does nearly everything and should perhaps be abstracted away.
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
  rownames(DFNRC) <- names(listStud); # why is this necessary?
  DF$nrC <- DFNRC[rownames(DF), 'nrC']

  # generate a subset of DF with only A range students who have taken at least 7 math courses
  relevantSubset <- subset(DF, GPA >= minGPA & GPA <= maxGPA & nrC >= 5);
  remIDs <- as.character(relevantSubset$ID);

  # subsets listStud for desired GPA and nrc (?)
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
  return(final)
}

rankCentrality = function(nmatrix) { 
  A <- data.matrix(nmatrix) # Aij = the number of times j occurs before i
  dmax = (dim(A)[1] - 1)  #not sure if this should be the max of a particular node of or of all nodes
                          #assuming I can just use the dim - 1
  
  P = (1/dmax) * A
  for (i in 1:dim(P)[1]) {
    P[i,i] = 1 - sum(P[i,]);
  }
  
  #we need the top left eigenvector, not sure if this is right
  P_eigen <- eigen(t(P)) #left eigenvectors
 # P_eigenright <- eigen(P) right eigenvectors
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
kendall = function (firstOrdering, secondOrdering, normalize = FALSE) {
  nDisagreements <- 0;
  len <- length(firstOrdering);
  
  # for all pairs of courses
  for (i in 1:len) {
    for (j in i:len) {
      thisElement <- firstOrdering[i];
      thatElement <- firstOrdering[j];
      
      thisPositionFirstOrdering <- match(thisElement, firstOrdering);
      thatPositionFirstOrdering <- match(thatElement, firstOrdering);
      thisPositionSecondOrdering <- match(thisElement, secondOrdering);
      thatPositionSecondOrdering <- match(thatElement, secondOrdering);
      
      if ((thisPositionFirstOrdering < thatPositionFirstOrdering & thisPositionSecondOrdering > thatPositionSecondOrdering)
          | (thisPositionFirstOrdering > thatPositionFirstOrdering & thisPositionSecondOrdering < thatPositionSecondOrdering)) {
        nDisagreements <- nDisagreements + 1;
      }
    }
  }
  
  if (normalize) {
    nDisagreements <- nDisagreements/(len*(len-1)/2);
  }
  
  return(nDisagreements);
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
  
  k <- kendall(serialRank(A_pruned), serialRank(C_pruned), TRUE);
  
  a <- generateComparisonMatrixForGPA(3.7, 4.0, forSerialRank = FALSE, reducer = flatten);
  
  A <- from_a_to_A(a);
}

main();
