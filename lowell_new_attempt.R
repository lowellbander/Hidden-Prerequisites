# Lowell's attempt to copy and annotate the functionally necessary subset of Mihai's original code

# a main function that does nearly everything and should perhaps be abstracted away.
generateComparisonMatrixForGPA <- function (minGPA, maxGPA) {
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
  relevantSubset <- subset(DF, GPA >= minGPA & GPA <= maxGPA & nrC >= 7);
  remIDs <- as.character(relevantSubset$ID);

  # subsets listStud for desired GPA and nrc (?)
  #listStud <- listStud[remIDs];

  # generate requisite metadata:
  # * unqCourses, a sorted and unique list of all courses in the dataset
  # * nrUnqCourses, the number of unique courses in the data set (length of unqCourses)
  # * nrRemIDs, the number of students in the subset of students selected
  AllCourses <- NULL;
  for( stud in DF$ID){
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
    COMPTD[,,stud] <- generateComparisonMatrix(listStud[[stud]], EmptyComp);
  }

  # normalize the COMPTD prism to a matrix
  normalized <- normalize(COMPTD, EmptyComp)
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

serialRank = function(nmatrix) {
  names = colnames(nmatrix)
  C = nmatrix
  size = dim(nmatrix)
  diag(C) <- 1
  
  S = matrix(data=NA, ncol=size[1], nrow=size[2]);
  
  onesMatrix = matrix(data=1, nrow=size[1], ncol=size[2]);
  S = (1/2)*(size[1]*(onesMatrix %*% t(onesMatrix)) + C %*% t(C));
  
  D = matrix(data=0, nrow=size[1], ncol=size[2]);
  for (i in 1:size[1]) {
    sum = 0
    for (j in 1:size[2]) {
      sum = sum + S[i, j]
    }
    D[i,i] = sum
  }
  
  L = D - S;
  
  eigenL = eigen(L);
  print (eigenL$vectors);
  nonzeroEigenvalue = eigenL$values[eigenL$values != 0];
  Fiedler = eigenL$vectors[,length(nonzeroEigenvalue)-1,drop=FALSE];
  sortedFiedler = sort(Fiedler)
  
  final = matrix(data=0, nrow=size[1], ncol=1)
  
  for (i in 1:size[1]) {
    for (j in 1:size[2]) {
      if (sortedFiedler[i] == Fiedler[j]) {
        final[i, 1] = names[j]
      }
    }
  }
  print(final)
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
generateComparisonMatrix = function (courses, emptyComparisonMatrix) {

  # Our comparison matrix, C, initialized to empty.
  C <- emptyComparisonMatrix;
  n <- dim(emptyComparisonMatrix)[1]

  cols <- colnames(C)
  rows <- rownames(C)

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        C[i,j] <- 1;
      } else {
        C[i,j] <- compare(cols[i], rows[j], courses);
      }
    }
  }
  return(C);
}

main = function () {
  A = generateComparisonMatrixForGPA(3.7, 4.0);
  C = generateComparisonMatrixForGPA(1.7, 2.3);
  print(A-C);
  print(dim(A))
  print(dim(C))
}

main();
