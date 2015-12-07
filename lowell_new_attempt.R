# Lowell's attempt to copy and annotate the functionally necessary subset of Mihai's original code

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
myDescGrade <- 'A';	
A <- subset(DF, GPA >= 3.7 & nrC >= 7);
remIDs <- as.character(A$ID);

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
rownames(EmptyComp) <- unqCourses;
colnames(EmptyComp) <- unqCourses;

# Build the 3D-array of size nrCourses x nrCourses x nrStudents
for(stud in remIDs){
  COMPTD[,,stud] <- genMyComp(listStud[[stud]], EmptyComp);
}
print(dim(COMPTD))
