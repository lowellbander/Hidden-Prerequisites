
# this generates DF and listStud
# DF is a mapping of ID's to GPA's. (there are several GPA's of 4.3 wtf)
# listStud is a jagged mapping of ID's to course orderings.
load(file = 'ID_GPA_LIST.data');

# necessary. maybe something about string matching. without this, adding the nrC column to DF fails.
rownames(DF) = as.character(DF$ID);

# creates a mapping of ID's to the number of courses taken by the student with that ID
nrCoursesStud = sapply(listStud, length);
DFNRC = data.frame(nrC = nrCoursesStud);
rownames(DFNRC) = names(listStud); # why is this necessary?
DF$nrC = DFNRC[ rownames(DF) , 'nrC' ]

# generate a subset of DF with only A range students who have taken at least 7 math courses
myDescGrade <- 'A';	
A <- subset(DF,  GPA >= 3.7 & nrC >= 7  );

remIDs <- as.character(A$ID);

# Build the the 3D-array of size  nrCourses x nrCourses x nrStudents
for(stud in remIDs){
  COMPTD[,,stud] <- genMyComp(listStud[[stud]], EmptyComp);
}
print(dim(COMPTD))
