setwd('~/gits/Hidden-Prerequisites/')

load(file = 'ID_GPA_LIST.data')
rownames(DF) = as.character(DF$ID);
# DF contains a mapping of [hashed] UID's to final GPA's
# listStud is a mapping of [hashed] UID's  to the ordering of courses taken

main = function() {
  #printMeta()
  createGPAhist()
  createCoursesTakenHistogram()
  distributionOfCoursesTakenAndOtherThings()  
}

distributionOfCoursesTakenAndOtherThings = function(){
  # subset by GPA and maybe number of courses
  myDescGrade = 'A';	
  A = subset(data,  GPA >= 3.7 & nCoursesTaken >= 7  );
  # myDescGrade = 'C';	A = subset(DF,  GPA >= 1.7 & GPA <= 2.3 &  nrC >= 7);
  
  print(head(A))
  #print(dim(A));
  remIDs = as.character(A$ID);
  listStud = listStud[ remIDs ];
  
  AllCourses = NULL;
  for( stud in remIDs){
    AllCourses = c(AllCourses, listStud[[ stud ]] );
  }
  sortedFreqCourses = sort(table(AllCourses));
  print(sortedFreqCourses);
  readline('see sort table all courses. Press key to continue...');
  
  unqCourses = sort(unique(AllCourses));  print(unqCourses)
  nrUnqCourses = length(unqCourses);
  print( paste( 'nrUnqCourses=', nrUnqCourses ) )
  
  nrRemIDs = length(remIDs);  print(paste('nrRemIDs=', nrRemIDs))
  print(unqCourses); readline('see unqCourses.   Press key to continue...'); 
  
  pdf(paste('CourseFreq_',myDescGrade,'_students.pdf', sep='') );
  barplot(sortedFreqCourses, las = 2, cex.names = 0.5, main = paste('Distribution of courses taken. \n  nr Stud=', nrRemIDs,'; grade= ',myDescGrade,'; nrUniqueCourses=', nrUnqCourses) , horiz=TRUE);
  dev.off();
}

# creates a histogram showing the frequency of courses taken by a student
createCoursesTakenHistogram = function() {
  coursesTakenPerStudent = data.frame(nCoursesTaken = sapply(listStud, length));  
  rownames(coursesTakenPerStudent) = names(listStud);  
  data = DF
  data$nCoursesTaken = coursesTakenPerStudent[ rownames(DF) , 'nCoursesTaken' ]
  #print(head(DF,10)); # sanity check
  
  # subset students only with > 5 courses taken
  data = subset(data, nCoursesTaken > 5)
  
  pdf('HistNRCoursesTaken.pdf');
  hist(data$nCoursesTaken, breaks=20, main='Histogram of courses taken per student', ylab = ' Nr courses '); 
  dev.off();
  #print(sort(table(data$nCoursesTaken)))
}

# prints meta information about the data
printMeta = function() {
  print(length(listStud)) # print the how many students are in the table
  print(names(listStud)) # print the UID's of the students
  print(dim(DF))
}

# creates a histogram of the distribution of GPA's
createGPAhist = function () {
  pdf('HistGrades.pdf');
  hist(DF$GPA, breaks=100, main='Histogram of GPA grades');
  dev.off();
}

main()

