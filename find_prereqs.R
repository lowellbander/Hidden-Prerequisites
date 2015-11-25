setwd('~/gits/Hidden-Prerequisites/')

load(file = 'ID_GPA_LIST.data')
rownames(DF) = as.character(DF$ID);
# DF contains a mapping of [hashed] UID's to final GPA's

main = function() {
  printMeta()
  createGPAhist()
  createCoursesTakenHistogram()
}

# creates a histogram showing the frequency of courses taken by a student
createCoursesTakenHistogram = function() {
  coursesTakenPerStudent = data.frame(numberOfCoursesTaken = sapply(listStud, length));  
  rownames(coursesTakenPerStudent) = names(listStud);  
  data = DF
  data$numberOfCoursesTaken = coursesTakenPerStudent[ rownames(DF) , 'numberOfCoursesTaken' ]
  #print(head(DF,10)); # sanity check
  
  # subset students only with > 5 courses taken
  data = subset(data, numberOfCoursesTaken > 5)
  
  pdf('HistNRCoursesTaken.pdf');
  hist(data$numberOfCoursesTaken, breaks=20, main='Histogram of courses taken per student', ylab = ' Nr courses '); 
  dev.off();
  #print(sort(table(data$numberOfCoursesTaken)))
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

