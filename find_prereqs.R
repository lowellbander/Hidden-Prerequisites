setwd('~/gits/Hidden-Prerequisites/')

load(file = 'ID_GPA_LIST.data')
# DF contains a mapping of [hashed] UID's to final GPA's

main = function() {
  
  nrCoursesStud = sapply(listStud, getLen);
  DFNRC = data.frame(nrC = nrCoursesStud);  rownames(DFNRC) = names(listStud);  print(head(DFNRC))
  DF$nrC = DFNRC[ rownames(DF) , 'nrC' ]
  print(head(DF,30));
  
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

