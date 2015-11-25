# source("/Users/Mihai/Google Drive/UCLA_Courses/191_Data_Science_Fall_2015/AAA_Projects/Ranking_courses/math_courses_ucla.r");     math_courses_ucla();

math_courses_ucla = function(){

setwd('/Users/Mihai/Google Drive/UCLA_Courses/191_Data_Science_Fall_2015/AAA_Projects/Ranking_courses/');

load(file = 'ID_GPA_LIST.data');

# listStud :   a list where each element corresponds to a matrix (associated to each student)
print(length(listStud))
print(names(listStud))  # these are the student ID (hash value)
print(listStud[[ 1 ]]);  # first student
print(listStud[ 1:10 ]);  # first 10 students


# DF: a data frame with info on GPA and nrCourses taken
print(dim(DF))
rownames(DF) = as.character(DF$ID);		print(head(DF,30));


pdf('HistGrades.pdf');
hist(DF$GPA, breaks=100, main='Histogram of GPA grades');
dev.off();

nrCoursesStud = sapply(listStud, getLen);
DFNRC = data.frame(nrC = nrCoursesStud);  rownames(DFNRC) = names(listStud);  print(head(DFNRC))
DF$nrC = DFNRC[ rownames(DF) , 'nrC' ]
print(head(DF,30));
# print(nrCoursesStud)

# subset students only with > 5 courses taken
DF = subset(DF, nrC > 5 );

pdf('HistNRCoursesTaken.pdf');
# hist(log2(DF$nrC), breaks=20, main='Histogram of nr courses taken', ylab = 'log_2 (Nr courses)'); 
hist(DF$nrC, breaks=20, main='Histogram of nr courses taken', ylab = ' Nr courses '); 
dev.off();
print(sort(table(DF$nrC)))

# subset by GPA and maybe number of courses
myDescGrade = 'A';	A = subset(DF,  GPA >= 3.7 & nrC >= 7  );
# myDescGrade = 'C';	A = subset(DF,  GPA >= 1.7 & GPA <= 2.3 &  nrC >= 7);

print(head(A))
print(dim(A));
remIDs = as.character(A$ID);
listStud = listStud[ remIDs ];
# print(listStud);

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


COMPTD = array(0, dim = c(nrUnqCourses, nrUnqCourses, nrRemIDs) ); 	dimnames(COMPTD) = list(unqCourses, unqCourses, remIDs);

EmptyComp = array(0, dim = c(nrUnqCourses, nrUnqCourses) ); 	rownames(EmptyComp) = unqCourses;  	colnames(EmptyComp)= unqCourses;


# Build the the 3D-array of size  nrCourses x nrCourses x nrStudents
for(stud in remIDs){
	COMPTD[ ,,stud] = genMyComp(  listStud[[ stud ]] , EmptyComp );
}
print(dim( COMPTD ))


COMP = apply(COMPTD, c(1,2), myRho)

# "2" ,   "13" ,
myOrd = c( "1" ,  "3A" ,  "3B" ,  "3C",  "10A"  , "10B" ,  "10C" ,     "15" , "20",  "20A" ,  "20B",   "30", "31A" ,  "31B" ,  "31E", "32A" ,  "32AH" , "32AL",  "32B" ,  "32BL",  "33A",   "33B" ,  "35AH",  "35BH",  "35CH",  "38A",  "38B",   "40" ,   "60",   "61", "104",   "106" ,     "110" ,  "110A",  "110AH", "110B",  "110BH" ,"110C",  "111" ,  "113" ,  "114A",  "115A",  "115AH", "115B",  "117" , "120A" , "120B",  "121",   "123", "131A",  "131AH", "131AX" ,"131B" , "131BH", "131C" , "132" ,  "135A" , "135B",  "136" ,  "142", "143",   "146" ,  "149", "151A" , "151B",  "153" ,  "157" ,  "164",   "167",   "170A" , "170B" , "171" ,  "172A" , "172B",  "181",   "190", "192",   "197",   "199");

myOrd = intersect(myOrd, colnames(COMP));
 
# remove 
COMP = COMP[myOrd, myOrd];

skip = 1
if (skip == 1){
	pdf(paste('Agg_Pairwise_Prob_Matrix_',myDescGrade ,'.pdf',sep='')); 
	# heatmap( COMP , Rowv = NA, Colv = NA, scale = "column", main = "Aggregated Pairwise Prob Matrix");
	library("gplots") ;
	#install.packages("gplots");
	heatmap.2( COMP,  Rowv = NA, Colv = NA, key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=0.5, cexCol = 0.5,  main = paste( 'nr Stud=', nrRemIDs,'; grade= ',myDescGrade) );
	dev.off();
}

toDrop = c('110','153','31E','38A','104','35CH','38B','35AH','35BH','3A','143', '192','20B','3B','131AX', '32AL','3C',   '15',  '32BL', '1', '30', '60', '114A', '149',  '20',  '120B', '190',  '146',  '32AH', '157');

myOrd = setdiff(myOrd, toDrop)
nrUnqCourses = length(myOrd);

COMP = COMP[myOrd, myOrd];

# Do a naive ranking just by computing the rowSums (or colSums)
doPlain = 1
if( doPlain == 1 ){
plain_row_sums = colSums(COMP);
myDF = data.frame( rs = plain_row_sums, course = myOrd ); 
ord = order(myDF$rs, decreasing = FALSE);
myDF = myDF[ ord, ]

statToPlot = myDF$rs;  names(statToPlot) = rownames(myDF);   print(statToPlot)
pdf(paste('Plain_RowSums_',myDescGrade,'.pdf', sep='') );
barplot(statToPlot, las = 2, cex.names = 0.6, main = paste('Row Sums (Mtx of Probs). \n  nr Stud=', nrRemIDs,'; grade= ',myDescGrade) , horiz=TRUE);
dev.off();

print(myDF)
readline('see by large row...');

# return();
}



}



getLen = function(x){ return(length(x)); }



myRho = function(x){
	# print(x);
	nrNnz = sum(x!=0, na.rm=TRUE);  # print(nrNnz);
	# readline('xxxx');
	if(nrNnz==0){ rho = 0.5; }
	else{
		nrPlus = sum(x==1, na.rm=TRUE);
		nrMinus = sum(x==-1, na.rm=TRUE); 
		if( nrPlus > nrMinus){ rho = nrPlus / (nrPlus + nrMinus); }
		else{                  rho = -nrMinus / (nrPlus + nrMinus); }
		
		# rho = nrPlus / (nrPlus + nrMinus);
		rho = nrMinus / (nrPlus + nrMinus);
	}
	return(rho)
}


ones = function(nr_states_A, nr_states_B){
x = array( 1, dim = c(nr_states_A, nr_states_B) ); return(x);	
}

eye = function(nr_states) {
	
	return( diag( ones(1,nr_states) ) );
}
