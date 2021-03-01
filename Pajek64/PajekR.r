cat("########################################",sep="\n")
cat(" R called from Pajek                    ",sep="\n")
cat(" http://mrvar.fdv.uni-lj.si/pajek/      ",sep="\n")
cat(" Andrej Mrvar & Vladimir Batagelj       ",sep="\n")
cat(" University of Ljubljana, Slovenia      ",sep="\n")
cat("-----------------------------------------------------------------------",sep="\n")
cat(" The following vectors read:",sep="\n")
v3<-c(10,1,3,3,1,1,1,1,1,1,1,36,2,1,1,1,9,7,7,7,7,7,7,15,11,16,10,17,4,8,2,4,1,2,6,6,6,6,6,3,1,11,3,3,2,1,1,2,22,6,2,5,2,18,2,11,15,11,9,11,13,12,13,12,10,1,10,10,10,9,3,2,2,7,7)
comment(v3)<-"All Degree of N2 (75)"
cat(" v3  :  All Degree of N2 (75)",sep="\n")
v4<-c(3,7,11,3,8,8,3,5,9,6,9,5,9,4,7,6,6,7,7,8,5,2,4,7,6,4,6,3,5,7,8,12,5,11,9,5,6,8,8,4,7,12,3,6,6,9,4,6,5,6,7,7,8,4,10,7,10,7,8,4,5,5,5,9,9,8,8,10,6,4,10,8,7,5,7)
comment(v4)<-"All Degree of N3 (75)"
cat(" v4  :  All Degree of N3 (75)",sep="\n")
cat("",sep="\n")
cat(" Use objects() to get list of available objects           ",sep="\n")
cat(" Use comment(?) to get information about selected object  ",sep="\n")
savevector<-function(v,direct){write(c(paste("*Vertices",length(v)), v), file = direct, ncolumns=1)}
cat(" Use savevector(v?,'???.vec') to save vector to Pajek input file  ",sep="\n")
comment(savevector)<-"Save vector to file that can be read by Pajek"
savematrix <- function(n,direct,twomode=1){
if ((dim(n)[1] == dim(n)[2]) & (twomode!=2))
{ write(paste("*Vertices",dim(n)[1]), file = direct);
  write(paste(seq(1,length=dim(n)[1]),' "',rownames(n),'"',sep=""), file = direct,append=TRUE);
  write("*Matrix", file = direct,append=TRUE);
  write(t(n),file = direct,ncolumns=dim(n)[1],append=TRUE) }
else
{ write(paste("*Vertices",sum(dim(n)),dim(n)[1]), file = direct);
  write(paste(1:dim(n)[1],' "',rownames(n),'"',sep=""), file = direct,append=TRUE);
  write(paste(seq(dim(n)[1]+1,length=dim(n)[2]),' "',colnames(n),'"',sep=""), file = direct,append=TRUE);
  write("*Matrix", file = direct, append=TRUE);
  write(t(n),file = direct, ncolumns=dim(n)[2],append=TRUE)} }
cat(" Use savematrix(n?,'???.net') to save matrix to Pajek input file (.MAT) ",sep="\n")
cat("     savematrix(n?,'???.net',2) to request a 2-mode matrix (.MAT) ",sep="\n")
comment(savematrix)<-"Save matrix to file that can be read by Pajek (as *Matrix)"
savenetwork <- function(n,direct,twomode=1){
if ((dim(n)[1] == dim(n)[2]) & (twomode!=2))
{ write(paste("*Vertices",dim(n)[1]), file = direct);
  write(paste(seq(1,length=dim(n)[1]),' "',rownames(n),'"',sep=""), file = direct,append=TRUE);
  write("*Arcs", file = direct,append=TRUE);
   for (i in 1:dim(n)[1]) {
     for (j in 1:dim(n)[2]) {
       if (n[i,j]!=0) {write(paste(i,j,n[i,j]),file = direct,append=TRUE)}
     }
   } }
else
{ write(paste("*Vertices",sum(dim(n)),dim(n)[1]), file = direct);
  write(paste(1:dim(n)[1],' "',rownames(n),'"',sep=""), file = direct,append=TRUE);
  write(paste(seq(dim(n)[1]+1,length=dim(n)[2]),' "',colnames(n),'"',sep=""), file = direct,append=TRUE);
  write("*Edges", file = direct,append=TRUE);
   for (i in 1:dim(n)[1]) {
     for (j in 1:dim(n)[2]) {
       if (n[i,j]!=0) {write(paste(i,j+dim(n)[1],n[i,j]),file = direct,append=TRUE)}
     }
   } } }
cat(" Use savenetwork(n?,'???.net') to save matrix to Pajek input file (.NET) ",sep="\n")
cat("     savenetwork(n?,'???.net',2) to request a 2-mode network (.NET) ",sep="\n")
comment(savenetwork)<-"Save matrix to file that can be read by Pajek (as *Arcs)"
loadvector <- function(direct){
  vv<-read.table(file=direct,skip=1)
  if (dim(vv)[2]==1)
    vv<-vv[[1]]
  vv
}
cat(" Use v?<-loadvector('???.vec') to load vector(s) from Pajek input file  ",sep="\n")
comment(loadvector)<-"Load vector(s) from file that was produced by Pajek"
loadmatrix <- function(direct){
nn<-read.table(file=direct,nrows=1)
if (length(nn) == 2)
  { xx<-read.table(file=direct,skip=1,nrows=nn[[2]],fill=TRUE)
    n<-read.table(file=direct,skip=nn[[2]]+2)
    rownames(n)<-xx[[2]]
    colnames(n)<-xx[[2]] }
 else
   {xxrow<-read.table(file=direct,skip=1,nrows=nn[[3]],fill=TRUE)
    xxcol<-read.table(file=direct,skip=nn[[3]]+1,nrows=nn[[2]]-nn[[3]],fill=TRUE)
    n<-read.table(file=direct,skip=nn[[2]]+2)
    rownames(n)<-xxrow[[2]]
    colnames(n)<-xxcol[[2]] }
  as.matrix(n)
  }
cat(" Use n?<-loadmatrix('???.mat') to load matrix from Pajek input file  ",sep="\n")
comment(loadmatrix)<-"Load matrix from file that was produced by Pajek"
cat("-----------------------------------------------------------------------",sep="\n")
cat("",sep="\n")
