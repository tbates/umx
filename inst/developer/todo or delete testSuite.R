require(OpenMx); require(foreign); require(MASS); library(doBy) # for summaryBy() #require(ggplot2); library(reshape);
source("http://openmxhelpers.googlecode.com/svn/trunk/genEpi.lib.R")

twinData = read.spss("~/Dropbox/current/Despina_eudaimonia/twins_restructured_for_analysis.sav", use.value.labels=T, to.data.frame=TRUE, max.value.labels=Inf, trim.factor.names=FALSE)
names(twinData) = gsub(".", "", names(twinData),fixed=T) # replace "." globally
names(twinData)
selDVs = c("accept1", "positrel1", "autono1",
           "accept2", "positrel2", "autono2")

nSib    = 2;
nDVs    = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL(so times-2 for a family)**
nlower	= 2*nDVs*(2*nDVs+1)/2 #	number of free elements	in a lower matrix tnDVs*tnDVs
mzmData = subset(twinData, sex1=="MALE"   & zyg1=="MONOZYGOTIC"         , selDVs)  # MZ males
mzfData = subset(twinData, sex1=="FEMALE" & zyg1=="MONOZYGOTIC"         , selDVs)  # MZ females
dzmData = subset(twinData, sex1=="MALE"   & zyg1=="DIZYGOTIC - SAME SEX", selDVs)  # DZ males
dzfData = subset(twinData, sex1=="FEMALE" & zyg1=="DIZYGOTIC - SAME SEX", selDVs)  # DZ females
dzoData = subset(twinData, zyg1=="DIZYGOTIC - DIFFERENT SEX"            , selDVs)  # DZ OS's

model = twoGroupCholesky("ACE", selDVs=selDVs, dzData=dzData, mzData=mzData, nSib=nSib);

genEpi_equate(model, master="a" , slave="c" , free=T)
genEpi_equate(model, master="a1", slave="c1", free=T, verbose=F)
genEpi_equate(model, master="am", slave="af", free=T, verbose=F)

genEpi_Labeler(mxMatrix("Diag", nrow=3, ncol=3, free=T, values=0, byrow=TRUE, name="a"), setfree=T, drop=0,jiggle=.1,boundDiag=.0001)
genEpi_Labeler(mxMatrix("Full", nrow=3, ncol=3, free=T, values=0, byrow=TRUE, name="a"), setfree=T, drop=0,jiggle=.1,boundDiag=.0001)
genEpi_Labeler(mxMatrix("Iden", nrow=3, ncol=3, byrow=TRUE, name="a"), setfree=F, drop=0,jiggle=.1,boundDiag=.0001)
genEpi_Labeler(mxMatrix("Stand", nrow=nDVs, ncol=nDVs, free=T, values=.5, lbound=-.9999, ubound=.9999, name="Rem" ))

mxMatrix("Diag Full Iden Lower Sdiag Stand Symm Unit Zero", nrow=nVar, ncol=nVar, free=TRUE, values=0, label=labels,  dimnames=list(rowList, columnList), byrow=TRUE, name="expMean")	


genEpi_getLabels(model, regex="a", free=T, verbose=T)

