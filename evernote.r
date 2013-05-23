sampleLearning <- function(rate=1, datafile="d:/workspace/r/precond_dt/evernote_reduce.arff") {
  library("RWeka")
  # read data from an .arff file(already reduced truth table)
#   if(data == NULL){
    data = read.arff(datafile)
#   }  
  # learn decision tree with the whole data, as baseline
  m1 = J48(EDAM ~ ., data = data)
  # get rownames of data, in this case the row number of the rows
  row = rownames(data)
  # sample 1/10, 1/4, 1/2, 3/4 of data to learn DT
  ratio = rate # 1/10, 1/4, 1/2, 3/4
  sampledata = subset(data,(row %in% sample(1:length(row),length(row)*ratio,replace=FALSE)))

  m_samp = J48(EDAM ~ ., data = sampledata)
  outputdir = "results"
  if(!file.exists(outputdir)){
    dir.create(file.path(getwd(),outputdir))
  }
  # write result DT and data to files
  resultfile = paste(getwd(),outputdir,paste(paste(format(Sys.time(),"%y%m%d_%H%M%OS3"),length(row)*ratio,sep="_"),"dat", sep="."), sep="/")

  tmp = file(resultfile,"w")
  write.table(sampledata,tmp)
  #write_to_dot(m1)
  write_to_dot(m_samp, con = tmp)
  close(tmp)
}

exp <- function(rate,times,datafile){
  for (i in 1:times) sampleLearning(rate,datafile)
}
