sampleLearning <- function(x, datafile =) {
  
}

library("RWeka")
# read data from an .arff file(already reduced truth table)
data = read.arff("d:/workspace/r/precond_dt/evernote_reduce.arff")
# learn decision tree with the whole data, as baseline
m1 = J48(EDAM ~ ., data = data)
# sample 1/10, 1/4, 1/2, 3/4 of data to learn DT
row = rownames(data)
# 
ratio = 3/4 # 1/10, 1/4, 1/2, 3/4
data_16 = subset(data,(row %in% sample(1:length(row),length(row)*ratio,replace=FALSE)))
m16 = J48(EDAM ~ ., data = data_16)

# write result DT and data to files, need to revise
tmp = file("full.data","w")
write_to_dot(m1)
write_to_dot(m1, con = tmp)
close(tmp)
exit
