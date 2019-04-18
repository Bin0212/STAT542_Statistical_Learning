rm(list = objects())
s = 3
source("mymain.R")

# Now the directory should have three new files named "mysubmission*.txt"
# Next compute AUC

rm(list = objects())
all = read.table("Project2_data.tsv", stringsAsFactors = F, header = T)
splits = read.table("Project2_splits.csv", header = T)
s = 3
test.y = all[which(all$new_id%in%splits[,s]), 1:2]
rm("all")
rm("splits")

err = rep(0, 3)
for(i in 1:3){
  pred = read.csv(paste0("mysubmission", i, ".txt"))
  pred = merge(pred, test.y, by="new_id")
  roc_obj = roc(pred$sentiment, pred$prob)
  err[i] = pROC::auc(roc_obj)
}

# The three errors below should match with the ones from your report. 
print(err)