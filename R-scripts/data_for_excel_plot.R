library(R.utils)

args = commandArgs(trailingOnly=TRUE, asValues=TRUE)

if (length(args) < 1) {
  stop("usage: Rscript 31-charts-ggplot-type_1-design2.R <measure>", call.=FALSE)
}
measure = args[['measure']]

mea_fullname = c('ndcg100' = 'Normalized Discounted Cumulative Gain(nDCG)',
                 'recip_rank100' = 'Reciprocal Rank')

source("R-scripts/Utility.R")

library(tools)
library(strex)
library(rio)
library("tidyverse")

data.type1 = data.frame()
for (dataset in .DATASETS){
  path_in = file.path("output/type_1", dataset)

  #get the type-1 error rate data
  dat_type1 = import_list(list.files(path_in, pattern ="type_1_",
                                     full.names = TRUE), rbind = TRUE)

  #add dataset name to dataset
  dat_type1["data_name"]= dataset

  #grab the measure and add it to the dataset
  dat_type1["measure"]=str_before_last(str_after_last(dat_type1[,"_file"], "type_1_"), "_")

  #grab the sample size and add it to the dataset
  dat_type1["sample_size"]=as.numeric(str_before_last(str_after_last(dat_type1[,"_file"], "_"),
                                                      ".csv"))
  #concatenate datasets
  data.type1 = rbind(data.type1, dat_type1)
}

print(head(data.type1))

#Order the dataset by sample size
data.type1 = data.type1[order(data.type1$sample_size),]

# Data preparation
df = data.type1 %>%
  select(alpha, t2, w2, s2, p2, b2, sample_size, measure, data_name)

write.csv(df, file = "typ1_error_excelplot.csv", row.names = FALSE)
