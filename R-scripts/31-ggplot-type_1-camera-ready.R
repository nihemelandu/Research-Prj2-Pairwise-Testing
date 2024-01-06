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
library(ggplot2)
library("tidyverse")
library(gtable)

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
  select(alpha, t2, w2, s2, p2, b2, sample_size, measure, data_name) %>%
  gather(key = "Statistical_tests", value = "type1_err", 
         -c(alpha, sample_size, measure, data_name))

df[,"Statistical_tests"][df[,"Statistical_tests"]=='t2'] = 't-test'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='w2'] = 'Wilcoxon'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='s2'] = 'Sign'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='p2'] = 'Randomization'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='b2'] = 'Bootstrap'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='blb2'] = 'BLB'

df[,"data_name"][df[,"data_name"]=='ml-100k'] = 'ML-100K'
df[,"data_name"][df[,"data_name"]=='ml-25m'] = 'ML-25M'
df[,"data_name"][df[,"data_name"]=='amazon-instantvideo'] = 'AZ-Video'
df[,"data_name"][df[,"data_name"]=='msmarco'] = 'MSMARCO'

df = df[df[,"measure"] == measure,]
df = df[df[,"alpha"]%in% c(0.01, 0.05, 0.1),]
head(df)

#Remove leading and trailing zeros
no_zero <- function(x) {
  y = sub('0.', '.', sprintf('%s',x))
  y
}

#New facet label names for the sample size
# sample.size = c(
#   '25' = '25 users',
#   '50' = '50 users',
#   '100' = '100 users',
#   '500' = '500 users',
#   '1000' = '1000 users',
#   '5000' = '5000 users',
#   '10000' = '10000 users',
#   '20000' = '20000 users',
#   '50000' = '50000 users'
# )

sample.size = c(
  '25' = '25',
  '50' = '50',
  '100' = '100',
  '500' = '500',
  '1000' = '1k',
  '5000' = '5k',
  '10000' = '10k',
  '20000' = '20k',
  '50000' = '50k'
)

# Visualization
path_out = file.path("chart/type_1/typ1_camera_ready")
dir.create(path_out, recursive = TRUE)

#pdf(file = file.path(path_out, paste0(dataset, "_tails2.pdf")))

ggplot_type1 = ggplot(df, aes(x = sample_size, y = type1_err)) +
  facet_grid(alpha ~ data_name) + 
  geom_line(aes(color = Statistical_tests, linetype = Statistical_tests), size=0.5) +
  labs(
    # x = expression(paste('Significance level ', alpha)),
    title = mea_fullname[measure]
  ) + 
  scale_x_continuous(name = "Sample size (# of Requests)") +
  scale_y_continuous(name = "Type 1 error rate") +
  theme_minimal(base_size=10) + 
  theme(
    panel.border=element_rect(linetype="solid", color="grey", fill=NA),
    plot.margin=margin(),
    axis.text.x = element_text(angle = -90),
    legend.title = element_blank(),
    legend.position = "bottom",
  )

ggsave(plot = ggplot_type1, #ggplot
       filename = paste0("type1_", measure, "_large.pdf"),
       device = "pdf", #pdf
       path = file.path(path_out),
       width = 9, height = 7, 
       units = "in"
)
#dev.off()
