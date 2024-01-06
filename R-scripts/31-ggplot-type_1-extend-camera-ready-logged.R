library(R.utils)

args = commandArgs(trailingOnly=TRUE, asValues=TRUE)

if (length(args) < 1) {
  stop("usage: 31-ggplot-type_1-extend-camera-ready.R <measure=value>", call.=FALSE)
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
  path_in = file.path("output/type_1/final_use", dataset)
  
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

sampsize = c(20000, 50) #filter by dataset and sample size
dname = 'ml-25m'

# Data preparation
df = data.type1 %>%
  select(alpha, t2, w2, s2, p2, b2, sample_size, measure, data_name) %>%
  filter((data_name == dname) & (sample_size %in% sampsize)) %>%
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
cat("measure-2", measure)

#df["type1_err_log10"]=log10(df[,"type1_err"])
#df["alpha_log10"]=log10(df[,"alpha"])
df = df[df[,"measure"] == measure,]
#df = df[df[,"alpha"]%in% seq(.01, .1, .01),]
df = df[df[,"alpha"]< .1,]
print(head(df))
print(tail(df))

#Remove leading and trailing zeros
no_zero <- function(x) {
  y = sub('0.', '.', sprintf('%s',x))
  y
}

#New facet label names for the sample size
sample.size = c(
  '25' = 'n = 25',
  '50' = 'n = 50',
  '100' = 'n = 100',
  '500' = 'n = 500',
  '1000' = 'n = 1000',
  '5000' = 'n = 5000',
  '10000' = 'n = 10000',
  '20000' = 'n = 20000',
  '50000' = 'n = 50000'
)

# Visualization
path_out = file.path("chart/type_1/calibration_extended_cameraReady_lowerAlpha")
dir.create(path_out, recursive = TRUE)

#pdf(file = file.path(path_out, paste0(dataset, "_tails2.pdf")))

ggplot_type1 = ggplot(df, aes(x = alpha, y = type1_err)) +
  facet_wrap(vars(sample_size), labeller = labeller(sample_size = sample.size)) +
  geom_line(aes(color = Statistical_tests, linetype = Statistical_tests), size=0.5) +
  geom_abline(intercept = 0) +
  #scale_x_log10() +
  #scale_y_log10() +
  #scale_y_continuous(trans = 'log2') +
  #scale__continuous(trans = 'log2')
  labs(
    x = expression(paste('Significance level ', alpha)),
    y = expression(paste('Fraction with p < ', alpha)),
    title = mea_fullname[measure]
  ) +
  theme_minimal(base_size=10) +
  theme(
    panel.border=element_rect(linetype="solid", color="grey", fill=NA),
    plot.margin=margin(),
    legend.title = element_blank(),
    legend.position = "bottom",
  )
cat("measure-1", measure, "\n")
print(paste0("type1_calib_Extended_", measure, "_cameraReady.pdf"))
ggsave(plot = ggplot_type1, #ggplot
       filename = paste0("type1_calib_Extended_", measure, "_cameraReady.pdf"),
       device = "pdf",
       path = file.path(path_out),
       width = 5, height = 3, units = "in"
)
#dev.off()
