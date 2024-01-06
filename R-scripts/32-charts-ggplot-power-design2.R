theme_publish <- function(base_size = 12, base_family = "",
                          base_line_size = 0.25, ...) {
  half_line <- base_size / 2
  small_rel <- 0.8
  small_size <- small_rel * base_size
  
  # TODO: replace size with linewidth in `element_rect()`
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(
      rect = element_rect(fill = "transparent", colour = NA, color = NA,
                          size = 0, linetype = 0),
      text = element_text(family = base_family, face = "plain",
                          colour = "black", size = base_size, hjust = 0.5,
                          vjust = 0.5, angle = 0, lineheight = 0.9,
                          margin = ggplot2::margin(), debug = F),
      
      axis.text = element_text(size = small_size),
      axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4),
                                 vjust = 1),
      axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4), 
                                 hjust = 1),
      axis.title.x = element_text(margin = ggplot2::margin(t = small_size,
                                                           b = small_size)),
      axis.title.y = element_text(angle = 90,
                                  margin = ggplot2::margin(r = small_size,
                                                           l = small_size/4)),
      axis.ticks = element_line(colour = "black", size = base_line_size),
      axis.ticks.length = unit(0.25, 'lines'),
      
      axis.line = element_line(colour = "black", size = base_line_size),
      axis.line.x = element_line(colour = "black", size = base_line_size), 
      axis.line.y = element_line(colour = "black", size = base_line_size), 
      
      legend.spacing = unit(base_size/4, "pt"),
      legend.key = element_blank(),
      legend.key.size = unit(1 * base_size, "pt"),
      legend.key.width = unit(1.5 * base_size, 'pt'),
      legend.text = element_text(size = rel(small_rel)),
      legend.title = element_text(size = rel(small_rel), face = 'bold'),
      legend.position = 'bottom',
      legend.box = 'horizontal',
      
      panel.spacing = unit(1, "lines"),
      panel.background = element_blank(),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      strip.text = element_text(size = base_size),
      strip.background = element_rect(fill = NA, colour = "black",
                                      size = 0.125),
      strip.text.x = element_text(face = 'bold', hjust = 0,
                                  margin = ggplot2::margin(b = small_size/2,
                                                           t = small_size/4)),
      strip.text.y = element_text(angle = -90, face = 'bold',
                                  margin = ggplot2::margin(l = small_size/2,
                                                           r = small_size/4)),
      
      plot.margin = unit(c(5,5,0,0), "pt"),
      plot.background = element_blank(),
      plot.title = element_text(face = "bold", size = 1.2 * base_size, 
                                margin = ggplot2::margin(b = half_line),
                                hjust = 0)
    )
}
#function end

library(R.utils)
args = commandArgs(trailingOnly=TRUE, asValues=TRUE)

if (length(args) < 1) {
  stop("usage: Rscript 32-charts-ggplot-power-design2.R <measure>", call.=FALSE)
}
measure = args[['measure']]

mea_fullname = c('ndcg100' = 'Normalized Discounted Cumulative Gain(nDCG)',
                 'recip_rank100' = 'Reciprocal Rank')


library(strex)
library(rio)
library(ggplot2)
library(tidyverse)

source("R-scripts/Utility.R")

alpha = 0.05
data.power = data.frame()
for (dataset in .DATASETS){

  path_in = file.path("output/type_2", dataset, "by_delta", paste0("alpha", salpha(alpha)))
  
  dat_power = import_list(list.files(path_in, pattern = "type_2_by_delta_", full.names = TRUE),
                          rbind = TRUE)
  
  #add dataset name to dataset
  dat_power["data_name"]= dataset
  
  # grab and add sample size to dataset
  dat_power["sample_size"] = as.numeric(str_after_last(str_before_last(dat_power[,"_file"], "_"),
                                                       "_"))
  
  # grab and add measure to dataset
  dat_power["measure"] = str_before_last(str_before_last(str_after_last(dat_power[,"_file"],
                                                                        "/type_2_by_delta_"), 
                  paste0("_alpha", salpha(alpha),".csv")), "_")
  # Combine datasets
  data.power = rbind(data.power, dat_power)
  print(head(dat_power))
}

#Order the dataset by sample size
data.power = data.power[order(data.power$sample_size),]

# Data preparation
df = data.power %>%
  select(delta, t2, w2, s2, p2, b2, sample_size, measure, data_name)  %>%
  gather(key = "Statistical_tests", value = "power", -c(delta, sample_size, measure, data_name))

df = df[df[,"sample_size"]%in% c(50, 1000, 5000, 10000),]
df[,"Statistical_tests"][df[,"Statistical_tests"]=='t2'] = 't-test'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='w2'] = 'Wilcoxon'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='s2'] = 'Sign'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='p2'] = 'Randomization'
df[,"Statistical_tests"][df[,"Statistical_tests"]=='b2'] = 'Bootstrap'

df[,"data_name"][df[,"data_name"]=='ml-100k'] = 'ML-100K'
df[,"data_name"][df[,"data_name"]=='ml-25m'] = 'ML-25M'
df[,"data_name"][df[,"data_name"]=='amazon-instantvideo'] = 'AZ-Video'
df[,"data_name"][df[,"data_name"]=='msmarco'] = 'MSMARCO'

df = df[df[,"measure"] == measure,]
head(df)

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
#path_out = file.path("chart/type_2/published_charts") #For paper
path_out = file.path("chart/type_2/poster_charts") #For poster
dir.create(path_out, recursive = TRUE)

#pdf(file = file.path(path_out, paste0(dataset, "_alpha", salpha(alpha), ".pdf")))
ggplot_power = ggplot(df, aes(x = delta, y = power)) +
  facet_grid(sample_size ~ data_name, labeller = labeller(sample_size = sample.size)) + 
  geom_line(aes(color = Statistical_tests, linetype = Statistical_tests)) +
  labs(
    x = expression(paste('effect size ', delta)),
    y = "Power (True positive rate)",
    title = mea_fullname[measure]) +
    #title = expression(paste('Dataset: ', dataset, "  ",alpha, "=0.05"))) + 
  scale_x_continuous(labels = no_zero) +
  theme_publish() 
  # theme_minimal(base_size=10) + 
  # theme(
  #   panel.border=element_rect(linetype="solid", color="grey", fill=NA),
  #   plot.margin=margin(),  
  #   #plot.title = element_text(face = "bold", size = 12),
  #   #axis.ticks = element_line(colour = "grey70", size = 0.2),
  #   #panel.grid.major = element_line(colour = "grey70", size = 0.2),
  #   #axis.text.x = element_text(size = 5),
  #   #axis.text.y = element_text(size = 4),
  #   #strip.text.x = element_text(size = 6),
  #   strip.text.y = element_text(size = 6),
  #   legend.title = element_blank(),
  #   #legend.text  = element_text(size = 8),
  #   #legend.background = element_rect(fill = "white", size = 0.8, colour = "white"),
  #   legend.position = "bottom"
  # )
  
#dev.off()
ggsave(plot = ggplot_power, #ggplot
       #filename = paste0("power_",measure,"_published.png"),
       filename = paste0("power_",measure,"_poster.png"),
       device = "png",
       width = 7, height = , units = "in",
       path = file.path(path_out),
       #dpi = 660,
       dpi = 2000)
       #width = 5, #long width
       #height = 3) #short height






