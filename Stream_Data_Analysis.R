getwd()
#import libraries
library(bestNormalize)
library(tidyverse)
library(psych)
library(recipes)
library(rcompanion)
library(rstatix)
library(RVAideMemoire)


################### Data Visualization and Standardization #####################


#set path to data
path <- "~/Classes/Fall 2020/STAT613/Exam 1/P4/Stream_data.csv"

#read data into a tibble
Stream_data <- read_csv(path)

#Assign relevant vars to a single tibble
Stream_raw <- select(Stream_data, mayfly, caddisfly, stonefly, flies, beetles)

#Get mean with standard deviation of the relevant vars
psych::describe(Stream_raw)

#Transform and assign vars to Stream_subset_tr
Stream_log <- mutate(Stream_raw,
                           mayfly = log10(mayfly + 1),
                           caddisfly = log10(caddisfly + 1),
                           stonefly = log10(stonefly + 1),
                           flies = log10(flies + 1),
                           beetles = log10(beetles + 1))

Stream_Tuk <- mutate(Stream_raw,
                     mayfly = transformTukey(mayfly, plotit=FALSE),
                     caddisfly = transformTukey(caddisfly, plotit=FALSE),
                     stonefly = transformTukey(stonefly, plotit=FALSE),
                     flies = transformTukey(flies, plotit=FALSE),
                     beetles = transformTukey(beetles, plotit=FALSE))


#get new mean and standard deviations of transformed vars
psych::describe(Stream_raw)
psych::describe(Stream_log)
psych::describe(Stream_Tuk)

#Create histograms for raw data
raw_may <- hist(Stream_raw$mayfly)
raw_cds <- hist(Stream_raw$caddisfly)
raw_stn <- hist(Stream_raw$stonefly)
raw_fly <- hist(Stream_raw$flies)
raw_btl <- hist(Stream_raw$beetles)

#Create histograms for log-transformed data
log_may <- hist(Stream_log$mayfly)
log_cds <- hist(Stream_log$caddisfly)
log_stn <- hist(Stream_log$stonefly)
log_fly <- hist(Stream_log$flies)
log_btl <- hist(Stream_log$beetles)

#Create histograms for Tukey-transformed data
tuk_may <- hist(Stream_Tuk$mayfly)
tuk_cds <- hist(Stream_Tuk$caddisfly)
tuk_stn <- hist(Stream_Tuk$stonefly)
tuk_fly <- hist(Stream_Tuk$flies)
tuk_btl <- hist(Stream_Tuk$beetles)

## Analysis of the Impacts of Stream Quality and Diversity on Insect Species ###

# We are interested in whether or not Stream biodiversity or quality have an
# impact on the number of mayflies, caddisflies, and stoneflies. This makes Stream
# biodiversity and quality our independent vars, and the number of may/caddis/stone
# flies the dependent vars.


#bind ind and dep vars into matrices to make manova easier. Note that we are
#concatenating stream_qlty and stream_diversity into a single character column
#that is our independent variable.

ind_vars <- cbind(paste0(Stream_data$stream_qlty, Stream_data$stream_diversity))
dep_vars <- cbind(Stream_Tuk$mayfly, Stream_Tuk$caddisfly, Stream_Tuk$stonefly)

#Run MANOVA

fly_manova <- summary(manova(dep_vars ~ Stream_data$stream_qlty*Stream_data$stream_diversity))
print(fly_manova)


#Run pairwise one-way ANOVA's

fly_anova <- summary(aov(dep_vars ~ Stream_data$stream_qlty*Stream_data$stream_diversity))
print(fly_anova)

pairwise.perm.manova(dep_vars, ind_vars, 
                     test=c("Pillai","Wilks","Hotelling-Lawley","Roy","Spherical"), 
                     nperm=999, progress=TRUE, p.method="fdr")












       