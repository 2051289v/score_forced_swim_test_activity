library(tidyverse)

# This script creates a scatterplot with lines of best fit of activity measured
# with the different scoring methods vs human scoring. 
# Requires the summary file generated from the raw data and file with ID and 
# manual scores.

# Where are the files?
summaryFile<-'pathTo_summary.csv'
humanFile<-'pathTo_manual_scoring.csv'

# read files
sf= read.csv(summaryFile)
hf=read.csv(humanFile) # ID, type, total_sec_7

# select only flyvende. with the swimming ones there will an issue, can't read Danish
hf <- filter(hf,type=='ANTAL, FLYDENDE')

# merge the two tables
df <- merge(sf,hf,by.x='ID',by.y='id')

# reshape the table for plotting into long format
lf<-pivot_longer(df,cols=c(2:5),names_to='ScoringMethod',values_to='Activity')


# da plot:
ggplot(data=lf,aes(x = sec_total_7 , y = Activity, color=ScoringMethod)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  xlab('human score') +
  ylab('R script') 


# plot subset of rats that could not be scored manually:
IDsToPlot<-c(11, 12,  19, 20, 21, 22, 39, 40, 41, 42 )            
filt_lf <- lf[lf$ID %in% IDsToPlot,] #what does the comma do here?!

ggplot(data=filt_lf,aes(x = sec_total_7 , y = Activity, color=ScoringMethod)) + 
  geom_point() +
  xlab('human score') +
  ylab('R script')            
