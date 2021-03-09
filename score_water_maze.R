library(readxl)
library(tidyverse)

# where are the raw data?
folder<-'pathToRawData'
# get all xls-files
file_list<-list.files(path=folder,pattern="\\.xlsx$")

# some variables:
arenasize<- 28.62*60.31 #cm 
bodyweight<-NaN # optional
nframes<-25

# Column names in excel file. Can probably be done in the reading function.
Cols<-c("Trial time","Recording time","X center","Y center","Area","Areachange",
  "Elongation","2.5 < Activity [0.1s; AVR 1sample;SR 5]< 3.5(Highly active)",
  "2.5 < Activity [0.1s; AVR 1sample;SR 5]< 3.5(Moderately active)",
  "2.5 < Activity [0.1s; AVR 1sample;SR 5]< 3.5(Inactive)",
  "9_ Mobility  AVR 1 sample  SR 5   _18(Highly mobile)",
  "9_ Mobility  AVR 1 sample  SR 5   _18(Mobile)",	
  "9_ Mobility  AVR 1 sample  SR 5   _18(Immobile)",
  "Mobility","Activity","FRL")



# initiate empty table that is filled in for-loop:
summary_table <- tibble(
  ID = numeric(),
  BSsecs = numeric(),
  BWsecs = numeric(),
  AVRBSsecs = numeric()
)

#### start for loop through all the files and sheets, do calculations ##########
for (fn in file_list){                  # for all files
  fullfn<-paste(folder,fn,sep="/")      # for each sheet - always 4??
  nSheets <- length(excel_sheets(path = fullfn))
  for (sn in c(1:nSheets)){
    print(paste('Now',fn,'sheet',sn)) # Can be disabled, just to keep track
    id<-as.numeric(read_xlsx(fullfn,sheet=sn,range="B35:B35",col_names=FALSE)) 
      # hard-coded-is ID ALWAYS in this field?
    tab<-read_xlsx(fullfn,sheet=sn,range=cell_limits(c(43, 1), c(NA, 16)),
                   col_names=Cols)
    # Reading xlsx file with missing data will always cause warning New Name...
    # safe to ignore?
    
    
    # select columns
    df <- tab %>% select('Recording time','Area','Activity')
    #sapply(df, class)
    
    # make sure data is numeric. If missing data >> NaN
    df <- df %>% mutate_if(is.character,as.numeric)
    
    #calculate following:
    #activity cm2 = % activity/100*arenasize
    #activity/BS % = activity cm2/area cm2 *100%
    #  activity/AVR BS% = activity cm2/AVERAGE(area cm2 of all 10500 rows for 
    #                                          each individual animal)*100%
    #activity/BW = activity cm2/bodyweight grams *100
    # HOPE I GOT YOUR CALCS RIGHT, CHECK!
    
    df<-df %>% mutate(activity_cm2 = Activity/100*arenasize,
        activityBS = activity_cm2/Area*100,
        activityAVRBS=activity_cm2/mean(df$Area,na.rm=TRUE)*100, # exclude missing data from AVG
        activityBW=activity_cm2/bodyweight*100)
        #check=activity_cm2-activityAVRBS) diff are small but present..
  
   # My advice: choose different variable names, spaces and special characters 
   # may mess things up at some point. I just made them close to what you suggested.
   
    # How to calculate:
    # count activity/BS % if activity/BS %<30. This number I have then 
    # multiplied by 25/60 to convert from frames into seconds
    # count activity/BW if activity/BS <25. This number I have then multiplied 
    # by 25/60 to convert from frames into seconds
    # count activity/AVR BS% if activity/AVR BS %<30. This number I have 
    # then multiplied by 25/60 to convert from frames into seconds
   countActivityBSsecs<-df %>% filter(activityBS >30) %>% nrow()*nframes/60
   countActivityBWsecs<-df %>% filter(activityBW >25) %>% nrow()*nframes/60 
   countActivityAVRBSsecs<-df %>% filter(activityAVRBS >25) %>% nrow()*nframes/60 
   
  # Now add data to summary table:
   summary_table<- summary_table %>% add_row(ID=id,BSsecs=countActivityBSsecs,
                         BWsecs= countActivityBWsecs,AVRBSsecs=countActivityAVRBSsecs)
 
  }
}
### end of loop

# sort by ID because less messy
summary_table <- arrange(summary_table, ID)

# write to CSV
write_csv(summary_table,paste(folder,'summary.csv',sep="/"),col_names=TRUE)


#plot: makes sense?
#ggplot(summary_table) + # the plus has to be at the end of this line
#geom_point(mapping=aes(x=BSsecs,y=AVRBSsecs)) 


