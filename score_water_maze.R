#install.packages('gdata')
#library(gdata)
library(readxl)
library(tidyverse)

# where are the raw data?
folder<-'pathTo_raw data'
# get all xls-files
file_list<-list.files(path=folder,pattern="\\.xlsx$")

# file with bodyweight info
info_file <- 'pathTo_bodyweight.csv'
info <-read.csv(info_file) # maybe good idea to have size as well.


# some variables:
arenasize<- 28.62*60.31 #cm^2 
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
  ArenaSecs = numeric(),
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
    bodyweight<-as.numeric(select(filter(info,ID==id),'BW'))
    tab<-read_xlsx(fullfn,sheet=sn,range=cell_limits(c(43, 1), c(NA, 16)),
                   col_names=Cols)
    # Reading xlsx file with missing data will always cause warning New Name...
    # safe to ignore?
    
    print(paste(tab %>% nrow(),'rows,','duration =', tab %>% select('Recording time') %>% 
            slice(tail(row_number(), 1)),'s'))
    # this seems a ridiculous round about way but it works. 
   
    
    # select columns: t [s], area = body size [cm^2], activity relative to arena size [%]
    df <- tab %>% select('Recording time','Area','Activity')
    #sapply(df, class)
    
    # make sure data is numeric. If missing data >> NaN
    df <- df %>% mutate_if(is.character,as.numeric)
    
    # And then we have 4 different inactivity measures
    # 1) as % of arena (we should add that one to the summary file
    # 2) as % of body weight
    # 3) as % to body size (frame-to-frame)
    # 4) as % to average body size (average over all frames)
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
   
    
    # count activity/BS % if activity/BS %<30. This number I have then 
    # multiplied by 25/60 to convert from frames into seconds
    # count activity/BW if activity/BS <25. This number I have then multiplied 
    # by 25/60 to convert from frames into seconds
    # count activity/AVR BS% if activity/AVR BS %<30. This number I have 
    # then multiplied by 25/60 to convert from frames into seconds
   countActivityArena<-df %>% filter(Activity <2.5) %>% nrow()/nframes
   countActivityBSsecs<-df %>% filter(activityBS <30) %>% nrow()/nframes
   countActivityBWsecs<-df %>% filter(activityBW <25) %>% nrow()/nframes 
   countActivityAVRBSsecs<-df %>% filter(activityAVRBS <30) %>% nrow()/nframes
   
  # Now add data to summary table:
   summary_table<- summary_table %>% add_row(ID=id,
                         ArenaSecs= countActivityArena,                   
                         BSsecs=countActivityBSsecs,
                         BWsecs= countActivityBWsecs,
                         AVRBSsecs=countActivityAVRBSsecs
                         )
 
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






# Raw_data_2020_6_FST_SR25_B3_T4_dAY_3_Trial_2 <- read_excel("Desktop/For 
# Alex/raw data/Raw data-2020-6 FST SR25 B3 T4 dAY 3-Trial 2.xlsx", 
#                                                            sheet = "Track-Arena 1-Subject 1")
# View(Raw_data_2020_6_FST_SR25_B3_T4_dAY_3_Trial_2)
# Sheet 2
# library(readxl)
# Raw_data_2020_6_FST_SR25_B3_T4_dAY_3_Trial_2 <- read_excel("Desktop/For 
# Alex/raw data/Raw data-2020-6 FST SR25 B3 T4 dAY 3-Trial 2.xlsx", 
#                                                            sheet = "Track-Arena 2-Subject 1")
# View(Raw_data_2020_6_FST_SR25_B3_T4_dAY_3_Trial_2)
# sheet = "Track-Arena 3-Subject 1"
# sheet = "Track-Arena 4-Subject 1"
