# In BASH: Rscript /home/alex/Downloads/Amanda/FST3/calculate_amandas_things_FST3.R > tmpR
# prints output in file tmpR (home dir)

library(readxl)
library(tidyverse)

user <- "Alex"
print(paste('Hi',user,'!'))

# Where are the raw data?
# This is where you insert your data path:
if (user == "Alex") {
  folder<-'/home/alex/Downloads/Amanda/FST'
} else {
  folder<-'/Users/au265176/Dropbox/For Alex/raw data 2020-7 (4)/FST3'
}

# get all xls-files
file_list<-list.files(path=folder,pattern="^[^~]*.xlsx$")
print(file_list)


# file with bodyweight info
info_file <- paste(folder,'BW.csv',sep='/')
info <-read.csv(info_file) # maybe good idea to have size as well.
arenasize<-info$ARENA [1]  #cm^2, Now in BW file
#30.28*63.1

# some variables:
nframes<-25 #frames per sec
npts=10500 #data points, exactly 7min

# Column names in excel file. Can probably be done in the reading function.
# Cols<-c("Trial time","Recording time","X center","Y center","Area","Areachange",
#   "Elongation","2.5 < Activity [0.1s; AVR 1sample;SR 5]< 3.5(Highly active)",
#   "2.5 < Activity [0.1s; AVR 1sample;SR 5]< 3.5(Moderately active)",
#   "2.5 < Activity [0.1s; AVR 1sample;SR 5]< 3.5(Inactive)",
#   "9_ Mobility  AVR 1 sample  SR 5   _18(Highly mobile)",
#   "9_ Mobility  AVR 1 sample  SR 5   _18(Mobile)",	
#   "9_ Mobility  AVR 1 sample  SR 5   _18(Immobile)",
#   "Mobility","Activity","FRL")
# DEPRECATED: Columns aren't consistent between experiments!

ThreshArenaSecs <- 2.5
ThreshBSsecs <- 30
ThreshBWsecs <- 25
ThreshAVRBSsecs <- 30

# initiate empty table that is filled in for-loop:
summary_table <- tibble(
  ID = numeric(),
  BW = numeric(),
  STRAIN = character(),
  DRUG = numeric(),
  GROUP = numeric(),
  AGE = numeric(),
  TrialName = character(),
  ArenaName = character(),
  ArenaSecs = numeric(),
  BSsecs = numeric(),
  BWsecs = numeric(),
  AVRBSsecs = numeric(),
  ThreshArenaSecs = numeric(),
  ThreshBSsecs = numeric(),
  ThreshBWsecs = numeric(),
  ThreshAVRBSsecs = numeric(),
  MissingArea = numeric(),
  MissingActivity = numeric(),
  Exclude = numeric(),
  Note = character(),
  VideoFile = character()
)

#### start for loop through all the files and sheets, do calculations ##########
for (fn in file_list){                  # for all files
  fullfn<-paste(folder,fn,sep="/")      # for each sheet - always 4??
  nSheets <- length(excel_sheets(path = fullfn))
  for (sn in c(1:nSheets)){
    print(paste('Now',fn,'sheet',sn)) # Can be disabled, just to keep track
    

    # get start position
    nHeaderlines <- as.numeric(names(read_xlsx(fullfn,sheet=sn) [2])) 
    # R guesses number of headerlines
    
    # ID is not always in the same field:
    # id<-as.numeric(read_xlsx(fullfn,sheet=sn,range="B38:B38",col_names=FALSE)) 
    tab <- read_xlsx(fullfn,sheet=sn,col_names = FALSE)
    id <- as.numeric(tab[ which(tab$...1 == "id"), 2])
    trialName = as.character(tab[ which(tab$...1 == "Trial name"), 2])
    arenaName = as.character(tab[ which(tab$...1 == "Arena name"), 2])
    videoFile = as.character(tab[ which(tab$...1 == "Video file"), 2])
    
    # exclude <- as.numeric(tab[ which(tab$...1 == "Exclude"), 2]) # old: from xls file
    # if (is.na(exclude)) { exclude = 0 } # account for missing exclude field in some xls sheet
    exclude<-as.numeric(select(filter(info,ID==id),'EXCLUDE'))     # new: from BW file
    
   
    
    if (TRUE) { #(exclude == 1) {
    #   print(paste("ID",id,": Excluded, skipping file", fn,"sheet",sn))
    # } else if (dim(tab)[1] < 50) {
    #   # some arbitrary low number
    #   print(paste("ID",id,": No data, Skipping file", fn,"sheet",sn))
    # } else {
      # Process the file
      Cols <- as.character(tab[nHeaderlines-1,])
      # Cols <- read_xlsx(fullfn,sheet=sn,range= cell_rows(41))
      
      bodyweight<-as.numeric(select(filter(info,ID==id),'BW'))
      age<-as.numeric(select(filter(info,ID==id),'AGE')) 
      strain<-as.character(select(filter(info,ID==id),'STRAIN')) 
      drug<-as.numeric(select(filter(info,ID==id),'DRUG')) 
      group<-as.numeric(select(filter(info,ID==id),'GROUP')) 
      note<-as.character(select(filter(info,ID==id),'NOTE')) 
      if (note == '') { note = 'N/A' } # account for missing exclude field in some xls sheet
      
      # tab<-read_xlsx(fullfn,sheet=sn,range=cell_limits(c(40, 1), c(NA, length(Cols))), 
      #                col_names=Cols) # row 44 is where recording starts
      
      # Now with the correct information we can overwrite the whole crap:
      tab<-read_xlsx(fullfn,sheet=sn,range=cell_limits(c(nHeaderlines+1, 1), c(NA,  ncol(tab))), 
                     col_names=Cols) 
      
      # Reading xlsx file with missing data will always cause warning New Name...
      # safe to ignore?
      # data starts in different lines in different experiments
      # drop first data point to get exactly 7min. -> 10500 data points
      # select the last 10500 data points from the table
      tab<-tail(tab,n=npts)
      
      
      
      print(paste('ID = ',id,', ',tab %>% nrow(),' rows, duration = ', 
                  tab %>% select('Recording time') %>% 
                    slice(tail(row_number(), 1)),'s',sep = ""))
      # this seems a ridiculous round about way but it works. 
      # Should print:  ID = correct ID, 10500 rows, duration = 420s
      
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
      
      
      
      # are there any missing data in Area, Activity?
      missingArea <- sum(is.na(df$Area)) 
      missingActivity <- sum(is.na(df$Activity)) 
      # Now add data to summary table:
      
      summary_table<- summary_table %>% add_row(ID=id,
                                                BW=bodyweight,
                                                AGE=age,
                                                STRAIN=strain,
                                                DRUG=drug,
                                                GROUP=group,
                                                TrialName = trialName,
                                                ArenaName = arenaName,
                                                ArenaSecs= countActivityArena,                   
                                                BSsecs=countActivityBSsecs,
                                                BWsecs= countActivityBWsecs,
                                                AVRBSsecs=countActivityAVRBSsecs,
                                                ThreshArenaSecs = ThreshArenaSecs,
                                                ThreshBSsecs = ThreshBSsecs,
                                                ThreshBWsecs = ThreshBWsecs,
                                                ThreshAVRBSsecs = ThreshAVRBSsecs,
                                                MissingArea = missingArea,
                                                MissingActivity = missingActivity,
                                                Exclude = exclude,
                                                Note = note,
                                                VideoFile = videoFile
      )
    } 
  } 
}
### end of loop

# sort by ID because less messy
summary_table <- arrange(summary_table, ID)
summary_table <- summary_table %>% distinct() # activate this if you want to remove identical rows

# # write to CSV
write_csv(summary_table,paste(folder,'summary.csv',sep="/"),col_names=TRUE)


#plot: makes sense?
#ggplot(summary_table) + # the plus has to be at the end of this line
#geom_point(mapping=aes(x=BSsecs,y=AVRBSsecs)) 





