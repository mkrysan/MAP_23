### CleaningFile.R 
### Author: MarkK 
### Date: 29 – Aug 2023

# Purpose: Creating Loop to clean raw data

## Library
library(tidyverse)
library(R.matlab)
library(stringr)
library(readxl)

# Read in DAQ info and filter for subjects deemed worth for analysis. 
disp = read_excel("/Users/markkrysan/Library/CloudStorage/OneDrive-GrinnellCollege/MAP_SUM23/ISBRG_data/Other/output_ISBRG_S1_v2.xls")
include_list = c("1","2","3","5","6","9","10","11","13","14","15","16","17","18","19",
                 "21","22","23","24","25","26","27","1004","1007","1008","1028","1029","1030","2012","2020")
disp = disp %>% filter(Subject %in% include_list)

drive1 = disp %>% filter(Drive == 1)
drive2 = disp %>% filter(Drive == 2)
drive3 = disp %>% filter(Drive == 3)
drive4 = disp %>% filter(Drive == 4)

#File Header for .mat file
ht = "/Users/markkrysan/Library/CloudStorage/OneDrive-GrinnellCollege/MAP_SUM23/ISBRG_data/MatFiles/MatFiles/"

#File List
file_list1 = unique(str_replace_all(drive1$DaqName, ".daq", ""))
file_list2 = unique(str_replace_all(drive2$DaqName, ".daq", ""))
file_list3 = unique(str_replace_all(drive3$DaqName, ".daq", ""))
file_list4 = unique(str_replace_all(drive4$DaqName, ".daq", ""))



test = readMat("/Users/markkrysan/Library/CloudStorage/OneDrive-GrinnellCollege/MAP_SUM23/ISBRG_data/MatFiles/MatFiles/NADS_IMPAIR_1_20220309112943.mat")
lp_idx = which(attr(test$elemDataI, "dimnames")[[1]] == "SCC.Lane.Deviation")
logstr_idx = which(attr(test$elemDataI, "dimnames")[[1]] == "SCC.LogStreams")
brk_pdl_force_id = which(attr(test$elemDataI, "dimnames")[[1]] ==  "CFS.Brake.Pedal.Force")
acl_pdl_pos_id = which(attr(test$elemDataI, "dimnames")[[1]] == "CFS.Accelerator.Pedal.Position")
spd_id = which(attr(test$elemDataI, "dimnames")[[1]] ==  "VDS.Veh.Speed")
Frames_id = which(attr(test$elemDataI, "dimnames")[[1]] ==  "Frames")


# test.out1 = data.frame(event = "Dark", lp = lp[dark_event], brk_pdl_force = brk_pdl_force[dark_event],
#                       acl_pdl_pos = acl_pdl_pos[dark_event], spd = spd[dark_event], 
#                       lv = l_vel[dark_event])
# test.out2 = data.frame(event = "gravel", lp = lp[gravel_event], brk_pdl_force = brk_pdl_force[gravel_event],
#                        acl_pdl_pos = acl_pdl_pos[gravel_event], spd = spd[gravel_event], 
#                        lv = l_vel[gravel_event])
# test.out3 = data.frame(event = "rural straight", lp = lp[rural_straight], brk_pdl_force = brk_pdl_force[rural_straight],
#                        acl_pdl_pos = acl_pdl_pos[rural_straight], spd = spd[rural_straight], 
#                        lv = l_vel[rural_straight])
# test.out = rbind(test.out1, test.out2, test.out3)

outfile1 = disp %>% select(c(1:7)) %>% filter(Subject %in% include_list, Drive == 1)
outfile2 = disp %>% select(c(1:7)) %>% filter(Subject %in% include_list, Drive == 2)
outfile3 = disp %>% select(c(1:7)) %>% filter(Subject %in% include_list, Drive == 3)
outfile4 = disp %>% select(c(1:7)) %>% filter(Subject %in% include_list, Drive == 4)



pb = txtProgressBar(min = 0, max = length(file_list), initial = 0) 

clean_function = function(file_list, outfile){
  final_df = NULL
  for(i in 1:length(file_list)){
      dark_event = NULL
      gravel_event = NULL
      rural_straight = NULL
      tdata = readMat.default(paste0(ht, file_list[i], ".mat"))
      info = outfile %>% filter(DaqName == paste(file_list[i],".daq", sep = ""))
      
      lp = tdata$elemDataI[[lp_idx]][,2]
      brk_pdl_force = tdata$elemDataI[[brk_pdl_force_id]][,1]
      acl_pdl_pos = tdata$elemDataI[[brk_pdl_force_id]][,1]
      spd = tdata$elemDataI[[spd_id]][,1]
      l_vel= tdata$elemDataI[[lp_idx]][,2] - lag(tdata$elemDataI[[lp_idx]][,2], 1)*60
      frames = tdata$elemDataI[[Frames_id]][,1]
      
      dark_event = tdata$elemDataI[[logstr_idx]][,2] == 304
      gravel_event = tdata$elemDataI[[logstr_idx]][,2] == 306
      rural_straight = tdata$elemDataI[[logstr_idx]][,2] == 311
      
      #info[,"DaqName"] = sub("(.*)_.*","\\2", info[,"DaqName"])
      
      if(sum(dark_event)>0){
      temp.1 = data.frame(EventNum = 304, EventName = "Dark", Count = frames[dark_event]-(frames[dark_event][1]-1), lp = lp[dark_event], brk_pdl_force = brk_pdl_force[dark_event],
                             acl_pdl_pos = acl_pdl_pos[dark_event], spd = spd[dark_event], 
                             lv = l_vel[dark_event])
      temp.1 = cbind(info[2,], temp.1)
      }else{
        temp.1 = NULL
      }
      if(sum(gravel_event>0)){
      temp.2 = data.frame(EventNum = 306, EventName = "Gravel", Count = frames[gravel_event]-(frames[gravel_event][1]-1),lp = lp[gravel_event], brk_pdl_force = brk_pdl_force[gravel_event],
                             acl_pdl_pos = acl_pdl_pos[gravel_event], spd = spd[gravel_event], 
                             lv = l_vel[gravel_event])
      temp.2 = cbind(info[4,], temp.2)
      }else{
        temp.2 = NULL
      }
      if(sum(rural_straight)){
      temp.3 = data.frame(EventNum = 311, EventName = "RuralStr", Count = frames[rural_straight]-(frames[rural_straight][1]-1),lp = lp[rural_straight], brk_pdl_force = brk_pdl_force[rural_straight],
                             acl_pdl_pos = acl_pdl_pos[rural_straight], spd = spd[rural_straight], 
                             lv = l_vel[rural_straight])
      temp.3 = cbind(info[6, ], temp.3)
      }else{
        temp.3 = NULL
      }
      temp = rbind( temp.1, temp.2, temp.3)
      #setTxtProgressBar(pb,i)
      final_df = rbind(final_df, temp)
  }
  final_df
}

clean_function(file_list1[1])

drive1 = clean_function(file_list1, outfile1)

write.csv(clean_function(file_list1, outfile1),"/Users/markkrysan/Library/CloudStorage/OneDrive-GrinnellCollege/MAP_SUM23/MarkFall/Data/Cleaned_Drive1.csv")
write.csv(clean_function(file_list2,outfile2),"/Users/markkrysan/Library/CloudStorage/OneDrive-GrinnellCollege/MAP_SUM23/MarkFall/Data/Cleaned_Drive2.csv")
write.csv(clean_function(file_list3,outfile3),"/Users/markkrysan/Library/CloudStorage/OneDrive-GrinnellCollege/MAP_SUM23/MarkFall/Data/Cleaned_Drive3.csv")
write.csv(clean_function(file_list4,outfile4),"/Users/markkrysan/Library/CloudStorage/OneDrive-GrinnellCollege/MAP_SUM23/MarkFall/Data/Cleaned_Drive4.csv")











