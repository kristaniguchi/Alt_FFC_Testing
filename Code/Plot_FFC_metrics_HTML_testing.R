# Run the functional flows calculator and plot results on interactive hydrograph
# Ethan Baruch
# 5/5/2022
#Minor modifications two plot two hydrographs at once by Cameron Carpenter 
# 01/25/2023


library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);
library(ggplot2);library(dplyr)


#########Plot 1 sets of metrics########

#Section for new users
#Ask User How many hydrographs they want to plot



Token <- readline("paste Token: ");

plot_num <- readline("Enter the number of hydrographs to be evaluated,  between 1 and 3: ");

if(plot_num == 1) {
  anlysis_type <- readline("Enter type of data to be used ('gage' for usgs gage or 'csv' for already downloaded timeseries ) ")
}

output_loc <- readline("Please Enter the desired output location for figures: ")


####Enter Gage and Comid####

#Information needed to run function (replace the info below with your gage of interest and its associated COMID)
gage_1<-10293000 # change as needed, enter gage here
gage_2<-11289650
#ex: tuolumne at La Grange usgs gage 11289650
comid_1<- 8915857 #From determine this manually using the Natural Flows Database
comid_2<-2823750 #
#ex: tuolumne at la grange, comid=2823750

####Run the FFC####
#Paste token here currently Kris'
Token <- 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE2OTE1MzY5ODN9.MgbahOzKu59eNbFtToaBdel3mHy2Le4tTCOKXzJgZdg'


###For observed flow data from a USGS gage

#Example
#USGS gage: 11378800, Red bank Creek
#COMID: 12068268

#gage <- 11014000 #enter USGS gage ID for given site, this is for Kekawaka Creek

#comid <- 20334508 #enter COMID for site, this is for Kekawaka Creek

#run the FFC for the two sites
FFC_out_1 <- ffcAPIClient::evaluate_gage_alteration(gage_id = gage_1, token = Token, 
                                                       comid = comid_1, plot_results = FALSE)

FFC_out_2 <- ffcAPIClient::evaluate_gage_alteration(gage_id = gage_2, token = Token, 
                                                    comid = comid_2, plot_results = FALSE)


#Save flow data
Flow_1 <- FFC_out_1[["timeseries"]] %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

Flow_2 <- FFC_out_2[["timeseries"]] %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

###

Flow_1 <- Flow_1[,c("date","flow_interp_cfs")]

Flow_1 <- Flow_1 %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))


Flow_1 <- Flow_1 %>%
  rename("flow" = "flow_interp_cfs")

Flow_1$flow <- modeled_flows$flow_interp_cfs
###For observed flow data from data frame. Must have date and flow columns

Flow_1 <- read.csv("~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Tuolumne River/Flow Data/clean_fnf_daily_dwr_TLG_cdec.csv", header = T) %>% #Enter first flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))

Flow_2 <- read.csv("/Users/cameroncarpenter/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Tuolumne River/Flow Data/Tuol_40_FNF_Varied_FERC.csv", header = T) %>% #Enter Second set of Flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))


Flow_3 <- read.csv("/Users/cameroncarpenter/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Tuolumne River/Flow Data/Toul_observed_mean_flow.csv", header = T) %>% #Enter Second set of Flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))

comid <- 2823750 ##tuolumne at la grange, comid=2823750, MFY Our House comid = 8063597, Pit 4 comid = 7952784, Los Gatos Creek comid = 14883269
###

#STEP 2 Add water year
FlowYear_1 <- attach_water_year_data(Flow_1, date_field = "date") %>% 
  mutate(date = format(date, "%m/%d/%Y")) #reformat date 

FlowYear_2 <- attach_water_year_data(Flow_2, date_field = "date") %>% 
  mutate(date = format(date, "%m/%d/%Y")) #reformat date 


FlowYear_3 <- attach_water_year_data(Flow_3, date_field = "date") %>% 
  mutate(date = format(date, "%m/%d/%Y")) #reformat date 

#Step 3 Run the FFC
FFC_out_1 <- evaluate_alteration(FlowYear_1, token = Token, 
                               comid = comid, plot_results = FALSE)

FFC_out_2 <- evaluate_alteration(FlowYear_2, token = Token, 
                               comid = comid, plot_results = FALSE)

FFC_out_3 <- evaluate_alteration(FlowYear_3, token = Token, 
                                 comid = comid, plot_results = FALSE)

###




#Step 4 save observed metrics for each year 
FF_Observe_1 <- FFC_out_1[["ffc_results"]] %>% 
  mutate(Year = as.integer(Year)) 


FF_Observe_2 <- FFC_out_2[["ffc_results"]] %>% 
  mutate(Year = as.integer(Year))

FF_Observe_3 <- FFC_out_3[["ffc_results"]] %>% 
  mutate(Year = as.integer(Year))



FF_Observe_1$SP_Tim <- Temp_Results_2$SP_Tim

###FOR BOX PLOT ONLY STOP HERE FOR ALTERED GO TO BOX PLOT CODE###



#########Plot 1 sets of metrics########

#Flow metrics are plotted as a magnitude at the start timing of each flow characteristic

#Make sure you have all your flow data defined 
#Use the outputs from the previous script for this plotting function


# Find date of all timing metrics

# NOTE: 1) timing is output in water year days. Need to convert to Julian days before plotting
#       2) Metrics from the original calculator need to be adjusted by -93 and 274, since python indexes different than R


# NOTE: timing is output in water year days. Need to convert to Julian days before plotting
FF_Dates <- MERCED_NR_STEVINSON__Summer_2 %>%  #Update the name of the data from
  mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y')-92, #Date in Julian days
         FA_Tim_Dt = as.Date(paste(FA_Tim, Year-1), '%j %Y')+273, #add 274 because water year starts Oct 1
         Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y')-92, #Date in Julian days
         SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y')-92) #Date in Julian days

# Add timing for each flow metric
Flow_MetTim$DS <- flow %>% 
  left_join(select(FF_Dates, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt"))%>% 
  left_join(select(FF_Dates, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt"))%>% 
  left_join(select(FF_Dates, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
  left_join(select(FF_Dates, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))

# Create interactive figure
Flow_metric_fig <- plot_ly(data = Flow_MetTim, x = ~date, y = ~flow, type = 'scatter', mode = 'line',
                           name = "Discharge") %>%
  add_trace(y = ~Ann_Metrics.Mean_Ann_Flow,  name = 'DS_Mag_50 + DS_Tim', mode = 'markers') %>%  #Add DS timing
  add_trace(y = ~FA_Mag,  name = 'FA_Mag + FA_Tim', mode = 'markers') %>%  #Add Fall timing
  add_trace(y = ~Wet_BFL_Mag_10,  name = 'Wet_BFL_Mag_10 + Wet_Tm', mode = 'markers') %>%  #Add Wet timing
  add_trace(y = ~SP_met_and_DS_Tim.SP_Mag,  name = 'SP_Mag + SP_Tm', mode = 'markers') %>%  #Add Spring timing
  layout(title = "Merced River Near Stevinson", #Change site name here
         xaxis = list(title = "Date"),
         yaxis = list (title = "Streamflow (CFS)"))

Flow_metric_fig


#Save interactive hydrograph as .html webpage
htmlwidgets::saveWidget(as_widget(Flow_metric_fig), "Merced_River_new Summer_with_half_median.html")






#########Plot metrics 2 sets of metrics########

#Flow metrics are plotted as a magnitude at the start timing of each flow characteristic

#Make sure you have all your flow data defined 
#Use the outputs from the previous script for this plotting function


#Set the names of the data of interest

name_1 <- "Observed Flow Data" 

name_2 <- "FERC Flow Data"


# Find date of all timing metrics

# NOTE: 1) timing is output in water year days. Need to convert to Julian days before plotting
#       2) Metrics from the original calculator need to be adjusted by -93 and 274, since python indexes different than R
FF_Dates_1 <- X6_19_23_MFY_Our_House_Observed_Flow_Metrics %>%  #Update the name of the data frame in this line
  mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y')-92, #Date in Julian days
         FA_Tim_Dt = as.Date(paste(FA_Tim, Year-1), '%j %Y')+273, #add 273 because water year starts Oct 1
         Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y')-92, #Date in Julian days
         SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y')-92) #Date in Julian days

FF_Dates_2 <- MFY_check %>% #Update the name of the data frame in this line
  mutate(DS_Tim_Dt = as.Date(paste((DS_Tim-3), Year), '%j %Y')-92, #Date in Julian days
         FA_Tim_Dt = as.Date(paste(FA_Tim, Year-1), '%j %Y')+273, #add 274 because water year starts Oct 1
         Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y')-92, #Date in Julian days
         SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y')-92) #Date in Julian days

# Add timing for each flow metric, You can add your own metrics here too!
Flow_MetTim_1 <- flow %>% #Update the flow data to make sure it is the same data that was used to make the metrics
  left_join(select(FF_Dates_1, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt"))%>% 
  left_join(select(FF_Dates_1, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt"))%>% 
  left_join(select(FF_Dates_1, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
  left_join(select(FF_Dates_1, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))# Only if adding hand calced metrics %>%

Flow_MetTim_2 <- flow %>% #Update the flow data to make sure it is the same data that was used to make the metrics
  left_join(select(FF_Dates_2, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt"))%>% 
  left_join(select(FF_Dates_2, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt"))%>% 
  left_join(select(FF_Dates_2, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
  left_join(select(FF_Dates_2, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))

#If you need you can trim the data from to the period of interest if necessary 
NewMettime_1 <- Flow_MetTim_1 %>% filter(date > '1999-10-01')
NewMettime_2 <- Flow_MetTim_2 %>% filter(date > '1999-10-01')



# Create interactive figure New Code
#Set up the legend Need to update flow name, UPDATE METICS AS SHAPES THEN ALL Triangle for FALL Pulse, Circle srping recession, square for dry season, diamond for wet season.
#colors <- c("Observed flow" = "red", "Full Natural Flow" = "blue","40% FNF with FERC Flows" = "chartreuse3") #Have to update with new names each time
#shapes <- c("DS_Mag_50 + DS_Tim_Flow" = 1, "FA_Mag + FA_Tim_Flow" = 0 ,"Wet_BFL_Mag_10"= 3,"SP_Mag + SP_Tm"= 1)

test_fig <- ggplot() +
  geom_line(data = Flow_MetTim_1, aes(x= date, y= flow, color = name_1))+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= DS_Mag_50, color = name_1, shape = "DS_Mag_50 + DS_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= FA_Mag, color = name_1, shape = "FA_Mag + FA_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= Wet_BFL_Mag_10, color = name_1, shape ="Wet_BFL_Mag_10"),size = 1.5)+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= SP_Mag, color = name_1, shape = "SP_Mag + SP_Tm"),size = 1.5)+
  geom_line(data = Flow_MetTim_2, aes(x= date, y= flow, color = name_2))+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= DS_Mag_50, color = name_2, shape = "DS_Mag_50 + DS_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= FA_Mag, color = name_2, shape = "FA_Mag + FA_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= Wet_BFL_Mag_10, color = name_2, shape = "Wet_BFL_Mag_10"),size = 1.5)+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= SP_Mag, color = name_3, shape = "SP_Mag + SP_Tm"),size = 1.5)+
  scale_colour_manual(values = c( "chartreuse3","blue"))+ #Change the plot colors here is wanted
  scale_shape_manual(values = c(0,2, 1, 5))+
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") + #Can adjust the frequency of the date labels 
  theme(axis.text.x=element_text(angle=-90, vjust=0.5)) +
  labs( x = "Date", y = "Streamflow (cfs)")+
  ggtitle("Your title here!! Wooo!") # Update the title here


print_fig<-ggplotly(test_fig, dynamicTicks = "y")


#print_fig.update
print(print_fig)  


#Save interactive hydrograph as .html webpage
htmlwidgets::saveWidget(widget = print_fig,file = "~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/FFC figures/Interim Results Photos/MFY Observed metric comparison_Winter_Check.html", selfcontained = TRUE)





#########Plot metrics 3 sets of metrics########

#Flow metrics are plotted as a magnitude at the start timing of each flow characteristic

#Make sure you have all your flow data defined 
#Use the outputs from the previous script for this plotting function


#Set the names of the data of interest

name_1 <- "Observed Flow Data" 

name_2 <- "FERC Flow Data"

name_3 <- "FNF Data"


# Find date of all timing metrics

# NOTE: 1) timing is output in water year days. Need to convert to Julian days before plotting
#       2) Metrics from the original calculator need to be adjusted by -93 and 274, since python indexes different than R
FF_Dates_1 <- X6_19_23_MFY_Our_House_Observed_Flow_Metrics %>%  #Update the name of the data frame in this line
  mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y')-92, #Date in Julian days
         FA_Tim_Dt = as.Date(paste(FA_Tim, Year-1), '%j %Y')+273, #add 273 because water year starts Oct 1
         Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y')-92, #Date in Julian days
         SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y')-92) #Date in Julian days

FF_Dates_2 <- MFY_check %>% #Update the name of the data frame in this line
  mutate(DS_Tim_Dt = as.Date(paste((DS_Tim-3), Year), '%j %Y')-92, #Date in Julian days
         FA_Tim_Dt = as.Date(paste(FA_Tim, Year-1), '%j %Y')+273, #add 274 because water year starts Oct 1
         Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y')-92, #Date in Julian days
         SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y')-92) #Date in Julian days

FF_Dates_3 <- New_Summer_Obs_MFY_merge %>% #Update the name of the data frame in this line
  mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y')-92, #Date in Julian days
         FA_Tim_Dt = as.Date(paste(FA_Tim, Year-1), '%j %Y')+274, #add 274 because water year starts Oct 1
         Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y')-92, #Date in Julian days
         SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y')-92) #Date in Julian days


# Add timing for each flow metric, You can add your own metrics here too!
Flow_MetTim_1 <- flow %>% #Update the flow data to make sure it is the same data that was used to make the metrics
  left_join(select(FF_Dates_1, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt"))%>% 
  left_join(select(FF_Dates_1, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt"))%>% 
  left_join(select(FF_Dates_1, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
  left_join(select(FF_Dates_1, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))# Only if adding hand calced metrics %>%

Flow_MetTim_2 <- flow %>% #Update the flow data to make sure it is the same data that was used to make the metrics
  left_join(select(FF_Dates_2, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt"))%>% 
  left_join(select(FF_Dates_2, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt"))%>% 
  left_join(select(FF_Dates_2, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
  left_join(select(FF_Dates_2, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))


Flow_MetTim_3 <- flow %>% #Update the flow data to make sure it is the same data that was used to make the metrics
  left_join(select(FF_Dates_3, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt"))%>% 
  left_join(select(FF_Dates_3, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt"))%>% 
  left_join(select(FF_Dates_3, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
  left_join(select(FF_Dates_3, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt")) 

#If you need you can trim the data from to the period of interest if necessary 
NewMettime_1 <- Flow_MetTim_1 %>% filter(date > '1999-10-01')
NewMettime_2 <- Flow_MetTim_2 %>% filter(date > '1999-10-01')
NewMettime_3 <- Flow_MetTim_3 %>% filter(date > '1999-10-01')


# Create interactive figure New Code
#Set up the legend Need to update flow name, UPDATE METICS AS SHAPES THEN ALL Triangle for FALL Pulse, Circle srping recession, square for dry season, diamond for wet season.
#colors <- c("Observed flow" = "red", "Full Natural Flow" = "blue","40% FNF with FERC Flows" = "chartreuse3") #Have to update with new names each time
#shapes <- c("DS_Mag_50 + DS_Tim_Flow" = 1, "FA_Mag + FA_Tim_Flow" = 0 ,"Wet_BFL_Mag_10"= 3,"SP_Mag + SP_Tm"= 1)

test_fig <- ggplot() +
  geom_line(data = Flow_MetTim_1, aes(x= date, y= flow, color = name_1))+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= DS_Mag_50, color = name_1, shape = "DS_Mag_50 + DS_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= FA_Mag, color = name_1, shape = "FA_Mag + FA_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= Wet_BFL_Mag_10, color = name_1, shape ="Wet_BFL_Mag_10"),size = 1.5)+
  geom_point(data = Flow_MetTim_1, aes(x= date, y= SP_Mag, color = name_1, shape = "SP_Mag + SP_Tm"),size = 1.5)+
  geom_line(data = Flow_MetTim_2, aes(x= date, y= flow, color = name_2))+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= DS_Mag_50, color = name_2, shape = "DS_Mag_50 + DS_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= FA_Mag, color = name_2, shape = "FA_Mag + FA_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= Wet_BFL_Mag_10, color = name_2, shape = "Wet_BFL_Mag_10"),size = 1.5)+
  geom_point(data = Flow_MetTim_2, aes(x= date, y= SP_Mag, color = name_3, shape = "SP_Mag + SP_Tm"),size = 1.5)+
  geom_line(data = Flow_MetTim_3, aes(x= date, y= flow, color = name_3))+
  geom_point(data = Flow_MetTim_3, aes(x= date, y= DS_Mag_50, color = name_3, shape = "DS_Mag_50 + DS_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_3, aes(x= date, y= FA_Mag, color = name_3, shape = "FA_Mag + FA_Tim_Flow"),size = 1.5)+
  geom_point(data = Flow_MetTim_3, aes(x= date, y= Wet_BFL_Mag_10, color = name_3, shape = "Wet_BFL_Mag_10"),size = 1.5)+
  geom_point(data = Flow_MetTim_3, aes(x= date, y= SP_Mag, color = name_3, shape = "SP_Mag + SP_Tm"),size = 1.5)+
  scale_colour_manual(values = c( "chartreuse3","blue", "red"))+ #Change the plot colors here is wanted
  scale_shape_manual(values = c(0,2, 1, 5))+
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") + #Can adjust the frequency of the date labels 
  theme(axis.text.x=element_text(angle=-90, vjust=0.5)) +
  labs( x = "Date", y = "Streamflow (cfs)")+
  ggtitle("Your title here!! Wooo!") # Update the title here


print_fig<-ggplotly(test_fig, dynamicTicks = "y")


#print_fig.update
print(print_fig)  
  

#Save interactive hydrograph as .html webpage
htmlwidgets::saveWidget(widget = print_fig,file = "~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/FFC figures/Interim Results Photos/MFY Observed metric comparison_Winter_Check.html", selfcontained = TRUE)





###Making Box Charts for all the metrics in the FF_observed databases####


####Replace old metics with new metrics
for (i in cnames){
  #cat(i)
  cat(i %in% Hand_Calculation_Spreadsheet_4e_MFY)
  if (any(Hand_Calculation_Spreadsheet_4e_MFY$i)) {
    Hand_Calculation_Spreadsheet_4e_MFY$i
    FF_Observe_3$i <- Hand_Calculation_Spreadsheet_4e_MFY$i
  }
}


FOR_BOX_PLOT <- FF_Observe_1 %>% filter(Year>=2011) #Filter 

OBS_CALCED_Metrics <- read_csv(Filename.csv, header = T) #

FOR_BOX_PLOT[SP_Dur] <- OBS_CALCED_Metrics[SP_Dur] #repeat for each hand calculated metric


#first get all the unique column names 
combined_df <- rbind(FF_Observe_1,FF_Observe_2,FF_Observe_3)
cnames <- colnames(combined_df)
Year_Typ <- c("All Years","wet year", "mod year", "dry year")


Box_plot_print<- function(FF_Obs_Observed_Flows, FF_Obs_FNF, FF_Obs_4e,Year_type){
  for (col in cnames[2:20]) {
    #Check to see if the entire column are NA or NAN values
    if (all(is.na(FF_Obs_4e[[col]]) | is.nan(FF_Obs_4e[[col]]))) {
      # Set all values in the column to 0
      FF_Obs_4e[[col]] <- 0
    }
    #Move data from the dataframe to the placeholder list
    placeholder1 <- FF_Obs_4e[col]
    
    #Check to see if the entire column are NA or NAN values
    if (all(is.na(FF_Obs_Observed_Flows[[col]]) | is.nan(FF_Obs_Observed_Flows[[col]]))) {
      # Set all values in the column to 0
      FF_Obs_Observed_Flows[[col]] <- 0
    }
    
    placeholder2 <- FF_Obs_Observed_Flows[col]
    
    #Check to see if the entire column are NA or NAN values
    if (all(is.na(FF_Obs_FNF[[col]]) | is.nan(FF_Obs_FNF[[col]]))) {
      # Set all values in the column to 0
      FF_Obs_FNF[[col]] <- 0
    }
    
    
    placeholder3 <- FF_Obs_FNF[col]
    # Check if all values in the list are NA or NaN
    
    cat(col)
    
    Boxplot <- ggplot()+
      geom_boxplot(aes(x= '40% FNF with FERC Flows', y=as.numeric(unlist(placeholder1))), color = "green")+
      geom_jitter(aes(x= '40% FNF with FERC Flows', y=as.numeric(unlist(placeholder1))),width = 0.2, height = 0, color = "darkgreen", alpha = 0.6) +
      geom_boxplot(aes(x= 'Observed Flows', y=as.numeric(unlist(placeholder2))) , color = 'red')+
      geom_jitter(aes(x= 'Observed Flows', y=as.numeric(unlist(placeholder2))) ,width = 0.2, height = 0, color = "darkred", alpha = 0.6) +
      geom_boxplot(aes(x= 'Unimpar Flows', y= as.numeric(unlist(placeholder3))), color = 'blue' )+
      geom_jitter(aes(x= 'Unimpar Flows', y= as.numeric(unlist(placeholder3))),width = 0.2, height = 0, color = "darkblue", alpha = 0.6) +
      labs( x = "Flow Regimes", y = paste(col), title=paste("Tuolume River Near LaGrange Dam | Year Type:",typ,", Metric:",col))#+ Need to update based the gage that is being evaluated
    #ggtitle("Middle Fork Yuba Flow Conditions Comparison Using Current FFC")
    
    plot(Boxplot)
    ggsave(paste0(output_loc,"/Box_Plots_Update_Year_types_split/",typ,"/", col, ".png"), plot = Boxplot, device = "png")
  }
}

##Now make comparison Bar charts Need to run through step 4 to produce
##Loop through all the column names besides years to make the box plots
for (typ in Year_Typ){
  if(typ == "All Years"){
    Box_plot_print(Alt_calc_LGN_observed,TLG_FNF_Metics,Alt_calc_40_FNF_Varied_FERC,typ)
  }
  if(typ == "wet year"){
    cat(typ)
    FF_Obs_Observed_Flows <- filter(Alt_calc_LGN_observed, WY_Cat == typ)
    FF_Obs_FNF <- filter(TLG_FNF_Metics, WY_Cat == typ)
    FF_Obs_4e <- filter(Alt_calc_40_FNF_Varied_FERC, WY_Cat == typ)
    Box_plot_print(FF_Obs_Observed_Flows,FF_Obs_FNF,FF_Obs_4e,typ)
  }
  if(typ == "mod year"){
    cat(typ)
    FF_Obs_Observed_Flows <- filter(Alt_calc_LGN_observed, WY_Cat == typ)
    FF_Obs_FNF <- filter(TLG_FNF_Metics, WY_Cat == typ)
    FF_Obs_4e <- filter(Alt_calc_40_FNF_Varied_FERC, WY_Cat == typ)
    Box_plot_print(FF_Obs_Observed_Flows,FF_Obs_FNF,FF_Obs_4e,typ)
  }
  if(typ == "dry year"){
    cat(typ)
    FF_Obs_Observed_Flows <- filter(Alt_calc_LGN_observed, WY_Cat == typ)
    FF_Obs_FNF <- filter(TLG_FNF_Metics, WY_Cat == typ)
    FF_Obs_4e <- filter(Alt_calc_40_FNF_Varied_FERC, WY_Cat == typ)
    Box_plot_print(FF_Obs_Observed_Flows,FF_Obs_FNF,FF_Obs_4e, typ)
  }
}

FOR_BOX_PLOT <- FF_Observe_1 %>% filter(Year>2011)


####HERE is some working code for making box plots with a various number of dataframes

for (col in cnames) {
  cat(col)
  
  Boxplot <- ggplot()+
    geom_boxplot(aes(x= col, y= as.numeric(FF_Observe_deer[[col]])), color = 'blue' )+
    geom_jitter(aes(x= col, y= as.numeric(FF_Observe_deer[[col]]),width = 0.2, height = 0, color = "darkblue", alpha = 0.6) +
                  labs( x = "Flow Regimes", y = paste(col), title=paste("Deer Creek | Metric:",col))+ #Need to update based the gage that is being evaluated
                ggtitle("Middle Fork Yuba Flow Conditions Comparison Using Current FFC"))
                
                
                plot(Boxplot)
                
                ggsave(paste0("~/Desktop/School Stuff/UC Davis/Work/Kelly Help","/Box_Plots/", col, ".png"), plot = Boxplot, device = "png")
}

cnames <- colnames(FF_Observe_2)
colour = 'blue'
  
for (col in cnames) {
  cat(col)
  
  Boxplot <- ggplot() +
    geom_boxplot(aes(x = col, y = as.numeric(FF_Observe_deer[[col]],colour = 'blue') )) +
    geom_jitter(aes(x = col, y = as.numeric(FF_Observe_deer[[col]],colour = 'darkblue',alpha = 0.6), width = 0.2, height = 0)) +
    labs(x = "Flow Regimes", y = col, title = paste("Deer Creek | Metric:", col))
  
  print(Boxplot)
  ggsave(paste0("~/Desktop/School Stuff/UC Davis/Work/Kelly Help", "/Box_Plots/", col, ".png"), plot = Boxplot, device = "png")
}



create_boxplots <- function(...) {
  # Retrieve the number of dataframes
  num_dataframes <- length(list(...))
  
  # Create box plots for each dataframe
  for (i in 1:num_dataframes) {
    # Extract the current dataframe
    df <- list(...)[[i]]
    
    # Create the box plot
    boxplot(df, main = paste("Box Plot - DataFrame", i))
  }
}

# Example dataframes
df1 <- data.frame(A = rnorm(100), B = rnorm(100))
df2 <- data.frame(C = rnorm(100), D = rnorm(100), E = rnorm(100))

# Create box plots for the dataframes
create_boxplots(df1, df2)

compare_boxplots <- function(output_loc,...) {
  # Retrieve the dataframes and their names
  dataframes <- list(...)
  names_arg <- as.list(match.call(expand.dots = FALSE)[-1])
  names_df <- unlist(names_arg)[-1]
  names_df <- unlist(names_df)
  num_dataframes <- length(dataframes)
  
  # Get common column names
  common_cols <- Reduce(intersect, lapply(dataframes, colnames))
  
  #Set the colors for each of the potential 
  color_base <- c("blue","chartreuse3","red")
  jitter_color <- c("darkblue","darkgreen","darkred")
  
  # Generate colors for each dataframe
  colors <- color_base[1:num_dataframes]
  jit_col <- jitter_color[1:num_dataframes]
  Year_Typ <- c("All year","wet year", "mod year", "dry year")
  for (typ in Year_Typ) {
    if(typ == "All year") {
      # Create box plots for common columns
      for (col in common_cols[-1]) {
      boxplot_data <- lapply(dataframes, function(df) df[[col]])
    
      cat("\n year type: ", typ, " metric: ", col)
      
      # Add jitter to the box plots
      jitter_data <- lapply(boxplot_data, jitter, amount = 0.4)
    
      # Create the box plot with custom colors
      boxplot(jitter_data, names = names_df, col = colors, 
            main = paste("Tuolumne River at LaGrange /n comid = 2823750  -", col, " - ", typ,"s"), ylab = col)
      stripchart(jitter_data,
               method = "jitter",
               pch = 19,
               bg = 10,
               col = jit_col,
               vertical = TRUE,
               add = TRUE)
    
      # Add a legend
      legend("topright", legend = names_df, fill = colors, border = NA)
    
      # Save the plot
      file_name <- paste(output_loc,"/box_plots/",typ,"s/", col," Metric - ", typ, "s", ".png", sep = "")
      cat("\n", output_loc)
      dev.copy(png, file_name)
      dev.off()
      
    }
    }
    else{
      filtered_dataframe <- filter(dataframes, WY_cat == typ)
      
      # Create box plots for common columns
      for (col in common_cols[-1]) {
        boxplot_data <- lapply(filtered_dataframe, function(df) df[[col]])
        
        # Add jitter to the box plots
        jitter_data <- lapply(boxplot_data, jitter, amount = 0.4)
        
        # Create the box plot with custom colors
        boxplot(jitter_data, names = names_df, col = colors, 
                main = paste("Tuolumne River at LaGrange /n comid = 2823750 -", col, " - ", typ,"s"), ylab = col)
        stripchart(jitter_data,
                   method = "jitter",
                   pch = 19,
                   bg = 10,
                   col = jit_col,
                   vertical = TRUE,
                   add = TRUE)
        
        # Add a legend
        legend("topright", legend = names_df, fill = colors, border = NA)
        
        # Save the plot
        file_name <- paste(output_loc,"/box_plots/",typ,"s/", col," Metric, ", typ, "s", ".png", sep = "")
        dev.copy(png, file_name)
        dev.off()
        
      }
    }
    
  }
}

library(grDevices) # Load the grDevices package for dev.print()

compare_boxplots2 <- function(output_loc, ...) {
  # Retrieve the dataframes and their names
  dataframes <- list(...)
  names_arg <- as.list(match.call(expand.dots = FALSE)[-1])
  names_df <- unlist(names_arg)[-1]
  names_df <- unlist(names_df)
  num_dataframes <- length(dataframes)
  
  # Get common column names
  common_cols <- Reduce(intersect, lapply(dataframes, colnames))
  
  # Set the colors for each of the potential
  color_base <- c("blue", "chartreuse3", "red")
  jitter_color <- c("darkblue", "darkgreen", "darkred")
  
  # Generate colors for each dataframe
  colors <- color_base[1:num_dataframes]
  jit_col <- jitter_color[1:num_dataframes]
  Year_Typ <- c("All year", "wet year", "mod year", "dry year")
  for (typ in Year_Typ) {
    if (typ == "All year") {
      # Create box plots for common columns
      for (col in common_cols[-1]) {
        boxplot_data <- lapply(dataframes, function(df) df[[col]])
        
        cat("\n year type: ", typ, " metric: ", col)
        
        # Add jitter to the box plots
        jitter_data <- lapply(boxplot_data, jitter, amount = 0.4)
        
        # Create the box plot with custom colors
        boxplot(jitter_data, names = names_df, col = colors,
                main = paste("Tuolumne River at LaGrange /n comid = 2823750  -", col, " - ", typ, "s"), ylab = col)
        stripchart(jitter_data,
                   method = "jitter",
                   pch = 19,
                   bg = 10,
                   col = jit_col,
                   vertical = TRUE,
                   add = TRUE)
        
        # Add a legend
        legend("topright", legend = names_df, fill = colors, border = NA)
        
        # Save the plot
        file_name <- paste(output_loc, "/box_plots/", typ, "s/", col, " Metric - ", typ, "s", ".png", sep = "")
        cat("\n", output_loc)
        png(file_name)
        boxplot(jitter_data, names = names_df, col = colors,
                main = paste("Tuolumne River at LaGrange /n comid = 2823750  -", col, " - ", typ, "s"), ylab = col)
        stripchart(jitter_data,
                   method = "jitter",
                   pch = 19,
                   bg = 10,
                   col = jit_col,
                   vertical = TRUE,
                   add = TRUE)
        legend("topright", legend = names_df, fill = colors, border = NA)
        dev.print(png, target = NULL) # Use dev.print() to save the plot
        graphics.off() # Close the current graphics device
      }
    } else {
      filtered_dataframe <- filter(dataframes, WY_cat == typ)
      
      # Create box plots for common columns
      for (col in common_cols[-1]) {
        boxplot_data <- lapply(filtered_dataframe, function(df) df[[col]])
        
        # Add jitter to the box plots
        jitter_data <- lapply(boxplot_data, jitter, amount = 0.4)
        
        # Create the box plot with custom colors
        boxplot(jitter_data, names = names_df, col = colors,
                main = paste("Tuolumne River at LaGrange /n comid = 2823750 -", col, " - ", typ, "s"), ylab = col)
        stripchart(jitter_data,
                   method = "jitter",
                   pch = 19,
                   bg = 10,
                   col = jit_col,
                   vertical = TRUE,
                   add = TRUE)
        
        # Add a legend
        legend("topright", legend = names_df, fill = colors, border = NA)
        
        # Save the plot
        file_name <- paste(output_loc, "/box_plots/", typ, "s/", col, " Metric, ", typ, "s", ".png", sep = "")
        png(file_name)
        boxplot(jitter_data, names = names_df, col = colors,
                main = paste("Tuolumne River at LaGrange /n comid = 2823750 -", col, " - ", typ, "s"), ylab = col)
        stripchart(jitter_data,
                   method = "jitter",
                   pch = 19,
                   bg = 10,
                   col = jit_col,
                   vertical = TRUE,
                   add = TRUE)
        legend("topright", legend = names_df, fill = colors, border = NA)
        dev.print(png, target = NULL) # Use dev.print() to save the plot
        graphics.off() # Close the current graphics device
      }
    }
  }
}
#By using png() followed by dev.print() and graphics.off(), we can save the plots without using dev.off().


compare_boxplots3 <- function(output_loc, ...) {
  # Retrieve the dataframes and their names
  dataframes <- list(...)
  names_arg <- as.list(match.call(expand.dots = FALSE)[-1])
  names_df <- unlist(names_arg)[-1]
  names_df <- unlist(names_df)
  num_dataframes <- length(dataframes)
  
  # Get common column names
  common_cols <- Reduce(intersect, lapply(dataframes, colnames))
  
  # Set the colors for each of the potential
  color_base <- c("blue", "chartreuse3", "red")
  jitter_color <- c("darkblue", "darkgreen", "darkred")
  
  # Generate colors for each dataframe
  colors <- color_base[1:num_dataframes]
  jit_col <- jitter_color[1:num_dataframes]
  Year_Typ <- c("All year", "wet year", "mod year", "dry year")
  for (typ in Year_Typ) {
    if (typ == "All year") {
      # Create box plots for common columns
      for (col in common_cols[-1]) {
        boxplot_data <- lapply(dataframes, function(df) df[[col]])
        df <- data.frame(value = unlist(boxplot_data), names_df = rep(names_df, each = length(boxplot_data)))
        
        cat("\n year type: ", typ, " metric: ", col)
        
        # Create the box plot with custom colors using ggplot2
        p <- ggplot(df, aes(x = names_df, y = value, fill = names_df)) +
          geom_boxplot() +
          geom_jitter(width = 0.2, alpha = 0.7, position = position_jitter(seed = 42)) +
          labs(title = paste("Tuolumne River at LaGrange\ncomid = 2823750 -", col, "-", typ, "s"),
               y = col) +
          theme_minimal() +
          theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
        
        # Save the plot using ggsave
        file_name <- paste(output_loc, "/box_plots/", typ, "s/", col, " Metric - ", typ, "s", ".png", sep = "")
        cat("\n", output_loc)
        ggsave(p, file = file_name, width = 8, height = 6, dpi = 300)
      }
    } else {
      filtered_dataframe <- dplyr::filter(dataframes, WY_cat == typ)
      
      # Create box plots for common columns
      for (col in common_cols[-1]) {
        boxplot_data <- lapply(filtered_dataframe, function(df) df[[col]])
        df <- data.frame(value = unlist(boxplot_data), names_df = rep(names_df, each = length(boxplot_data)))
        
        # Create the box plot with custom colors using ggplot2
        p <- ggplot(df, aes(x = names_df, y = value, fill = names_df)) +
          geom_boxplot() +
          geom_jitter(width = 0.2, alpha = 0.7, position = position_jitter(seed = 42)) +
          labs(title = paste("Tuolumne River at LaGrange\ncomid = 2823750 -", col, "-", typ, "s"),
               y = col) +
          theme_minimal() +
          theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
        
        # Save the plot using ggsave
        file_name <- paste(output_loc, "/box_plots/", typ, "s/", col, " Metric, ", typ, "s", ".png", sep = "")
        ggsave(p, file = file_name, width = 8, height = 6, dpi = 300)
      }
    }
  }
}









