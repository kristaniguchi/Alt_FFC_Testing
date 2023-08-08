#### This is a basic Script to to allow for testing of the CEFF altered calculator ...
#### While the actual package gets built

### Made by Cam Carpenter
## Last Updated on August 1st 2023

#### First install the necessary libraries 

library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);library(zoo)
library(ggplot2);library(dplyr);library(pracma); library(lubridate); library(dataRetrieval)

####Install the various functions required for the altered calculator, just run each of these to install them

# All the individual functions for the calculator -------------------------


#This function downloads from the USGS gages and manipulates the data to match user input
gage_flow <- function(gage_id) {
  
  
  #First get the data from, We want all the flow data available so we just read in the users gage data
  flow <- readNWISdv(
    gage_id,
    "00060",
    startDate = "",
    endDate = "",
    statCd = "00003"
  )
  
  #Apply proper names from the USGS site
  flow <- renameNWISColumns(flow)
  
  #Take just the data and flow data
  flow <- flow[,c("Date","Flow")]
  
  #Rename the data so it matches the user input files
  flow <- flow %>%
    rename("date" = "Date", "flow" = "Flow")
  #Return the flow data
  return(flow)
}

#Define a function that calculates the rate of change for a time series
rate_of_change <- function(flow) {
  #get the changes in flow 
  diffs <- diff(flow)
  #now divide the change by the flow
  roc <- diffs/flow[-length(flow)]
  #in the case where the flow is 0 then the roc value will be a NaN value and need to be replaced with 0
  roc[is.nan(roc)] <- 0 
  roc <- c(NA,roc)
  
}


#Define a function that finds the dry season timing given the flow after the top of the spring recession until the end of the water year
Altered_Summer_Dry_Season_Tim <- function(flow) {
  median <- median(flow)
  roc <- rate_of_change(flow)
  # Initialize variables
  n_consec <- 0
  n_neg <-0
  idx_consec <- NULL
  idx_start <- NULL
  
  # Loop through the time series
  for (i in seq_along(flow)) {
    #If the program has not found a qualifying timing in entire vector then assign a NA to the dry season timing
    #Since there isn't a rate of change value for index 1 set is at the first possible start and continue the loop
    if(i==1){
      idx_start <- i
      next
    }
    if (i == length(flow)){
      DS_Tim <- NA
    }
    # Check if this is the first value in a potential consecutive sequence
    if (is.null(idx_start)) {
      idx_start <- i
      
    }
    # Check if the current value is within 2% of the previous value
    if (abs(roc[i]) <= .02 | flow[i] < 2 & !is.null(idx_start)) {
      n_consec <- n_consec + 1
      if (roc[i]<0){
        n_neg <- n_neg+1
      }
      # Check if this is the fourth consecutive value that meets the criteria
      if (n_consec == 5) {
        #cat("should be done")
        idx_consec <- seq(idx_start, i)
        break
      }
    }
    # Reset the consecutive counter if the current value is not within 2% of the previous value
    else {
      n_consec <- 0
      n_neg <-0
      idx_start <- NULL
    }
    #cat("\n number in iteration: ", i,"\n consectutive: ", n_consec)
  }
  #This is just for testing on FNF date but is there is a year where there aren't 5 days of 2% change or less than try less than 5%
  if (is.null(idx_consec) == TRUE){
    idx_consec <- NULL
    idx_start <- NULL
    # Loop through the time series
    for (i in seq_along(flow)) {
      if(i==1){
        idx_start <- i
        next
      }
      # Check if this is the first value in a potential consecutive sequence
      if (is.null(idx_start)) {
        idx_start <- i
      }
      # Check if the current value is within 5% of the previous value
      if (abs(roc[i]) <= .05 | flow[i] < 2 & !is.null(idx_start)) {
        n_consec <- n_consec + 1
        if (roc[i]<0){
          n_neg <- n_neg+1
        }
        # Check if this is the fourth consecutive value that meets the criteria
        if (n_consec == 5) {
          idx_consec <- seq(idx_start, i)
          break
        }
      }
      # Reset the consecutive counter if the current value is not within 2% of the previous value
      else {
        n_consec <- 0
        n_neg <-0
        idx_start <- NULL
      }
      #cat("\n number in iteration: ", i,"\n consectutive: ", n_consec)
    }
  }
  
  if (n_neg > 3){
    DS_Tim <- (idx_consec[5]+1)
  }
  else if (n_neg <=3){
    DS_Tim <- (idx_consec[1]+1)
  }
  #cat(DS_Tim)
  return(DS_Tim)
}

#Define the function that calculates the spring metrics and Dry season start timing
Altered_Spring_Recession <- function(FlowYear) {
  
  #Setup the output vectors
  SP_Mag<- c()
  SP_Tim_test <- c()
  SP_Tim<- c()
  SP_ROC <- c()
  SP_ROC_Max <- c()
  SP_Dur <- c()
  DS_Tim <- c()
  
  #Determine how many water years there are 
  Water_Years <- unique(FlowYear$water_year)
  
  #Loop through all of the water years
  
  for (i in 1:length(Water_Years)) {
    cat("\n \n Water Year: ", Water_Years[i])
    #Filter the flow data to the individual water year
    flow <- filter(FlowYear, FlowYear$water_year== Water_Years[i])
    WY_median <- median(flow$flow)
    #Skip the year if there are more than 100 NA flow data points
    if ((sum(is.na(flow$flow)) +  sum(is.nan(flow$flow))) > 100 | length(flow$date) < 358) {
      #Assign Null value if there are more than 100 NA flow data points
      #cat("Broke too many NA values")
      SP_Tim[i] <- NA
      SP_Mag[i] <- NA
      SP_ROC[i] <- NA
      SP_Dur[i] <- NA
      SP_ROC_Max[i] <- NA
      DS_Tim[i] <- NA
      next
    } 
    #Check to see if there are more than the allowable number of 0s in the vector
    else if (sum(flow$flow[flow$flow==0 & !is.na(flow$flow)])>= 365){
      #cat("Too many 0's ")
      #Assign NA values to everything
      SP_Tim[i] <- NA
      SP_Mag[i] <- NA
      SP_ROC[i] <- NA
      SP_Dur[i] <- NA
      SP_ROC_Max[i] <- NA
      DS_Tim[i] <- NA
      #Then move to the next year
      next
      
    }
    else {
      #Now that the data has past the checks then we need to replace the NA data
      flow$flow <- replace_na(flow$flow)
      
      #calculate the 50th and 9th percentile for the flows
      quants <- quantile(flow$flow, probs = c(0.5, 0.9),na.rm = FALSE, names = FALSE)
      #Calculate the peaks that occur throughout the year
      peaks <- as.data.frame(findpeaks(flow$flow))
      
      #We also want to consider peaks that are flat the limit was set as 3 "flat" points for these
      peaks_2 <- as.data.frame(findpeaks(flow$flow, peakpat = "[+]{1,}[0]{1,3}[-]{1,}"))
      
      #combine the two data sets of peaks
      peaks_all <- bind_rows(peaks,peaks_2)
      
      #cat("broke after peaks")
      #Check to make sure there is data in the peaks 
      if (length(peaks_all)>1 ){
        #If there is data in the data frame then makes sure it is more than just the titles
        if (length(peaks_all[,1]) > 0){
          #Sort all the peaks by the date of the peak
          peaks_all <- peaks[order(peaks_all$V2,decreasing = FALSE),]
          
          #Filter out peaks that are not above the 90th percentile and peaks that are in september
          peaks_90 <- filter(peaks_all, peaks_all$V1 > quants[2])
          peaks_90 <- filter(peaks_90, peaks_90$V2 <330)
          
        }
        else {
          peaks_90 <- NULL
        }
        
      }
      else {
        peaks_90 <- NULL
      }
      
      
      #cat("broke after making peaks_90")
      #Check to make sure that there are qualified peaks 
      if (is.null(peaks_90) == FALSE & length(peaks_90[,1]) >0) {
        
        
        #Assign the last peak of the year as the first potential spring timing
        springindex_PH1 <- tail(peaks_90[,2],1)
        
        #Check to see if this peak is also the fall pulse
        if (springindex_PH1 <= 75 & length(peaks_90[,2]) <2){
          #If it is then it doesn't not count as the spring as well
          springindex_PH1 <- NULL
        }
      }
      
      #if there are no qualifying peaks set this placeholder to null value
      else {
        springindex_PH1 = NULL
      }
      #cat("broke after making PH1 NULL")
      #Find the index of flows at or above the 90th percentile
      highflows <- which(flow$flow >= quants[2])
      
      #Assign the last index above 90th percentile flow as the second potential spring recession index 
      springindex_PH2 <- max(highflows)
      #cat("broke after making PH2")
      #If the first placeholder is not valid then use the second place holder vlue
      if ( is.null(springindex_PH1) ) { #(springindex_PH1+30)<springindex_PH2 || ###OTHER OPTION
        #Set the index of the spring timing to the second place holder
        springindex <- springindex_PH2
      }
      else {
        #Otherwise set the spring index to the first placement
        springindex <- springindex_PH1
      }
      
      #Set the spring timing to index identified 
      SP_Tim[i] <- springindex
      #SP_Tim_test[i] <- append(as.Date(flow$date[springindex], "%m/%d/%Y"))
      
      #Set the spring magnitude to the flow on the index of the timing
      SP_Mag[i] <- as.numeric(flow$flow[springindex])
    }
    
    #make a new data frame with just the flows after the top of the spring recession
    flow_post_SP <- flow %>% slice(springindex:length(flow))
    
    #Calculate the rate of change for the rest of the year after the top of the spring recession
    roc <- rate_of_change(flow_post_SP$flow)
    cat("\n",SP_Tim[i],"\n" )
    #calculate the dry season start timing by subtracting the length of the water year by the time remaining 
    #after the spring recession peak and then add the timing of the start of the dry season after the spring peak
    PH_DS_Tim <- as.numeric(Altered_Summer_Dry_Season_Tim(flow_post_SP$flow)) #Original Code
    #PH_DS_Tim <- as.numeric(Altered_Summer_Dry_Season_Tim_Squ_Dif(flow_post_SP$flow)) #Square Dif Method
    #PH_DS_Tim <- as.numeric(Altered_Summer_Dry_Season_Tim_Merged(flow_post_SP$flow)) #combined 
    #H_DS_Tim <- as.numeric(Altered_Summer_Dry_Season_Tim_smoothed(flow_post_SP$flow)) #Smoothed Code
   
    #set threshold 
    #threshold <- min(flow$flow)
    #threshold <- quantile(flow$flow, .10)
    
    #Then new code door Dry season
    #PH_DS_Tim <- as.numeric(Altered_Summer_Dry_Season_Tim_Squ_Dif_threshold(flow_post_SP$flow,threshold))
    
    if (i == length(Water_Years) & length(as.numeric(Altered_Summer_Dry_Season_Tim(flow_post_SP$flow)))<1){
      PH_DS_Tim <- length(flow_post_SP$flow)
    }
    #It is possible that if all the flows are below 2 cfs then the dry season will be set to 0
    else if (PH_DS_Tim == 0){
      PH_DS_Tim <- 1
    }
    cat("\n Dry season place holder:", PH_DS_Tim)
    #DS_Tim[i] <- length(flow$flow)-length(flow_post_SP$flow)+PH_DS_Tim ORIGINAL CODE
    DS_Tim[i] <- PH_DS_Tim+SP_Tim[i]
    #The spring duration is the time between spring timing and dry season start timing
    SP_Dur[i] <- DS_Tim[i]-SP_Tim[i]
    
    cat("\n spring timing: ", SP_Tim[i],"  Dry season Timing: ", DS_Tim[i])
    
    #Make an array of the rate of change values after the spring peak until the
    #Start of the dry season. This needs to start at the second value since the
    #first value is the NA. This is because there is nothing to compare the first value to.
    SP_recs_temp <- roc[2:(SP_Dur[i]+1)]
    #cat("\n SP recs ", SP_recs_temp)
    #The spring rate of change metric is the median of the of all the negative roc values
    SP_ROC[i] <- abs(median(SP_recs_temp[SP_recs_temp<0]))
    
    #The spring maximum rate of change metric is the largest  negative roc value
    SP_ROC_Max[i] <- max(abs(SP_recs_temp[SP_recs_temp<0]))
  }
  
  #Put all the metrics into a list
  SP_Metrics_and_Dry_Season_Tim <- list( "SP_Tim"= SP_Tim,"SP_Mag"= SP_Mag, "SP_ROC"= SP_ROC, "SP_Dur"= SP_Dur, "SP_ROC_Max" = SP_ROC_Max, "DS_Tim"= DS_Tim)
  
  #Return all of the metrics
  return(SP_Metrics_and_Dry_Season_Tim)
}

#Define a function that finds the Fall Metrics and Wet Season timing
Altered_Fall_Wet_Timing <- function(FlowYear, DS_Tim) {
  #Set up Output Vectors
  FA_Tim <- c()
  Wet_Tim <- c()
  FA_Mag <- c()
  FA_Dur <- c()
  FA_Dif_ratio <- c()
  FA_Dif_num <- c()
  
  
  #Set up place holder vectors
  Temp_FA_Tim <- c()
  Temp_Wet_Tim <- c()
  Temp_DS_flow <- c()
  
  #Determine how many water years there are 
  Water_Years <- unique(FlowYear$water_year)
  
  for (i in 1:length(Water_Years)) {
    
    cat("\n New Water Year: ",Water_Years[i], "\n")
    
    
    #If this is the first year then skip since there is not a previous dry season to use
    if (i == 1){
      #If it is the first year then skip since there no former dry season baseflow to compare to so it skips
      FA_Tim[i] <- NA
      Wet_Tim[i] <- NA
      FA_Mag[i] <- NA
      FA_Dif_ratio[i] <- NA
      FA_Dif_num[i] <- NA
      #Temp_DS_flow_1 <- flow$flow[DS_Tim[i]:length(flow$flow)]
      next
    } 
    #Make a data frame of the flow year to check to see if it qualifies to run
    check_flow <- filter(FlowYear, water_year== Water_Years[i])
    
    #check to see if there are too many "0" values or NA/NaN values in the water year
    if (sum(is.na(check_flow$flow) | is.nan(check_flow$flow))>= 100 || sum(check_flow$flow[check_flow$flow==0 & !is.na(check_flow$flow)])>= 365 | length(check_flow$date) < 358) {
      
      #If it does then set all the metrics to NA
      FA_Tim[i] <- NA
      Wet_Tim[i] <- NA
      FA_Mag[i] <- NA
      FA_Dur[i] <- NA
      FA_Dif_ratio[i] <- NA
      FA_Dif_num[i] <- NA
      next
    }
    #If there is enough data then calculate the fall pulse and wet season timing
    
    
    #Filters flow to the water year of interest and the previous water year
    flow <- filter(FlowYear, water_year== Water_Years[i] | water_year== Water_Years[i-1])
    
    #first we need to find the point where our water year of interest starts
    #To do this we will convert the date field of the filtered data into an actual date
    date_data <- as.Date(flow$date, tryFormats = c("%m/%d/%Y"))
    
    #Then we need to find the October 1st start date of the water year of interest
    WY_start <- which(month(date_data ) == 10 & day(date_data) == 1 & year(date_data) == (Water_Years[i]-1))
    
    cat("\n Length of all flow: ", length(flow$flow), "\n Date of 365 timestep:", flow$date[WY_start] )
    #sets up an initial dry season baseflow estimates
    #Temp_DS_Mag[i] <- median(flow$flow[DS_Tim[i]:length(flow$flow)])
    #Calculates the current water years median 
    WY_median <- median(flow$flow)
    FA_Check <- FALSE #Set the check whether the fall exists to false at the start of each loop
    

      #Now that the data has past the checks then we need to replace the NA data
      flow$flow <- replace_na(flow$flow)
      
      median_flow <- median(flow$flow)
      
      #Start by finding peaks that occur from October 1st to December 15th
      FA_peaks <- findpeaks(flow$flow[WY_start:(WY_start+75)], minpeakheight = (0.5*median_flow)) 
      
      #check to see if there aren't any peaks then see if there were plug flows
      if (length(FA_peaks)<0){
        
        #looking for plug flows, the longest plateau allowed is 7 days 
        FA_peaks <- findpeaks(flow$flow[WY_start:(WY_start+75)], peakpat = "[+]{1,}[0]{1,6}[-]{1,}")
        
      }
      
      if (Water_Years[i] == 2007){
        check <- FA_peaks
        check_3 <- flow$flow[WY_start:(WY_start+75)]
      }
      cat("\n WY start +75 = ", flow$date[(WY_start+75)],"\n")
      
      #Check to make sure the is data in the output from the peaks analysis
      if (length(FA_peaks)>0){
        
        #Loop through the peaks to see if there is a qualifying peak
        for(j in 1:length(FA_peaks[,1])) {
          
          #See if the last peak met the criteria if so break the loop
          if (FA_Check == TRUE) {
            break
          } 
          
          ##Check to see if the peaks meet 1.5 times the baseline threshold 
          ##First make a vector of the flow values from the previous dry season until the potential peak
          
          #To do this we need to make sure there was a dry season timing next year
          if(is.na(DS_Tim[i-1]) != TRUE) {
            #Then 
            
            Temp_DS_flow <- flow$flow[DS_Tim[i-1]:(FA_peaks[j,2]+WY_start)]
            Temp_DS_Mag <- median(Temp_DS_flow)
          }
          else if (is.na(DS_Tim[i-1]) == TRUE){
            
            Temp_DS_flow <- flow$flow[1:(FA_peaks[j,2]+WY_start)]
            Temp_DS_Mag <- median(Temp_DS_flow)
            
          }
          
          cat("\n Temp_DS: ", Temp_DS_Mag,"\n")
          #Check to see if the peak is larger than the estimated dry season baseflow
          if (FA_peaks[j,1] > 1.5*Temp_DS_Mag & FA_peaks[j,1] >= 1) {
            
            #Store the identified timing for the fall pulse
            FA_Tim_Temp <- FA_peaks[j,2]
            FA_Mag_Temp <- FA_peaks[j,1]
            
            #Look for the start of the wet season to check the if the fall peak actually qualifies
            WS_peaks <- findpeaks(flow$flow[(FA_Tim_Temp+WY_start):length(flow$flow)])
            
            #Go through each of the identified peaks
            for(k in 1:length(WS_peaks[,1])) {
              
              #Find the median flow between the potential fall pulse and the wet season start
              FA_to_Wet_med <- median(flow$flow[(WY_start+FA_Tim_Temp+1):(WY_start+FA_Tim_Temp+WS_peaks[k,2])])
              
              
              
              #Check if the peak is larger than 1.5 times the base flow
              if (WS_peaks[k,1] > 1.5*FA_to_Wet_med) {
                Wet_Tim_Temp <- WS_peaks[k,3]
                
                #To get the dry season median we need to make sure there was a dry season timing next year
                if(is.na(DS_Tim[i-1]) != TRUE) {
                  
                  #Calculate the potential dry season 50th percentile flow
                  Temp_DS_Mag <- median(flow$flow[DS_Tim[i-1]:(FA_Tim_Temp+Wet_Tim_Temp+WY_start)])
                }
                else if (is.na(DS_Tim[i-1]) == TRUE){
                  #Calculate the potential dry season 50th percentile flow
                  Temp_DS_Mag <- median(flow$flow[1:(FA_Tim_Temp+Wet_Tim_Temp+WY_start)])
                }
                cat("\n Wet Peak Mag: " , WS_peaks[k,1], " Potential Fall Pulse: ", FA_Mag_Temp," thershold : ", (1.5*Temp_DS_Mag),"\n")
                #Check to see if the fall pulse is still 1.5 time the dry season 50th percentile flow 
                if (FA_Mag_Temp > 1.5*Temp_DS_Mag) {
                  
                  FA_Tim[i] <- FA_peaks[j,2]
                  #Assign the peak value as the magnitude
                  FA_Mag[i] <- FA_peaks[j,1]
                  #Calculate the duration of the flush from the start of the rising limb to the peak
                  FA_Dur[i] <- FA_peaks[j,4] - FA_peaks[j,3]
                  #Calculate the difference metric 
                  FA_Dif_ratio[i] <- FA_peaks[j,1]/Temp_DS_Mag
                  FA_Dif_num[i] <- FA_peaks[j,1]-Temp_DS_Mag
                  #Set the wet season start timing 
                  Wet_Tim[i] <- FA_Tim[i]+Wet_Tim_Temp #Need to subtract one from the temporary time since the fall timing counted in both the fall timing and the Wet season timing
                  
                  cat("\n Fall pluse timing: " ,FA_Tim[i],"\n Wet timing with fall pulse: ",Wet_Tim[i],"\n" )
                  
                  cat("\n Fall pluse timing: " ,flow$date[WY_start+ FA_Tim[i]], "\n")
                  
                  FA_Check <- TRUE
                  break
                }
                
              }
              
              
            }
            #If none of the post fall pulse peaks meet the criteria it is likely a peak prior to a hat senario and we want to find the last day before the 90th percentile flow
            post_fall_flow <- flow[(FA_Tim_Temp+WY_start):length(flow$flow),]
            threshold_90 <- quantile(post_fall_flow$flow,0.9)
            if(all(WS_peaks[,1]<threshold_90)){
              #Find subset of data above the 90th percentile
              post_fall_90th <- subset(post_fall_flow, flow >= threshold_90)
              
              #find the first day that is at or above the 90th percentile of flow
              #index_90 <- which.min(post_fall_90th$flow)
              
              #start_date <- post_fall_90th$date[index_90]
              start_date <- post_fall_90th$date[1]
              
              #then go one day back to capture the rising limb
              Wet_start_index <- which(post_fall_flow$date == start_date)
              
              #To get the dry season median we need to make sure there was a dry season timing next year
              if(is.na(DS_Tim[i-1]) != TRUE) {
                
                #Calculate the potential dry season 50th percentile flow
                Temp_DS_Mag <- median(flow$flow[DS_Tim[i-1]:(FA_Tim_Temp+Wet_Tim_Temp+365)])
              }
              else if (is.na(DS_Tim[i-1]) == TRUE){
                #Calculate the potential dry season 50th percentile flow
                Temp_DS_Mag <- median(flow$flow[1:(FA_Tim_Temp+Wet_Tim_Temp+365)])
              }
              
              
              #Check to see if the fall pulse is still 1.5 time the dry season 50th percentile flow 
              if (FA_Mag_Temp > 1.5*Temp_DS_Mag) {
                
                FA_Tim[i] <- FA_peaks[j,2]
                #Assign the peak value as the magnitude
                FA_Mag[i] <- FA_peaks[j,1]
                #Calculate the duration of the flush from the start of the rising limb to the peak
                FA_Dur[i] <- FA_peaks[j,4] - FA_peaks[j,3]
                #Calculate the difference metric 
                FA_Dif_ratio[i] <- FA_peaks[j,1]/Temp_DS_Mag
                FA_Dif_num[i] <- FA_peaks[j,1]-Temp_DS_Mag
                #Set the wet timing based on the identified 
                Wet_Tim[i] <- Wet_start_index+FA_Tim_Temp
                cat("\n Wet Season Date",flow$date[(365+Wet_Tim[i])],"\n")
                
                FA_Check <- TRUE
              }
            }
            else {
              
              next
            }
            
          }
        }
        
        
        
        if (FA_Check == FALSE) { 
          
          #If there was not then set all Fall metrics besides the Difference metric to NA
          FA_Tim[i] <- NA
          FA_Mag[i] <- NA
          FA_Dur[i] <- NA
          FA_Dif_ratio[i] <- max(FA_peaks[,1])/Temp_DS_Mag
          FA_Dif_num[i] <- max(FA_peaks[,1])-Temp_DS_Mag
        }
        
        
        
      }
      
      
      #If there are no pulses detected in the required time period then apply NA values to all Fall metrics
      else {
        FA_Tim[i] <- NA
        FA_Mag[i] <- NA
        FA_Dur[i] <- NA
        FA_Dif_ratio[i] <- NA
        FA_Dif_num[i] <- NA
      }
      
      
      #Now the code will look at if no fall pulses occurred
      if (FA_Check == FALSE) { 
        
        cat("\n just wet ")
        
        #If there was no fall pulse then find the first pulse after that is 1.5 Dry season baseflow after the fall pulse period
        #first get the flow data from after the fall pulse window
        Wet_Peaks_flow <- flow[(WY_start+75):nrow(flow),]
        
        #Now find the peaks after in the post fall window 
        WS_peaks <- findpeaks(Wet_Peaks_flow$flow)
        cat(WS_peaks)
        #Check to make sure the is data in the output from the peaks analysis
        if (length(WS_peaks)>0){
          
          #Loop through the peaks to see if there is a qualifying peak
          for(j in 1:length(WS_peaks[,1])) {
            #Check to see if the peaks meet 1.5 times the baseline threshold 
            #First make a vector of the flow values from the previous dry season until the potential peak
            
            #To do this we need to make sure that there was a previous dry season timing
            #To get the dry season median we need to make sure there was a dry season timing next year
            if(is.na(DS_Tim[i-1]) != TRUE) {
              
              #Calculate the potential dry season 50th percentile flow
              Temp_DS_flow <- flow$flow[DS_Tim[i-1]:(WS_peaks[j,2]+WY_start)]
              Temp_DS_Mag <- median(Temp_DS_flow)
            }
            else if (is.na(DS_Tim[i-1]) == TRUE){
              #Calculate the potential dry season 50th percentile flow
              Temp_DS_flow <- flow$flow[1:(WS_peaks[j,2]+WY_start)]
              Temp_DS_Mag <- median(Temp_DS_flow)
            }
            
            
            #Check to see if the peak is larger than the estimated dry season baseflow
            if (WS_peaks[j,1] > 1.5*Temp_DS_Mag) {
              
              #Now we know we either have a peak on or peak before a "hat" scenario,
              # so we need to check the timing of the peak
              
              #To do that we are first going to 90th percentile flow for the current flow year
              current_flowyear <- filter(flow, water_year == Water_Years[i])
              
              
              threshold_90 <- quantile(current_flowyear$flow,0.9)
              
              cat("\n peak: ",WS_peaks[j,1], "\n", threshold_90)
              
              
              #Find subset of data above the 90th percentile or above 1 cfs if the 90th percentile is less than 
              flow_90th <- subset(current_flowyear, flow >= max(threshold_90,1))
              
              #If all of the flows are less than 1 cfs then just use the 90th percentile flow
              if(length(flow_90th[,1])<1){
                flow_90th <- subset(current_flowyear, flow >= threshold_90)
              }
              
              #find the first day that is at or above the 90th percentile of flow
              #index_90 <- which.min(flow_90th$flow)
              cat("hi")
              #start_date <- Wet_Peaks_flow$date[index_90]
              start_date <- flow_90th$date[1]
              #Now get the index of first 90th percentile flow start date in the wet peaks flow
              Wet_start_index_1 <- which(Wet_Peaks_flow$date == start_date)
              
              #Then get the index of the start of the qualified peak 
              Wet_start_index_2 <- WS_peaks[j,3]
              
              cat("\n index 1: ",Wet_start_index_1,"  index 2: ", Wet_start_index_2)
              
              #Check to see which potential timing 
              if (Wet_start_index_1 < Wet_start_index_2){
                #if the first index is smaller then set that as the wet season 
                Wet_Tim[i] <- Wet_start_index_1 + 75 
              }
              else if (Wet_start_index_1 >= Wet_start_index_2) {
                
                #Set the wet season start timing 
                Wet_Tim[i] <- 75+Wet_start_index_2
                
              }
              cat("\n Wet Season Date",flow$date[(365+Wet_Tim[i])],"\n")
              
              break
              
            }
          
          }
          #IF all peaks are below the 90th flow percentile then we will just choose the day before the 1st day of 90th flow
          #If none of the post fall pulse peaks meet the criteria it is likely a peak prior to a hat scenario
          #and we want to find the last day before the 90th percentile flow of that flow year
          Temp_dry_flow <- filter(FlowYear, water_year == Water_Years[i])
          threshold_90 <- quantile(Temp_dry_flow$flow,0.9)
          if(all(WS_peaks[,1]<threshold_90) | length(WS_peaks)<=0){
            
            
            #Find subset of data above the 90th percentile
            flow_90th <- subset(Temp_dry_flow, flow >= threshold_90)
            
            #find the first day that is at or above the 90th percentile of flow
            #index_90 <- which.min(flow_90th$flow)
            #start_date <- flow_90th$date[index_90]
            start_date <- flow_90th$date[1]
            
            #then go one day back to capture the rising limb
            Wet_start_index <- (which(Temp_dry_flow$date == start_date)-1)
            
            #Set the wet season timing to that date
            Wet_Tim[i] <- Wet_start_index
          }
          
        }
        else{  
          #If there aren't any peaks then it is likely hat scenario
          #and we want to find the last day before the 90th percentile flow of that flow year
          Temp_dry_flow <- filter(FlowYear, water_year == Water_Years[i])
          threshold_90 <- quantile(Temp_dry_flow$flow,0.9)
          if(all(WS_peaks[,1]<threshold_90) | length(WS_peaks)<=0){
            
            
            #Find subset of data above the 90th percentile
            flow_90th <- subset(Temp_dry_flow, flow >= threshold_90)
            
            #find the first day that is at or above the 90th percentile of flow
            #index_90 <- which.min(flow_90th$flow)
            #start_date <- flow_90th$date[index_90]
            start_date <- flow_90th$date[1]
            
            #then go one day back to capture the rising limb
            Wet_start_index <- (which(Temp_dry_flow$date == start_date)-1)
            
            #Set the wet season timing to that date
            Wet_Tim[i] <- Wet_start_index
          }
          
        }
        
      }
      
  }
  
  #Put Everything into a list for the ourput
  Fall_Metrics_and_Wet_tim <- list("FA_Tim"=FA_Tim,"FA_Mag"=FA_Mag, "FA_Dur"=FA_Dur,"FA_Dif_ratio"=FA_Dif_ratio,"FA_Dif_num"=FA_Dif_num,"Wet_Tim"=Wet_Tim)
  
  #Return the calculated metrics
  return(Fall_Metrics_and_Wet_tim)
}

#This function calculates the Dry and Wet Seasons magnitudes
#The inputs for this function are the timings of the spring recession,
#the dry season start timing, and the start of the wet season timing
Wet_Dry_Season_Non_Tim_Metrics <- function(FlowYear,SP_Tim,DS_Tim, Wet_Tim){
  
  #Get all the of the water years in question
  WYs <- unique(FlowYear$water_year)
  
  #Set up the result matrices 
  Wet_BFL_Mag_10 <- c()
  Wet_BFL_Mag_50 <- c()
  DS_Mag_50 <- c()
  DS_Mag_90 <- c()
  DS_Dur_WS <- c()
  Wet_BFL_Dur <- c()
  
  #cycle through the water years to calculate the metrics
  for (i in 1:length(WYs)) {
    cat("\n Water Year", WYs[i],"\n")
    
    #filter the flow to the water year in question
    Filteryear1 <- filter(FlowYear, water_year == WYs[i])
    
    #Check to see if there were too many 0's or NA values in this water year
    
    #check to see if there are too many "0" values or NA/NaN values in the water year
    if (sum(is.na(Filteryear1$flow) | is.nan(Filteryear1$flow))>= 100 || sum(Filteryear1$flow[Filteryear1$flow==0 & !is.na(Filteryear1$flow)])>= 365) {
      
      #If it does then set all the metrics to NA
      Wet_BFL_Mag_10[i] <- NA
      Wet_BFL_Mag_50[i] <- NA
      Wet_BFL_Dur[i] <- NA
      DS_Mag_50[i] <- NA
      DS_Mag_90[i] <- NA
      DS_Dur_WS[i] <- NA
      
      next
    }
    
    
    #Replace the NA in the data
    Filteryear1$flow <- replace_na(Filteryear1$flow)
    
    if(is.na(Wet_Tim[i]) & i == length(WYs)){
      #If it is the last year and there isn't wet season timing data then make the magnitude and duration NA
      Wet_BFL_Mag_10[i] <- NA
      Wet_BFL_Mag_50[i] <- NA
      Wet_BFL_Dur[i] <- NA
    }
    
    else if(is.na(Wet_Tim[i]) | is.nan(Wet_Tim[i] & is.na(Wet_Tim[i+1]) )) {
      next
    }
    else if( !is.na(Wet_Tim[i]) ){
      WS <- Filteryear1$flow[Wet_Tim[i]:SP_Tim[i]]
      WS_mag <- quantile(WS,c(.1,0.5))
      Wet_BFL_Mag_10[i] <- WS_mag[1]
      Wet_BFL_Mag_50[i] <- WS_mag[2]
      
      Wet_BFL_Dur[i] <- DS_Tim[i]-Wet_Tim[i]
      
      cat("\n wet season",WS_mag)
    }
    if(is.nan(Wet_Tim[i+1])|is.na(Wet_Tim[i+1]) & i != length(WYs)){
      #Set the 50th and 90th percentile flows for the dry season and assign it to the magnitude
      DS_mag <- quantile(DS,c(.5,0.9))
      DS_Mag_50[i] <- DS_mag[1]
      DS_Mag_90[i] <- DS_mag[2]
      
      #Since there is not the next year 
      DS_Dur_WS[i] <- NA
      next
    }
    else if(is.nan(Wet_Tim[i+1])|is.na(Wet_Tim[i+1]) & i == length(WYs)){
      
      #check to see if the dry season timing exists
      #if it does assign then calculate the dry season flow
      if(is.na(DS_Tim[i]) != TRUE & is.nan(DS_Tim[i]) != TRUE){
        DS <- Filteryear1$flow[DS_Tim[i]:length(Filteryear1$flow)]
      }
      #Other wise make it 4 days after the spring similar to the original calculator
      else {
        DS <- Filteryear1$flow[(SP_Tim[i]+4):length(Filteryear1$flow)]
      }
      
      #Get the 50th and 90th percentile flows for the dry season and assign it to the magnitude
      DS_mag <- quantile(DS,c(.5,0.9))
      DS_Mag_50[i] <- DS_mag[1]
      DS_Mag_90[i] <- DS_mag[2]
      
      #Since there is not the next year 
      DS_Dur_WS[i] <- NA
      
      cat("\n dry season",DS_mag)
      next
    }
    
    Filteryear2 <- filter(FlowYear, water_year == WYs[i] | water_year == WYs[i]+1)
    
    DS <- Filteryear2$flow[DS_Tim[i]:(Wet_Tim[i+1]+365)]
    
    
    #Replace the NA in the data
    DS <- replace_na(DS)
    
    #return(Filteryear2,DS)
    DS_mag <- quantile(DS,c(.5,0.9))
    DS_Mag_50[i] <- DS_mag[1]
    DS_Mag_90[i] <- DS_mag[2]
    
    DS_Dur_WS[i] <- (Wet_Tim[i+1]+365)-DS_Tim[i]
    
    cat("\n dry season",DS_mag, "\n", DS_Dur_WS[i])
    
  }
  #Put all the metrics in a list
  Output_Metrics <- list("Wet_BFL_Mag_10"=Wet_BFL_Mag_10,"Wet_BFL_Mag_50"=Wet_BFL_Mag_50,"Wet_BFL_Dur"=Wet_BFL_Dur,"DS_Mag_50"=DS_Mag_50,"DS_Mag_90"=DS_Mag_90,"DS_Dur_WS"=DS_Dur_WS) 
  
  
  return(Output_Metrics)
}

#This function calculates the the mean annual flow and their water year category 
Annual_Metrics <- function(FlowYear) {
  #get the number of water years and which years they are
  WYs <- unique(FlowYear$water_year)
  
  #Set up the output vectors
  Mean_Ann_Flow <-c()
  WY_Rank_WtoD <- c()
  WY_Cat <- c()
  count_0 <- c()
  
  
  #Cycle through all the water years
  for (i in 1:length(WYs)) {
    #Filter the flow to the water year in quation
    flow <- filter(FlowYear, water_year == WYs[i])
    
    #Then we need to find the October 1st start date of the water year of interest
    #WY_start <- which(month(date_data ) == 10 & day(date_data) == 1 & year(date_data) == (Water_Years[i]-1))
    
    
    if(sum(is.na(flow$flow) | is.nan(flow$flow[1:length(flow$flow)]))>= 100 || sum(flow$flow[flow$flow==0 & !is.na(flow$flow)])>= 365 | length(flow$date) < 358){
      Mean_Ann_Flow[i] <- NA
      
    }
    
    #Now that the data has past the checks then we need to replace the NA data
    flow$flow <- replace_na(flow$flow)
    
    
    #Calculate the mean annual flow for that water year and put it in the output vector
    Mean_Ann_Flow[i] <- mean(flow$flow)
    
    
    
    #Assign NA values to everything
    count_0[i] <- sum(flow$flow==0)
    
  }
  
  #Rank the water year mean annual flows from low to high
  WY_Rank_WtoD <- rank(-Mean_Ann_Flow)
  
  #calculate the total number of water years we have to rank
  num_years <- length(WYs)
  
  #the top tertile of years are wet years
  cutoff_1 <- round(num_years * (1/3))  # Upper cutoff for wet years
  
  #the middel tertile of years are wet years
  cutoff_2 <- round(num_years * (2/3))  # Upper cutoff for mod years
  
  #Set the water year catagory based on the tertiles above
  WY_Cat <- cut(WY_Rank_WtoD, breaks = c(0, cutoff_1, cutoff_2, Inf), labels = c("wet year", "mod year", "dry year"))
  
  Ann_metric_out <- list("Years" = WYs,"Mean_Ann_Flow"=Mean_Ann_Flow,"WY_Cat"=WY_Cat)
  
  #Return the mean annual flow and water year catagory
  return(Ann_metric_out)
}

#code updated from original calculator to remove NA values from flow time series
replace_na <- function(flow_data) {
  for (index in seq_along(flow_data)) {
    if (index == 1 & is.na(flow_data[index])) {
      flow_data[index] <- 0
    }
    else if(is.na(flow_data[index])){
      flow_data[index] <- flow_data[index-1]
    }
  }
  return(flow_data)
}

#Makes percentiles dataframe from outputs
get_percentiles <- function(results_df, comid, percentiles, quantile_type){
  if(missing(percentiles)){
    percentiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  }
  
  if(missing(quantile_type)){
    quantile_type = 7
  }
  
  #set formatting for output
  options(scipen = 100, digits = 1)
  
  metrics_list <- list()
  for (metric in colnames(results_df)){
    if (metric == "Year"){
      next
    }
    metrics_list[[metric]] = quantile(results_df[metric], probs=percentiles, na.rm=TRUE, names=TRUE, type=quantile_type)
  }
  output_data <- t(data.frame(metrics_list))
  colnames(output_data) <- paste("p", percentiles * 100, sep="")
  output_data <- as.data.frame(output_data)
  output_data["metric"] <- rownames(output_data)
  output_data["comid"] <- comid  # attach the comid column
  output_data["result_type"] <- "observed"
  return(output_data)
  
}


# Getting the required background information -----------------------------

#First get where the data should be stored after the calculators runs
#This will need to be updated, if you have issues with plotting try using an abosulte path
output_loc <- "~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Tuolumne River/Updated Results"

####Enter Gage and Comid information
#Information needed to run function (replace the info below with your gage of interest and its associated COMID)
gage_1<-11272500 # change as needed, enter gage here
gage_2<-11289650

#Some usefull gages are the following:
#USGS 11288000 TUOLUMNE R AB LA GRANGE DAM NR LA GRANGE CA
#USGS 11299600 BLACK C NR COPPEROPOLIS CA
#USGS 11292500 CLARK FORK STANISLAUS R NR DARDANELLE CA
#USGS 11302000 STANISLAUS R BL GOODWIN DAM NR KNIGHTS FERRY CA
#USGS 11272500 MERCED R NR STEVINSON CA

#ex: tuolumne at La Grange usgs gage 11289650

#Enter COMID's for the river sections of interest
comid_1<- 8063597 #From determine this manually using the Natural Flows Database
comid_2<-2823750 #
#ex: tuolumne at la grange, comid=2823750
#comid = 21607271 EXC, comid = 348545 STANISLAUS RIVER (SNS)348545
#comid = 2833552 MERCED R NR STEVINSON CA


#Paste token here currently Cam's
Token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJDYW1lcm9uIiwibGFzdE5hbWUiOiJDYXJwZW50ZXIiLCJlbWFpbCI6ImNhbWNhcnBlbnRlckB1Y2RhdmlzLmVkdSIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNjY5NzQxMjg2fQ.WoTh0hQX7oluRxjoTg3A0N5PJD6HnMCQs10CsgZqOTo"


### Now we can obtain flow data either from a CSV or directly from the USGS NWIS stie

#For reading in a csv, depending on the csv the second line of this code may need to up updated for the date format
flow <- read.csv("~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Tuolumne River/Flow Data/clean_fnf_daily_dwr_TLG_cdec.csv", header = T) %>% #Enter first flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))

#For getting data from USGS site
flow <- gage_flow(gage_1)



#### Now to run the altered calculator 

# Altered Calculator ------------------------------------------------------


#Format the data for the calculator 
FlowYear_1 <- attach_water_year_data(flow, date_field = "date") %>% 
  mutate(date = format(date, "%m/%d/%Y")) #reformat date 


#First run the spring and timing function
SP_met_and_DS_Tim <- Altered_Spring_Recession(FlowYear_1)

#Now Calculate the Fall and Wet season timing
FA_Mets_and_Wet_Tim <- Altered_Fall_Wet_Timing(FlowYear_1,SP_met_and_DS_Tim$DS_Tim)

#Now we will get the magnitudes for the seasons
Wet_DS_Mags_and_Dur <- Wet_Dry_Season_Non_Tim_Metrics(FlowYear_1,SP_met_and_DS_Tim$SP_Tim,SP_met_and_DS_Tim$DS_Tim, FA_Mets_and_Wet_Tim$Wet_Tim)

#Finally get the Annual metrics 
Ann_Metrics <- Annual_Metrics(FlowYear_1)

#Make a data frame of the results
Year <- unique(FlowYear_1$water_year)

#you can change the name of the data frame to make more easily identifiable 
MFY_check<- data.frame(Year,Wet_DS_Mags_and_Dur$DS_Dur_WS,SP_met_and_DS_Tim$DS_Tim,Wet_DS_Mags_and_Dur$DS_Mag_50,Wet_DS_Mags_and_Dur$DS_Mag_90,FA_Mets_and_Wet_Tim$FA_Dur,FA_Mets_and_Wet_Tim$FA_Mag,FA_Mets_and_Wet_Tim$FA_Tim,FA_Mets_and_Wet_Tim$FA_Dif_num,FA_Mets_and_Wet_Tim$FA_Dif_ratio,SP_met_and_DS_Tim$SP_ROC,SP_met_and_DS_Tim$SP_ROC_Max,SP_met_and_DS_Tim$SP_Dur,SP_met_and_DS_Tim$SP_Mag,SP_met_and_DS_Tim$SP_Tim,Wet_DS_Mags_and_Dur$Wet_BFL_Dur,Wet_DS_Mags_and_Dur$Wet_BFL_Mag_10,Wet_DS_Mags_and_Dur$Wet_BFL_Mag_50,FA_Mets_and_Wet_Tim$Wet_Tim,Ann_Metrics$Mean_Ann_Flow,Ann_Metrics$WY_Cat)

#Since the data frame will take the names of the list we needto rename the columns to match the original Calculator
MFY_check <- MFY_check %>%
  rename("DS_Dur_WS" = "Wet_DS_Mags_and_Dur.DS_Dur_WS",
         "DS_Tim" = "SP_met_and_DS_Tim.DS_Tim",
         "DS_Mag_50" = "Wet_DS_Mags_and_Dur.DS_Mag_50",
         "DS_Mag_90" ="Wet_DS_Mags_and_Dur.DS_Mag_90",
         "FA_Dur" = "FA_Mets_and_Wet_Tim.FA_Dur",
         "FA_Mag" = "FA_Mets_and_Wet_Tim.FA_Mag",
         "FA_Tim" = "FA_Mets_and_Wet_Tim.FA_Tim",
         "FA_Dif_num" = "FA_Mets_and_Wet_Tim.FA_Dif_num",
         "FA_Dif_ratio" = "FA_Mets_and_Wet_Tim.FA_Dif_ratio",
         "SP_ROC" = "SP_met_and_DS_Tim.SP_ROC",
         "SP_ROC_Max" = "SP_met_and_DS_Tim.SP_ROC_Max",
         "SP_Dur" = "SP_met_and_DS_Tim.SP_Dur",
         "SP_Mag" = "SP_met_and_DS_Tim.SP_Mag",
         "SP_Tim" ="SP_met_and_DS_Tim.SP_Tim",
         "Wet_BFL_Dur" = "Wet_DS_Mags_and_Dur.Wet_BFL_Dur",
         "Wet_BFL_Mag_10" = "Wet_DS_Mags_and_Dur.Wet_BFL_Mag_10",
         "Wet_BFL_Mag_50" = "Wet_DS_Mags_and_Dur.Wet_BFL_Mag_50",
         "Wet_Tim" = "FA_Mets_and_Wet_Tim.Wet_Tim",
         "Mean_Ann_Flow" ="Ann_Metrics.Mean_Ann_Flow",
         "WY_Cat" = "Ann_Metrics.WY_Cat")



# Producing Percentiles and printing to CSV -------------------------------


#Calculated the percentile of the metrics Need to update the names of the dataframes for your site
MERCED_NR_STEVINSON_Summer_10th_percentile <- New_Summer_Obs_MFY_10th_percentile

#write metrics to csv 
#The output location will need to be updated
write_csv(Stan_Below_Goodwin_Observed,file = "~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Stanislaus River/USGS_11302000 STANISLAUS R BL GOODWIN DAM Metric.csv")

#Calculate the percentiles of outputs
metrics_percentiles <- get_percentiles(Stan_Below_Goodwin_Observed[,2:20],comid_1)

#write the percentiles to csv 
#The output location will need to be updated
write_csv(metrics_percentiles,file = "~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Stanislaus River/USGS_11302000 STANISLAUS R BL GOODWIN DAM Metric percentiles.csv")





# Existing Calculator -----------------------------------------------------


#run the FFC with gage data
FFC_out_1 <- ffcAPIClient::evaluate_gage_alteration(gage_id = gage_1, token = Token, 
                                                    comid = comid_1, plot_results = FALSE)
#Or with time series data
FlowYear_1 <- attach_water_year_data(flow, date_field = "date") %>% 
  mutate(date = format(date, "%m/%d/%Y")) #reformat date 

#Now the calcalculator can be used with either data
FFC_out_MERCED_compliance <- evaluate_alteration(FlowYear_1, token = Token, 
                                 comid = comid_1, plot_results = FALSE)      

## After one of these are ran we can get the metrics in an readable format
FF_Observe_Merced_compliance <- FFC_out_1[["ffc_results"]] %>% 
  mutate(Year = as.integer(Year)) 

#The Original Calculator does not add the water year catagories so we will need to run one of the new calculator functions
Ann_Metrics <- Annual_Metrics(FlowYear_1)

#Add these results to metrics
FF_Observe_1$Mean_Ann_Flow <- Ann_Metrics$Mean_Ann_Flow
FF_Observe_1$WY_Cat <- Ann_Metrics$WY_Cat

#write metrics to csv 
#The output location will need to be updated
write_csv(FF_Observe_1,file = "/Users/cameroncarpenter/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Tuolumne River/Updated Results/Observed Alt Calculator/CDEC_LGN_Observed_Metrics.csv")

#Calculate the percentiles of outputs
metrics_percentiles <- get_percentiles(FF_Observe_1[,2:20],comid_1)

#write the percentiles to csv 
#The output location will need to be updated
write_csv(metrics_percentiles,file = "/Users/cameroncarpenter/Box Sync/UCD-B_Fish-Baseflows/Flow Data/San Joaquin tribs/Tuolumne River/Updated Results/Observed Alt Calculator/CDEC_LGN_Observed_Metric_percentiles.csv")

