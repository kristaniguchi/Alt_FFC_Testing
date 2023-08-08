# Plot results from the function flow calculator and altered calcuator in box plots
# Cameron Carpenter 
# 08/7/2023

#Install the required packages 
library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);
library(ggplot2);library(dplyr)



###Making Box Charts for all the metrics in the FF_observed databases####

#Make sure that you have already calculated the metrics using the other scrip

#Now name your metrics for the plotting
name_1 <- "Observed Flow Data" 
name_2 <- "FERC Flow Data"
name_3 <- "FNF Data"

#Either add your metric to there place holders or edit the data frames called out below
Metric_dataframe_1 <- FF_Observe_1 # Update as needed
Metric_dataframe_2 <- FF_Observe_2 # Update as needed
Metric_dataframe_3 <- FF_Observe_3 # Update as needed

#first get all the unique column names 
#Now we will check to make sure all of the set of metrics have all of the metrics otherwise it will break 
combined_df <- rbind(FF_Observe_1,FF_Observe_2,FF_Observe_3)
cnames <- colnames(combined_df)
Year_Typ <- c("All Years","wet year", "mod year", "dry year")

#Define your output location, and example location is shown below, but should be updated.
#absolute and relative paths should be allowed
output_loc <- "~/Box Sync/UCD-B_Fish-Baseflows/Flow Data/FFC Issues/FFC Calculator Code - Working/KOT_2819852 _predicted_ffm.csv" 

#Defining the function that will plot the metrics 
Box_plot_print<- function(FF_Obs_Observed_Flows,name_1, FF_Obs_FNF,name_2, FF_Obs_4e,name_3,Year_type){
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
      geom_boxplot(aes(x= name_1, y=as.numeric(unlist(placeholder1))), color = "green")+
      geom_jitter(aes(x= name_1, y=as.numeric(unlist(placeholder1))),width = 0.2, height = 0, color = "darkgreen", alpha = 0.6) +
      geom_boxplot(aes(x= name_2, y=as.numeric(unlist(placeholder2))) , color = 'red')+
      geom_jitter(aes(x= name_2, y=as.numeric(unlist(placeholder2))) ,width = 0.2, height = 0, color = "darkred", alpha = 0.6) +
      geom_boxplot(aes(x= name_3, y= as.numeric(unlist(placeholder3))), color = 'blue' )+
      geom_jitter(aes(x= name_3, y= as.numeric(unlist(placeholder3))),width = 0.2, height = 0, color = "darkblue", alpha = 0.6) +
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
    Box_plot_print(Metric_dataframe_1,name_1,Metric_dataframe_2,name_2 = ,Metric_dataframe_3,name_3,typ)#
  }
  if(typ == "wet year"){
    cat(typ)
    FF_Obs_Observed_Flows <- filter(Metric_dataframe_1, WY_Cat == typ)
    FF_Obs_FNF <- filter(Metric_dataframe_2, WY_Cat == typ)
    FF_Obs_4e <- filter(Metric_dataframe_3, WY_Cat == typ)
    Box_plot_print(FF_Obs_Observed_Flows,name_1,FF_Obs_FNF,name_2,FF_Obs_4e,name_3,typ)
  }
  if(typ == "mod year"){
    cat(typ)
    FF_Obs_Observed_Flows <- filter(Alt_calc_LGN_observed, WY_Cat == typ)
    FF_Obs_FNF <- filter(TLG_FNF_Metics, WY_Cat == typ)
    FF_Obs_4e <- filter(Alt_calc_40_FNF_Varied_FERC, WY_Cat == typ)
    Box_plot_print(FF_Obs_Observed_Flows,name_1,FF_Obs_FNF,name_2,FF_Obs_4e,name_3,typ)
  }
  if(typ == "dry year"){
    cat(typ)
    FF_Obs_Observed_Flows <- filter(Alt_calc_LGN_observed, WY_Cat == typ)
    FF_Obs_FNF <- filter(TLG_FNF_Metics, WY_Cat == typ)
    FF_Obs_4e <- filter(Alt_calc_40_FNF_Varied_FERC, WY_Cat == typ)
    Box_plot_print(FF_Obs_Observed_Flows,name_1,FF_Obs_FNF,name_2,FF_Obs_4e,name_3,typ)
  }
}


####HERE is some working code for making box plots with a various number of data frames ####


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



