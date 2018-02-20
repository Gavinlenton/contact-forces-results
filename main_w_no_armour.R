## ----- PROCESS data ----- ##

# Load packages -----------------------------------------------------------
# Load Tidyverse (for some reason it doesn't recognise tidyverse as a package so I load it individual package)
require("ggplot2"); require("tibble"); require("tidyr"); require("readr"); require("purrr"); require("dplyr"); 
require("forcats"); require("tidyselect"); library("broom")

# Load libraries for multiple imputation by chained equations
require("Rcpp"); require("VIM"); require("mice"); require("car")

# Functions ---------------------------------------------------------------
## Function to give count, mean, standard deviation, standard error of the mean, and confidence interval
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm),
                     median = median (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Function to reshape data from wide to long format
reshape4ANOVA <- function(data2shape, varName){
  
  # Error check for numeric data input
  if(is.numeric(data2shape)) {
    stop('I am so sorry, but this function does not work for numeric input!\n',
         'You have provided an object of class: ', class(data2shape)[1])
  }
  
  # Reshape while preserving column name as variable name and leaving Participants as column
  df_final <- gather(data2shape, key = armour_type, value = !!varName, -Participants)
  
  # Add extra columns
  df_final$Speed <- as.factor(gl(2, 20, labels = c("Moderate", "Fast")))
  df_final$Mass <- as.factor(parse_number(df_final$armour_type)) # Find numbers from data because they correspond to mass
  
  # Select columns to keep (here we discard original armour_type column)
  df_final <- df_final %>% 
    select(Participants, Mass, Speed, !!varName)
  
  return(df_final);
}

# Function to impute missing values
missingValueImputation <- function(data2impute){
  
  # Check the pattern of missing values
  patternMissing = md.pattern(data2impute)
  
  # Plot missing values
  missing_plot <- aggr(data2impute, col=c('navyblue','yellow'),
                       numbers=TRUE, sortVars=TRUE,
                       labels=names(data2impute), cex.axis=1,
                       gap=3, ylab=c("Missing data","Pattern"))
  
  # maxit – Refers to no. of iterations taken to impute missing values
  # method – Refers to method used in imputation. we used predictive mean matching (pmm).
  # m  – Refers to 5 imputed data sets
  imputed_Data <- mice(data2impute, m=5, maxit = 5, method = 'pmm', printFlag = FALSE);
  
  completeData <- invisible(complete(imputed_Data,1))
  return(completeData);
}

# Function to detect outliers and remove - uses Tukey method (1.5 * IQR)
outlierKD <- function(dt, var) {
  
  # Get just the variable I want
  variable_frame <- dt %>% 
    select(!!var)
  variable<- variable_frame[[1]]
  
  na1 <- sum(is.na(variable)) # Get sum of missing values
  m1 <- mean(variable, na.rm = T) # Get mean of variable without values
  
  par(mfrow=c(2, 2), oma=c(1,0,1,0)) # Set up the plots
  # Plot boxplot and histogram of variable with outliers labelled
  boxplot(variable, main="With outliers", xlab = var)
  hist(variable, main="With outliers", xlab=var, ylab=NA)
  
  # Get outlier stats
  outlier <- boxplot.stats(variable)$out
  mo <- mean(outlier)
  
  # Remove outliers and assign to new variable
  variable_new <- ifelse(variable %in% outlier, NA, variable)
  # Make same plots as before
  boxplot(variable_new, main="Without outliers", xlab = var)
  hist(variable_new, main="Without outliers", xlab=var, ylab=NA)
  title(paste("Removing outliers"), outer=TRUE)
  na2 <- sum(is.na(variable_new))
  
  # Print to screen:
  cat("Outliers identified:", na2 - na1, "\n") # the number of outliers identified
  cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(variable))*100, 1), "\n") # the prorportion relative to entire data set
  cat("Mean of the outliers:", round(mo, 2), "\n")
  m2 <- mean(variable_new, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "\n")
  cat("Mean if we remove outliers:", round(m2, 2), "\n") # the change in mean if they are removed
  
  # Choose if outliers will be removed
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[ncol(dt)+1] <- invisible(variable_new)
    name <- paste(var, "noOut", sep = "_") # Choose new variable name
    names(dt)[ncol(dt)] <- name
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    
    # Choose which columns to keep in data frame - here we delete existing variable containing outliers
    dt <- dt %>% 
      select(Participants, Mass, Speed, !!name)
    
    cat("Outliers successfully removed", "\n")
    return(dt)
    return(response)
  } else{
    cat("Nothing changed", "\n")
    return(dt)
    return(response)
  }
}

# Functions to perform ANOVA. Takes data frame and the ANOVA model expressed as a string as inputs
anova_wrapper <- function(data, model_expression_as_string,...) {
  f_wrap <- paste0('function(.) {',model_expression_as_string,'}') %>% 
    parse(text=.) %>% eval
  data %>% 
    do(f_wrap(.) %>% 
         Anova(...=...) %>% 
         tidy) %>% return
}

# Function to output summary for all factors in ANOVA
outputSummary <- function(dt, varName, model_ANOVA){
  
  # Make summaries of all variables - get grouping variable names from ANOVA list of comparisons, the names are split by the ':' symbol
  # 2-way interactions
  summary_mass_speed <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=unlist(strsplit(model_ANOVA$term[3], split = ":"))) 
  
  # Main effects
  summary_speed <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=model_ANOVA$term[2])
  summary_mass <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=model_ANOVA$term[1])
  
  # Save to .text files
  cat('\n', 'MASS * SPEED\n'); print(summary_mass_speed, digits = 3);
  cat('\nMASS\n');print(summary_mass, digits = 3); 
  cat('\nSPEED\n'); print(summary_speed, digits = 3);
}

# Define paths ------------------------------------------------------------
# Define path to raw data
armour_type_dir <- "~/Google Drive/postDoctoralWork/2018 defence contact forces/EMG-assisted results/stats/contact-forces-results/with_NA/"
setwd(armour_type_dir);
fullFileNames <- dir(path = armour_type_dir, pattern="*.csv", full.names = TRUE)
fileNames <- dir(path = armour_type_dir, pattern="*.csv")
# Define sub-directories to save data in along the way
subDirText = "summaries"; subDirOutliers = "outliersRemoved"; subDirQQplot = "QQ_plots";

# Data processing ---------------------------------------------------------

# Load data and store in data frame
y <- 1
for(i in fullFileNames){
  
  # Get name of variable
  t <- regexpr(".csv", fileNames[y]) # Determine where the stop index is
  varName <- substr(fileNames[y], start = 1, stop = t-1) # Use substr to extract variable name without .csv
  
  # Read .csv file containing raw data
  exp.data = as_tibble(read.csv(i))
  
  # Redirect all output to a text file - create folder if it doesn't exist already
  if (file.exists(subDirText)){
    setwd(file.path(armour_type_dir, subDirText))
  } else {
    dir.create(file.path(armour_type_dir, subDirText))
    setwd(file.path(armour_type_dir, subDirText))
    
  }
  sink(paste(file.path(armour_type_dir, subDirText), "/", varName, "_results.txt", sep = ""), append=FALSE, split=FALSE)
  
  # Jump back out to armour type folder
  setwd(armour_type_dir);
  
  # Print variable name to text file
  cat("\n=========== VARIABLE: ", varName, " =============\n\n")
  
  # Reshape to long format
  exp.data.reshaped <- reshape4ANOVA(exp.data, varName)
  # Save reshaped data as temp format
  saveRDS(exp.data.reshaped, paste(armour_type_dir, "/", varName, "_tidied.rds", sep=""));
  
  # Check for outliers
  exp.data.l <- outlierKD(exp.data.reshaped, varName)
  
  # Impute missing values only if there are missing values
  exp.data.outRemoved <- missingValueImputation(exp.data.l)
  # Stop outputting to text
  sink()
  
  # Write new data to .RDS
  saveRDS(exp.data.outRemoved, paste(armour_type_dir, "/", varName, "_noOut.rds", sep = ""))
  
  # Output to text again
  sink(paste(file.path(armour_type_dir, subDirText), "/", varName, "_results.txt", sep = ""), append=TRUE, split=FALSE)
  
  # Test for normality with shapiro wilk method
  shapiro_test <- shapiro.test(exp.data.outRemoved[,length(exp.data.outRemoved)]);
  shapiro_test$data.name <- varName; print(shapiro_test)
  sink()
  
  # QQ plots to show normality
  require(graphics)
  graphics.off()
  
  # Save QQ plot - make directory to store plots if it doesn't exist already
  if (file.exists(subDirQQplot)){
    setwd(file.path(armour_type_dir, subDirQQplot))
  } else {
    dir.create(file.path(armour_type_dir, subDirQQplot))
    setwd(file.path(armour_type_dir, subDirQQplot))
  }
  
  png(file= paste(file.path(armour_type_dir, subDirQQplot), "/QQ_plot_", varName, ".png", sep = ""),
      width = 7, height = 5, units = 'in', res = 300)
  qqnorm(exp.data.outRemoved[,length(exp.data.outRemoved)], main = "Normal Q-Q Plot",
         xlab = "Theoretical Quantiles", ylab = varName,
         plot.it = TRUE, datax = FALSE)
  qqline(exp.data.outRemoved[,length(exp.data.outRemoved)], col = 2, probs = c(0.2, 0.8), qtype = 7)
  graphics.off(); setwd(armour_type_dir);
  
  # Output to same text file
  sink(paste(file.path(armour_type_dir, subDirText), "/", varName, "_results.txt", sep = ""), append=TRUE, split=FALSE)
  
  # Assign model for the ANOVA
  aov_model_expression_as_string = paste('aov(', varName, '_noOut ', '~ Mass * Speed, data = .)', sep = "") # Armour X Mass X Speed
  model_ANOVA <- exp.data.outRemoved %>% 
    anova_wrapper(model_expression_as_string = aov_model_expression_as_string, type="II") # Type 2 repeated measures ANOVA
  
  # Print to text file
  cat("ANOVA comparing mass and walking speed\n")
  print(model_ANOVA, digits = 3)
  
  # Generate summary statistics and print it all to the text file
  outputSummary(exp.data.outRemoved, varName, model_ANOVA)
  sink()
  
  # Create one large variable with all the data
  if (y == 1){
    allData <- exp.data.outRemoved
  } else{
    # Join with previous tibble and match participants, armour, mass, and speed
    allData <- allData %>% 
      inner_join(exp.data.outRemoved, by = c("Participants", "Mass", "Speed"))
  }
  
  # Increment y
  y = y + 1
}

# Save data frame containing all variables to csv file
write.csv(allData, paste0(armour_type_dir, "/", "allData.csv"))

# T-tests for main effects of load and speed - paired with Benjamini corrections
armour_type_dir <- "~/Google Drive/postDoctoralWork/2018 defence contact forces/EMG-assisted results/stats/contact-forces-results/with_NA/"
allData <- read.csv(paste0(armour_type_dir, 'allData.csv'))

# Comparison of moderate and fast speed
p_value_speeds <- allData %>%
  summarise_each(funs(t.test(.[Speed == "Moderate"], .[Speed == "Fast"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_withNA_noOut:total_kjcf_second_peak_withNA_noOut)

# Comparison of 0 and 15 kg data
p_value_0_15 <- allData %>%
  summarise_each(funs(t.test(.[Mass == "0"], .[Mass == "15"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_withNA_noOut:total_kjcf_second_peak_withNA_noOut)

t_value_0_15 <- allData %>%
  summarise_each(funs(t.test(.[Mass == "0"], .[Mass == "15"], paired = TRUE, p.adjust.methods = "BH")$statistic), 
                 vars = lateral_kjcf_peak_withNA_noOut:total_kjcf_second_peak_withNA_noOut)

print(p_value_0_15); print(t_value_0_15)

# Comparison of 15 and 30 kg data
p_value_15_30 <- allData %>%
  summarise_each(funs(t.test(.[Mass == "15"], .[Mass == "30"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_withNA_noOut:total_kjcf_second_peak_withNA_noOut)

t_value_15_30 <- allData %>%
  summarise_each(funs(t.test(.[Mass == "15"], .[Mass == "30"], paired = TRUE, p.adjust.methods = "BH")$statistic), 
                 vars = lateral_kjcf_peak_withNA_noOut:total_kjcf_second_peak_withNA_noOut)

print(p_value_15_30); print(t_value_15_30)

# Comparison of 0 and 30 kg data
p_value_0_30 <- allData %>%
  summarise_each(funs(t.test(.[Mass == "0"], .[Mass == "30"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_withNA_noOut:total_kjcf_second_peak_withNA_noOut)

t_value_0_30 <- allData %>%
  summarise_each(funs(t.test(.[Mass == "0"], .[Mass == "30"], paired = TRUE, p.adjust.methods = "BH")$statistic), 
                 vars = lateral_kjcf_peak_withNA_noOut:total_kjcf_second_peak_withNA_noOut)

print(p_value_0_30); print(t_value_0_30)

