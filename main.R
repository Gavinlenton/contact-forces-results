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
  df_final$Armour <- as.factor(gl(4, 80, labels = c("pARM1", "cARM1", "TBAS", "cARM2")))
  #df_final$Armour <- as.factor(unlist(strsplit(df_final$armour_type, "_")))[seq(from = 2, to = 625, by = 2)]
  df_final$Speed <- as.factor(gl(2, 20, labels = c("Moderate", "Fast")))
  df_final$Mass <- as.factor(parse_number(df_final$armour_type)) # Find numbers from data because they correspond to mass
  
  # Select columns to keep (here we discard original armour_type column)
  df_final <- df_final %>% 
    select(Participants, Armour, Mass, Speed, !!varName)
  
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
      select(Participants, Armour, Mass, Speed, !!name)
    
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
  
  # Make summaries of all variables
  # 3-way interaction - get grouping variable names from ANOVA list of comparisons, the names are split by the ':' symbol
  summary_all <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=unlist(strsplit(model_ANOVA$term[7], split = ":"))) 
  
  # 2-way interactions
  summary_mass_speed <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=unlist(strsplit(model_ANOVA$term[6], split = ":"))) 
  summary_armour_speed <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=unlist(strsplit(model_ANOVA$term[5], split = ":")))
  summary_armour_mass <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=unlist(strsplit(model_ANOVA$term[4], split = ":")))

  # Main effects
  summary_speed <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=model_ANOVA$term[3])
  summary_mass <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=model_ANOVA$term[2])
  summary_armour <- summarySE(data = dt, measurevar=paste(varName, "noOut", sep = "_"), groupvars=model_ANOVA$term[1]) 

  # Save to .text files
  cat('\n', 'COMPARISON OF ALL\n'); print(summary_all, digits = 3); cat('\n', 'ARMOUR * MASS\n'); print(summary_armour_mass, digits = 3); 
  cat('\n', 'ARMOUR * SPEED\n'); print(summary_armour_speed, digits = 3); cat('\n', 'MASS * SPEED\n'); print(summary_mass_speed, digits = 3);
  cat('\nARMOUR\n'); print(summary_armour, digits = 3); cat('\nMASS\n');print(summary_mass, digits = 3); 
  cat('\nSPEED\n'); print(summary_speed, digits = 3);
}

# Define paths ------------------------------------------------------------
# Define path to raw data
armour_type_dir <- "~/Google Drive/postDoctoralWork/2018 defence contact forces/EMG-assisted results/stats/contact-forces-results/only_armour_type"
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
  aov_model_expression_as_string = paste('aov(', varName, '_noOut ', '~ Armour * Mass * Speed, data = .)', sep = "") # Armour X Mass X Speed
  model_ANOVA <- exp.data.outRemoved %>% 
    anova_wrapper(model_expression_as_string = aov_model_expression_as_string, type="II") # Type 2 repeated measures ANOVA
  
  # Print to text file
  cat("ANOVA comparing armour types, mass, and walking speed\n")
  print(model_ANOVA, digits = 3)

  # Detect p-values <0.05 and perform post-hoc tests 
  # if (model_ANOVA$p.value[1:length(model_ANOVA$p.value)-1] < 0.05) {
  #   sigDiff <- as.tibble(model_ANOVA$p.value[1:length(model_ANOVA$p.value)-1] < 0.05)
  #   diffIndex <- which(sigDiff$value, arr.ind = FALSE, useNames = TRUE) # Find index of sig diff
  #   variablesWithDiff <- unlist(strsplit(model_ANOVA$term[diffIndex], split = ":")) # Get name of factors with difference
  #   # Run t.tests for comparisons
  #   if (length(varName < 2)) {
  #     t.test(paste0(varName, '_noOut ', '~ ', variablesWithDiff[1]), data = ., mu = 0, paired = TRUE,
  #            conf.level = 0.95)
  #     } else {
  #       t.test(paste0(varName, '_noOut ', '~ ', variablesWithDiff[1]), data = ., mu = 0, paired = TRUE,
  #              conf.level = 0.95)
  #       t.test(paste0(varName, '_noOut ', '~ ', variablesWithDiff[2]), data = ., mu = 0, paired = TRUE,
  #              conf.level = 0.95)
  #   }
  # 
  # }
  
  # Generate summary statistics and print it all to the text file
  outputSummary(exp.data.outRemoved, varName, model_ANOVA)
  sink()

  # Create one large variable with all the data
  if (y == 1){
    allData <- exp.data.outRemoved
  } else{
    # Join with previous tibble and match participants, armour, mass, and speed
    allData <- allData %>% 
      inner_join(exp.data.outRemoved, by = c("Participants", "Armour", "Mass", "Speed"))
  }
  
  # Increment y
  y = y + 1
}

# Save data frame containing all variables to csv file
write.csv(allData, paste0(armour_type_dir, "/", "allData.csv"))
saveRDS(allData, paste0(armour_type_dir, "/", "allData.rds"))

# T-tests for main effects of load and speed - paired with Benjamini corrections
armour_type_dir <- "~/Google Drive/postDoctoralWork/2018 defence contact forces/EMG-assisted results/stats/contact-forces-results/only_armour_type/"
allData <- read.csv(paste0(armour_type_dir, 'allData.csv'))

# T-Tests for differences between armour types at 30 kg and fast walking  ---------------------------------------------------------------------

data_30_fast<-  allData %>% 
  filter(Mass == 30, Speed == "Fast")

p_value_TBAS_cARM1 <- data_30_fast %>%
  summarise_each(funs(t.test(.[Armour == "TBAS"], .[Armour == "cARM1"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_noOut:total_kjcf_second_peak_noOut)

p_value_TBAS_cARM2 <- data_30_fast %>%
  summarise_each(funs(t.test(.[Armour == "TBAS"], .[Armour == "cARM2"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_noOut:total_kjcf_second_peak_noOut)

p_value_TBAS_pARM1 <- data_30_fast %>%
  summarise_each(funs(t.test(.[Armour == "TBAS"], .[Armour == "pARM1"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_noOut:total_kjcf_second_peak_noOut)

p_value_cARM1_cARM2 <- data_30_fast %>%
  summarise_each(funs(t.test(.[Armour == "cARM1"], .[Armour == "cARM2"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_noOut:total_kjcf_second_peak_noOut)

p_value_cARM1_pARM1 <- data_30_fast %>%
  summarise_each(funs(t.test(.[Armour == "cARM1"], .[Armour == "pARM1"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_noOut:total_kjcf_second_peak_noOut)

p_value_cARM2_pARM1 <- data_30_fast %>%
  summarise_each(funs(t.test(.[Armour == "cARM2"], .[Armour == "pARM1"], paired = TRUE, p.adjust.methods = "BH")$p.value), 
                 vars = lateral_kjcf_peak_noOut:total_kjcf_second_peak_noOut)

# Plots of all variables
newOrder <- c("TBAS", "cARM1", "cARM2", "pARM1")
fileNamesForSummary <- names(allData[,5:length(allData)])

datalist = list();
iteration <- 1
# Loop through variables and create a summary
for (variableName in fileNamesForSummary){
  summary_data <- as.tibble(summarySE(data = data_30_fast, measurevar= variableName, groupvars="Armour")) %>% 
    mutate(variable = variableName) %>% # Append the variable name as an extra factor
    slice(match(newOrder, Armour)) # Creates summary tibble with Armour sorted accroding to newOrder
  levels(summary_data$Armour) <- newOrder
  datalist[[iteration]] <- summary_data # Add the data to the data list
  iteration <- iteration + 1; # Loop to next iteration
}

# Combine data
combined_data <- dplyr::bind_rows(datalist)
combined_data$variable <- as.factor(combined_data$variable); # Add factors for each variable
 
tiff(file="contact_forces_30kg_fast_diffArmour.tiff",width = 5, height = 3, units = 'in', res = 1200)

# All on one plot kjcf
plot_kjcf <- ggplot(combined_data, aes(x=variable, y=mean, fill = Armour)) +  
  geom_errorbar(aes(ymin=0, ymax=mean+ci), width=.3, position=position_dodge(0.6), colour="grey") +
  geom_bar(position=position_dodge(0.6), stat="identity", width = 0.5) + 
  coord_cartesian(ylim=c(0,7)) +
  scale_y_continuous(breaks=0:10*1, expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0.02, 0), 
                   breaks = fileNamesForSummary,
                   labels = c("Lateral", "Medial first peak", 
                              "Medial second peak", "Total first peak", "Total second peak")) +
  ylab(expression(Knee ~ joint ~ contact ~ force ~ (Nkg^{-1}))) +
  theme_classic(base_size = 8, base_family = "Arial") +
  theme(legend.background = element_rect(), legend.title = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = "bottom",
        panel.grid.minor.y = element_blank(), axis.line.y = element_line(colour = "grey"), 
        axis.title.y = element_text(size=8), axis.ticks.x = element_line(colour = "grey"),
        axis.line.x = element_line(colour = "grey"), axis.ticks.y = element_line(colour = "grey"),
        axis.title.x = element_blank(), plot.title = element_text(hjust = -0.01, vjust=2.12),
        axis.text.x = element_text(colour = "black"))

plot_kjcf
dev.off()

# Logistic regression -----------------------------------------------------
mylogit_latcf <- glm(allData$vest_fit_f ~  lateral_kjcf_peak_noOut, family = "binomial" , data = allData)
mylogit_max_pressure <- glm(data4probit_noTBAS$shoulders_comf_f ~  max_pressure_f, family = "binomial" , data = data4probit_noTBAS)
mylogit_max_pressure_unloading <- glm(data4probit_noTBAS$offset_f ~  max_pressure_f, family = "binomial" , data = data4probit_noTBAS)

# Generate summaries for plotting
summary_medCF_fit <- summarySE(data = allData, measurevar= "total_kjcf_first_peak_noOut", groupvars= c("Armour", "vest_fit_f"))
summary_medCF_fit$vest_fit_f <-  as.factor(summary_medCF_fit$vest_fit_f)
levels(summary_medCF_fit$Armour) <- newOrder

saveRDS(summary_medCF_fit, paste(armour_type_dir, "/", "lateral_kjcf_peak", "_vest_fit.rds", sep = ""))

# Plot contact force and armour fit
tiff(file="vest_fit_total_contact_force_firstPeak.tiff",width = 5, height = 4, units = 'in', res = 900, compression = "lzw")

tt <- ggplot(summary_medCF_fit, aes(x=Armour, y=mean, fill = vest_fit_f)) + 
  geom_bar(position=position_dodge(0.7), stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),
                width=.08, size = 0.5, position=position_dodge(0.7), colour="gray40") +
  ylab(expression(Knee ~ joint ~ contact ~ force ~ (Nkg^{-1}))) +
  coord_cartesian(ylim=c(0,8)) +
  scale_fill_manual(name="", # Legend label, use darker colors
                    breaks=c("0", "1"),
                    labels=c("Not acceptable", "Acceptable"),
                    values = c("lightskyblue1", "deepskyblue3")) +
  scale_y_continuous(breaks=0:7*1, expand = c(0, 0)) + 
  theme_classic(base_size = 14, base_family = "Arial") +
  theme(legend.background = element_rect(), legend.direction = "horizontal", legend.position = "bottom",
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.y = element_line(colour="grey"),
        panel.grid.minor.y = element_blank(), axis.ticks.x = element_blank(), axis.line.y = element_line(colour = "grey"),
        axis.line.x = element_line(colour = "grey"),axis.title.x = element_blank()) 
tt
dev.off()



