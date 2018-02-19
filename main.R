## ----- PROCESS data ----- ##

# Load Tidyverse (for some reason it doesn't recognise tidyverse as a package so I load it individual package)
require("ggplot2"); require("tibble"); require("tidyr"); require("readr"); require("purrr"); require("dplyr"); require("forcats"); require("tidyselect")

# Example function with error check
mmm2 <- function(x) {
  if(!is.numeric(x)) {
    stop('I am so sorry, but this function only works for numeric input!\n',
         'You have provided an object of class: ', class(x)[1])
  }
  max(x) - min(x)
}

# Function to reshape data from wide to long format
reshape4ANOVA <- function(data2shape, varName){
  
  # Reshape while preserving column name as variable name and leaving Participants as column
  df_final <- gather(data2shape, key = armour_type, value = !!varName, -Participants)
  
  # Add extra columns
  df_final$armour <- as.factor(gl(4, 80, labels = c("pARM1", "cARM1", "TBAS", "cARM2")))
  df_final$speed <- as.factor(gl(2, 20, labels = c("Moderate", "Fast")))
  df_final$mass <- as.factor(gl(2, 40, labels = c("15kg", "30kg")))
  df_final$L1 <- NULL
  
  return(df_final);
}

# Function to impute missing values
missingValueImputation <- function(data2impute){
  
  # Check the pattern of missing values
  patternMissing = md.pattern(data2impute)
  
  # Plot missing values
  missing_plot <- aggr(data2impute, col=c('navyblue','yellow'),
  numbers=TRUE, sortVars=TRUE,
  labels=names(data2impute), cex.axis=.7,
  gap=3, ylab=c("Missing data","Pattern"))
  
  # maxit – Refers to no. of iterations taken to impute missing values
  # method – Refers to method used in imputation. we used predictive mean matching.
  # m  – Refers to 5 imputed data sets
  imputed_Data <- invisible(mice(data2impute, m=5, maxit = 5, method = 'pmm'));
  
  completeData <- invisible(complete(imputed_Data,1))
  
  return(completeData);
}

# Function to detect outliers and remove - uses Tukey method (1.5 * IQR)
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,0,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name_new <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name_new, main="Without outliers")
  hist(var_name_new, main="Without outliers", xlab=NA, ylab=NA)
  title(paste(varName), outer=TRUE)
  na2 <- sum(is.na(var_name_new))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name_new, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[ncol(dt)+1] <- invisible(var_name_new)
    name <- "data"
    names(dt)[ncol(dt)] <- name
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed")
    return(dt)
    return(response)
  } else{
    cat("Nothing changed", "n")
    return(dt)
    return(response)
  }
}

# Import data
armour_type_dir <- "~/Google Drive/postDoctoralWork/2018 defence contact forces/EMG-assisted results/stats/contact-forces-results/only_armour_type"
fullFileNames <- dir(path = armour_type_dir, pattern="*.csv", full.names = TRUE)
fileNames <- dir(path = armour_type_dir, pattern="*.csv")

# Load data and store in data frame
y <- 1
for(i in fullFileNames){
  
  # Get name of variable
  t <- regexpr(".csv", fileNames[y])
  varName <- substr(fileNames[y], 1, t-1)
  
  # Read .csv file
  exp.data = as_tibble(read.csv(i))
  
  # Redirect all output to a text file.
  sink(paste("./summary_text_files/", varName, "_results.txt", sep = ""), append=FALSE, split=FALSE)
  
  print("");
  print("");
  print("=========================== VARIABLE: ============================")
  print(varName)
  print("==================================================================")
  
  # Reshape to long format
  exp.data.reshaped <- reshape4ANOVA(exp.data, varName)
  
  # Check for outliers
  exp.data.reshaped[!exp.data.reshaped %in% boxplot.stats(exp.data.reshaped)$out]
  #exp.data.l <- outlierKD(exp.data.reshaped, data)
  
  # Stop outputting to text
  sink()
  
  # Impute missing values only if there are missing values
  exp.data.noOut <- missingValueImputation(exp.data.l[, c(1, 3:length(exp.data.l))])
  
  # Write new data to .csv
  write.csv(exp.data.noOut, paste("./outliersRemoved/", varName, "_noOut.csv", sep = ""))
  
  # exp.data <- exp.data%>%
  #   mutate(country = factor(country),
  #          continent = factor(continent))
  # str(gapminder)
  
  y <- y + 1
}

# filter() takes logical expressions and returns the rows for which all are TRUE.

# Use %>% as a pipe operator for functions and data frames. Basically, %>%  means "then"
# E.g.: gapminder %>%                             Or in normal R language: gapminder[gapminder$country == "Cambodia", c("year", "lifeExp")]
#         filter(country == "Cambodia") %>%
#         select(year, lifeExp)


# Use rename() to rename variables

# everything() is one of several helpers for variable selection

# group_by() adds extra structure to your dataset – grouping information – which lays the groundwork for computations within the groups.

# summarize_at() applies the same summary function(s) to multiple variables.

# For example
# my_gap %>%
#   filter(year %in% c(1952, 2007)) %>%
#   group_by(continent, year) %>%
#   summarize_at(vars(lifeExp, gdpPercap), funs(mean, median))

# Use saveRDS(gap_life_exp, "gap_life_exp.rds") to preserve factor level order etc. and readRDS() to read it back in the same way