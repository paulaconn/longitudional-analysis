#
# Analysis of senior students by comparing the pre and senior responses.
# Requires libraries below, and seniors.csv and seniors-difference.csv files
#
# By Paula 
#

library(dplyr)
library(plyr)
library(tidyr)
library(broom) #for tidy function
library(PairedData)
library(ggpubr)
library(extrafont)


readData <- function(filename) {
  df <- read.csv(file=filename, header=TRUE, sep=",")
  df <- df %>% dplyr::select(-Participant) %>% filter_all(any_vars(!is.na(.)))
  return(df)
}

shapiroTest <- function(df){
  # Runs Shapiro Wilk for normality prior to testing. May require broom library.
  #
  # Note:
  # Null hypothesis is that the data is normally distributed. If p<0.05, then 
  # the data is not normally distributed and a non-parametric test should be used.
  # 
  results <- df %>% 
    gather(key = "variable_name", value = "value", Voting_PRE:Knowledge_SENIOR) %>% 
    group_by(variable_name, Condition)  %>% 
    do(tidy(shapiro.test(.$value))) %>% 
    ungroup() %>% dplyr::select(-method)
  return(results)
}

wilcoxonTest <- function(df) {
  # Runs paired Wilcoxon Signed Rank test by comparing the pre and senior
  # responses per condition.
  #
  # Note:
  # Null hypothesis is that the the data has equal medians.
  # 
  all_index <- c(1:(ncol(df)-1))
  odd_index <- all_index%%2==1
  for (x in all_index[odd_index]) {
    columns_test <- df %>% dplyr::select(c(colnames(df[x]), colnames(df[x+1]))) %>% drop_na()
    if (x==1) {
      condition_id <- df$Condition[1]
      cat('===================================================================')
      cat(paste0('\nAnalyzing Student Condition (Wilcoxon Signed Rank Test): ', 
        condition_id, '\n'))
    }
    cat_pre_data <- as.numeric(unlist(columns_test[,1]))
    cat_post_data <- as.numeric(unlist(columns_test[,2]))
    
    w_result <- wilcox.test(cat_pre_data, cat_post_data, paired = TRUE, 
                            exact = FALSE, conf.int = FALSE, conf.lev = 0.95)
    z_value <- qnorm(w_result$p.value)
    print(paste0(colnames(columns_test[1]), ' and ', colnames(columns_test[2]), 
                 ': stat (', sprintf("%.3f", w_result$statistic), '), ',
                 ': z-value (', sprintf("%.3f", z_value), '), ',
                 ': p-value (', sprintf("%.3f", w_result$p.value), ')'))
    
    format_df <- formatData(cat_pre_data, cat_post_data)          #uncomment to see descriptive stats or boxplot
    print(calculateDescriptiveStats(format_df))                   #uncomment to see descriptive stats
    
    if (w_result$p.value<0.05) {
      makeBoxPlot(format_df, condition_id, colnames(columns_test[1]))
    }
  }
}

formatData <- function(input_pre, input_post) {
  # Formats pre and senior data for each condition being analyzed. This format is
  # used in the calculateDescriptiveStats and makeBoxPlot functions.
  # 
  formatted_df <- data.frame( 
    group = rep(c("2nd/3rd-Year (pre)", "Senior"), each = length(input_pre)),
    score = c(input_pre, input_post)
  )
  return(formatted_df)
}

calculateDescriptiveStats <- function(formatted_df) {
  # Calculates descriptive stats for each of the conditions
  # 
  data<- dplyr::group_by(formatted_df, group) %>% dplyr::summarise(
    count = n(), min = min(score), max = max(score),
    median = median(score, na.rm = TRUE),
    Q1 = quantile(score, 0.25),
    Q3 = quantile(score, 0.75),
    IQR = IQR(score, na.rm = TRUE))
  return(data)
}

makeBoxPlot <- function(formatted_df, input_condition, group_name) {
  # Creates boxplot for results that are significant as per Wilcoxon Signed Rank tests
  # 

  # Create custom strings to label the graphs:
  test_id <- strsplit(group_name, "_")
  filename_string <- paste0(getwd(),'/',test_id[[1]][1], '-', input_condition, '_plot.png')
  ylabel_string <- paste0(test_id[[1]][1], " Composite Score")
  title_string <- paste0("Paired ", test_id[[1]][1], 
    " Score at Pre and Senior Intervals (", input_condition, ")")
  
  # Create the plots and save to folder
  png(filename=filename_string)
  plot(ggboxplot(formatted_df, x = "group", y = "score", 
            title = title_string,
            color = "group", palette = c("#00AFBB", "#E7B800"),
            order = c("2nd/3rd-Year (pre)", "Senior"),
            ylab = ylabel_string,
            xlab = "Academic Year") +
    theme(text=element_text(family="Baskerville", size=12),
          plot.title = element_text(hjust = 0.5)))
  dev.off()
}

mannWhitneyTest <- function(filename) {
  mw_df <- readData(filename)
  mw_result <- wilcox.test(Difference ~ Condition, data=mw_df, paired = FALSE, exact = FALSE) 
  z_value <- qnorm(mw_result$p.value)
  r <- abs(z_value)/sqrt(nrow(mw_df))
  print(paste0('Mann Whitney results:', 
               ': stat (', sprintf("%.3f", mw_result$statistic), '), ',
               ': p-val (', sprintf("%.3f", mw_result$p.value), '), ',
               ': r (', sprintf("%.3f", r), ')'))
}
  

######################
# MAIN EXECUTION
######################
df <- readData("seniors.csv")
results <- shapiroTest(df) #uncomment to run Shapiro-Wilk Test
#df %>% group_by(Condition) %>% do(data.frame(wilcoxonTest(.))) #uncomment to run Wilcoxon Signed Rank Test
#mannWhitneyTest("seniors-difference.csv") #uncomment to run Mann-Whitney: tests to see if knowledge scores are different for the two conditions
