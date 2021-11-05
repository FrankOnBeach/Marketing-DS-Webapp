library(CausalImpact)
library(readxl)
library(dplyr) 
library(ggplot2)
library(scales)
library(tidyr)
library("reshape2")


# Reading in all the sheets
xx_df = read_excel("", sheet = "")

# Causal Impact Function
causal_impact_analysis<-function(impact_point,causal_df){
  pre.period <- c(1,impact_point)
  post.period <- c(impact_point+1,nrow(causal_df))
  impact <- CausalImpact(causal_df, pre.period, post.period)
  return (impact)
}

impact = causal_impact_analysis(impact_point,xx_df)

impact