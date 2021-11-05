library(dplyr)
library(tibbletime)
library(glue)
library(tidyverse)
library(anomalize)
library(lubridate)
library(scales)
library(dplyr)

xx_df = read_excel("", sheet = "")

plot_anomalies_funct<-function(df,value_col,freq,max_anoms,file_path,file_name,title){
  temp_plot = df %>% 
    time_decompose(value_col, method = "stl", frequency = freq, trend = "auto") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = max_anoms) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    scale_x_date(labels = date_format("%m-%d-%Y"),date_breaks = "1 week")+
    geom_line()+labs(title=title)
  ggsave(temp_plot,path = file_path,filename=plot_file_name,device = "png",width=20, height=7, dpi=100)
}

detect_anomalies<-function(ts,field,freq,max_anoms){
  freq = as.numeric(freq)
  max_anoms = as.numeric(max_anoms)
  anomaly_tibble <- ts %>% time_decompose(field, method = "stl", frequency = freq, trend = "auto") %>% anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = max_anoms)
  anomaly_df<-as.data.frame(anomaly_tibble)
  return (anomaly_df)
}
