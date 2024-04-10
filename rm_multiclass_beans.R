setwd('C:\\Users\\criss\\OneDrive\\Documentos\\Projetos\\PIBIC\\IC_Random-Machines')
library(tidyverse)

source('rm_new.R')
beans <- openxlsx::read.xlsx("beandata.xlsx") %>%
  mutate(Class = factor(Class, ordered = FALSE))
