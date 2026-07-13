library(tidyverse)
library(here)

violate <- read.csv(here("violate","data","clean.csv"))
comply <- read.csv(here("comply","data","clean.csv"))
congruency_proportions <- read.csv(here("congruency_proportions","data","clean.csv"))
speed_accuracy <- read.csv(here("speed_accuracy","data","clean.csv")) 
stroop_flanker <- read.csv(here("stroop_flanker","data","clean.csv"))
punishment <- read.csv(here("punishment","data","clean.csv"))


#merge violate and comply datasets first, since they are the most similar
#some preparation: remove index column and practice trials, add column with study name
violate <- violate |> 
  select(-X) |> 
  mutate(study = rep("violate",length(violate$subj_code))) |> 
  filter(case!="noise") 

#same for comply
comply <- comply |> 
  select(-X) |> 
  mutate(study = rep("comply",length(comply$subj_code))) |> 
  filter(case!="noise")

