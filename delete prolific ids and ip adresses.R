
# this script was created to remove identifying information from the raw data of all studies
# specifically, we deleted prolific IDs and ip addresses

library(here)
library(tidyverse)

# violate

## raw data
d1 <- read.csv(here("violate","data","raw_batch1.csv"))
d2 <- read.csv(here("violate","data","raw_batch2.csv"))

##delete prolific ids and ip
d1$PROLIFIC_PID <- c(rep("redacted",length(d1$PROLIFIC_PID)))
d1$ip <- c(rep("redacted",length(d1$ip)))

d2$PROLIFIC_PID <- c(rep("redacted",length(d2$PROLIFIC_PID)))
d2$ip <- c(rep("redacted",length(d2$ip)))

##export
write.csv(d1,here("violate","data","raw_batch1.csv"),row.names = F)
write.csv(d2,here("violate","data","raw_batch2.csv"),row.names = F)

# comply
## raw data
d3 <- read.csv(here("comply","data","raw_batch1.csv"))
d4 <- read.csv(here("comply","data","raw_batch2.csv"))

##delete prolific ids, subject ids, and ip
d3$PROLIFIC_PID <- c(rep("redacted",length(d3$PROLIFIC_PID)))
d3$ip <- c(rep("redacted",length(d3$ip)))

d4$PROLIFIC_PID <- c(rep("redacted",length(d4$PROLIFIC_PID)))
d4$ip <- c(rep("redacted",length(d4$ip)))

##export
write.csv(d3,here("comply","data","raw_batch1.csv"),row.names = F)
write.csv(d4,here("comply","data","raw_batch2.csv"),row.names = F)

# congruency proportions
d5 <- read.csv(here("congruency_proportions","data","raw_batch1.csv"))
d6 <- read.csv(here("congruency_proportions","data","raw_batch2.csv"))

##delete prolific ids, subject ids, and ip
d5$PROLIFIC_PID <- c(rep("redacted",length(d5$PROLIFIC_PID)))
d5$ip <- c(rep("redacted",length(d5$ip)))

d6$PROLIFIC_PID <- c(rep("redacted",length(d6$PROLIFIC_PID)))
d6$ip <- c(rep("redacted",length(d6$ip)))

##export
write.csv(d5,here("congruency_proportions","data","raw_batch1.csv"),row.names = F)
write.csv(d6,here("congruency_proportions","data","raw_batch2.csv"),row.names = F)

# speed accuracy
d7 <- read.csv(here("speed_accuracy","data","raw_batch1.csv"))
d8 <- read.csv(here("speed_accuracy","data","raw_batch2.csv"))

##delete prolific ids, subject ids, and ip
d7$PROLIFIC_PID <- c(rep("redacted",length(d7$PROLIFIC_PID)))
d7$ip <- c(rep("redacted",length(d7$ip)))

d8$PROLIFIC_PID <- c(rep("redacted",length(d8$PROLIFIC_PID)))
d8$ip <- c(rep("redacted",length(d8$ip)))

##export
write.csv(d7,here("speed_accuracy","data","raw_batch1.csv"),row.names = F)
write.csv(d8,here("speed_accuracy","data","raw_batch2.csv"),row.names = F)

# stroop flanker - by strata
d9 <- read.csv(here("stroop_flanker","data","strata1.csv"))
d9$PROLIFIC_PID <- c(rep("redacted",length(d9$PROLIFIC_PID)))
d9$ip <- c(rep("redacted",length(d9$ip)))
write.csv(d9,here("stroop_flanker","data","strata1.csv"),row.names = F)

d10 <- read.csv(here("stroop_flanker","data","strata2.csv"))
d10$PROLIFIC_PID <- c(rep("redacted",length(d10$PROLIFIC_PID)))
d10$ip <- c(rep("redacted",length(d10$ip)))
write.csv(d10,here("stroop_flanker","data","strata2.csv"),row.names = F)

d11 <- read.csv(here("stroop_flanker","data","strata3.csv"))
d11$PROLIFIC_PID <- c(rep("redacted",length(d11$PROLIFIC_PID)))
d11$ip <- c(rep("redacted",length(d11$ip)))
write.csv(d11,here("stroop_flanker","data","strata3.csv"),row.names = F)

d12 <- read.csv(here("stroop_flanker","data","strata4.csv"))
d12$PROLIFIC_PID <- c(rep("redacted",length(d12$PROLIFIC_PID)))
d12$ip <- c(rep("redacted",length(d12$ip)))
write.csv(d12,here("stroop_flanker","data","strata4.csv"),row.names = F)

d13 <- read.csv(here("stroop_flanker","data","strata5.csv"))
d13$PROLIFIC_PID <- c(rep("redacted",length(d13$PROLIFIC_PID)))
d13$ip <- c(rep("redacted",length(d13$ip)))
write.csv(d13,here("stroop_flanker","data","strata5.csv"),row.names = F)

d14 <- read.csv(here("stroop_flanker","data","strata6.csv"))
d14$PROLIFIC_PID <- c(rep("redacted",length(d14$PROLIFIC_PID)))
d14$ip <- c(rep("redacted",length(d14$ip)))
write.csv(d14,here("stroop_flanker","data","strata6.csv"),row.names = F)

# punishment
d15 <- read.csv(here("punishment","data","raw_batch1.csv"))
d16 <- read.csv(here("punishment","data","raw_batch2.csv"))
d17 <- read.csv(here("punishment","data","raw_batch3.csv"))

d15$PROLIFIC_PID <- c(rep("redacted",length(d15$PROLIFIC_PID)))
d15$ip <- c(rep("redacted",length(d15$ip)))
write.csv(d15,here("punishment","data","raw_batch1.csv"),row.names = F)

d16$PROLIFIC_PID <- c(rep("redacted",length(d16$PROLIFIC_PID)))
d16$ip <- c(rep("redacted",length(d16$ip)))
write.csv(d16,here("punishment","data","raw_batch2.csv"),row.names = F)

d17$PROLIFIC_PID <- c(rep("redacted",length(d17$PROLIFIC_PID)))
d17$ip <- c(rep("redacted",length(d17$ip)))
write.csv(d17,here("punishment","data","raw_batch3.csv"),row.names = F)

