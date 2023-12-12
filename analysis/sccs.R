# Fitting the SCCS model to the data 
# For national analysis
# Format data
if (dat_type == "drc") {
  outb <- readxl::read_excel("drc_dat.xlsx", sheet = "outb")
  conf <- readxl::read_excel("drc_dat.xlsx", sheet = "conf")  
} else {
  outb <- readxl::read_excel("gui_dat.xlsx", sheet = "outb")
  conf <- readxl::read_excel("gui_dat.xlsx", sheet = "conf") 
}

# Subset the outbreaks data to those with >1 case 
##NG: do you mean == 1?
# I have a dataset with days with no reported cases, and its a case only method 
outb <- subset(outb, case == 1)

# Format outbreaks and conflict data 
outb <- data.frame(admin = outb$admin,
                   eventday = outb$continuous_weeks)
conf <- data.frame(admin = conf$admin,
                   exday = conf$continuous_weeks) 
outb <- distinct(outb)
conf <- distinct(conf)
data1 <- merge(outb, conf, by = "admin", all = TRUE)
data1 <- na.omit(data1)

# Create observation period 
# For weeks 
data1$start <- 1
data1$end <- 183

# Create exposure period
# For weeks
data1$start1 <- data1$exday + 0 
data1$end0 <- data1$exday + 0
data1$end1 <- data1$exday + 1
data1$end2 <- data1$exday + 2
data1$end4 <- data1$exday + 4
data1$end6 <- data1$exday + 6
data1$end8 <- data1$exday + 8
data1$end10 <- data1$exday + 10 

# Create am ID number of each outbreaks and conflict in each province/state 
data1 <- data1  %>% 
  mutate(indiv = group_indices(., admin, exday, eventday)) 
data1 <- data1 %>% distinct()

# Fit the models to the data 
# Onset
datLong <- plyr::ddply(.data = data1,
                       .variables = "indiv",
                       .fun = function(df) {
                         
                         cuts <- with(df, sort(c(start, end, start1, end1))) 
                         
                         out <- data.frame(indiv = df$indiv,
                                           exday = df$exday,
                                           eventday = df$eventday,
                                           start = head(cuts, -1),
                                           end   = cuts[-1])
                         
                         out$event <- as.numeric(out$start <= df$eventday & df$eventday <= out$end)
                         
                         out$exgr <- as.numeric(df$start1 <= out$start & out$end <= df$end1)
                         
                         out$interval <- out$end - out$start
                         out$loginterval <- log(out$interval)
                         
                         out
                       })

datLong <- subset(datLong, start < 183) 
datLong[datLong=="-Inf"]<-0

clogit_0 <- clogit(event ~ exgr + strata(indiv) + offset(loginterval), data = datLong)
results2 <- summary(clogit_0)
results2 <- data.frame(results2$coefficients, results2$conf.int)


