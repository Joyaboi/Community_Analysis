####load packages####
install.packages("tidyverse")
install.packages("vegan")
install.packages("betapart")
library(tidyverse)
library(vegan)
library(betapart)

####Load Database and Format Data####
dat.all <- read.csv("PacIFlora/Species_list_full_2905.csv", header=T, sep = ';')

#Remove rows with NA's for species and Islands
idx_drop <- which(is.na(dat.all$species) | is.na(dat.all$island))
dat.all <- dat.all[-idx_drop, ]

#Create data frames from dat.all
.soc<- which(dat.all$islandgroup=="Society") #Find rows that contain Society Island Group
dat.soc <- dat.all[.soc, ] #Create a data frame for only the Society Island group

.mar<- which(dat.all$islandgroup=="Marquesas") #Find rows that contain Marquesas Island Group
dat.mar <- dat.all[.mar, ] #Create a data frame for only the Marquesas Island group

.haw<- which(dat.all$islandgroup=="Hawaiian") #Find rows that contain Hawaiian Island Group
dat.haw <- dat.all[.haw, ] #Create a data frame for only the Hawaiian Island group

.sam<- which(dat.all$islandgroup=="Samoa") #Find rows that contain Samoa Island Group
dat.sam <- dat.all[.sam,] #Create a data frame for only the Samoa Island group

.fij<- which(dat.all$islandgroup=="Fiji") #Find rows that contain Fiji Island Group
dat.fij <- dat.all[.fij, ] #Create a data frame for only the Fiji Island group

.comb <- c(.soc,.mar,.haw,.sam,.fij) #Gather all rows that contain our Island Groups
dat.comb <- dat.all[.comb, ] #Create a data frame that combines our Island Groups

#Create site-by-site data frames
dat.soc.red <- dat.soc[ ,c("species","island")] # create a data frame for .soc with just species and island columns
dat.soc.red$presence <- 1                      # create another column for presence and fill it with 1's
dat.soc.pa <- dat.soc.red %>%                  # start building a presence/absence table (islands as rows, species as columns)
  pivot_wider(names_from = species,            # tidyr: make each species a column name
values_from = presence)                        # fill those columns with the presence values (1), NAs where absent
list0 <- as.list(rep(0, ncol(dat.soc.pa)))     # build a list of zeros the same length as the number of columns
names(list0) <- names(dat.soc.pa)              # name the list elements to match the data frame columns (required by replace_na)
dat.soc.pa <- as.data.frame(dat.soc.pa %>%     # convert tibble to data.frame after replacement (optional but keeps base-R style)
  replace_na(list0))                           # replace all NAs in every column with 0
row.names(dat.soc.pa) <- dat.soc.pa$island     # set row names to the island names
dat.soc.pa <- dat.soc.pa[ , -1]                # drop the 'island' column (now stored in row names), leaving only species columns

#.mar
dat.mar.red <- dat.mar[ ,c("species","island")] 
dat.mar.red$presence <- 1 
dat.mar.pa <- dat.mar.red %>%
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.mar.pa)))
names(list0) <- names(dat.mar.pa)
dat.mar.pa <- as.data.frame(dat.mar.pa %>% replace_na(list0))
row.names(dat.mar.pa) <- dat.mar.pa$island
dat.mar.pa <- dat.mar.pa[ , -1]


#.haw
dat.haw.red <- dat.haw[ ,c("species","island")] 
dat.haw.red$presence <- 1 
dat.haw.pa <- dat.haw.red %>%
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.haw.pa)))
names(list0) <- names(dat.haw.pa)
dat.haw.pa <- as.data.frame(dat.haw.pa %>% replace_na(list0))
row.names(dat.haw.pa) <- dat.haw.pa$island
dat.haw.pa <- dat.haw.pa[ , -1]

#.sam
dat.sam.red <- dat.sam[ ,c("species","island")] 
dat.sam.red$presence <- 1 
dat.sam.pa <- dat.sam.red %>%
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.sam.pa)))
names(list0) <- names(dat.sam.pa)
dat.sam.pa <- as.data.frame(dat.sam.pa %>% replace_na(list0))
row.names(dat.sam.pa) <- dat.sam.pa$island
dat.sam.pa <- dat.sam.pa[ , -1]

#.fij
dat.fij.red <- dat.fij[ ,c("species","island")] 
dat.fij.red$presence <- 1 
dat.fij.pa <- dat.fij.red %>%
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.fij.pa)))
names(list0) <- names(dat.fij.pa)
dat.fij.pa <- as.data.frame(dat.fij.pa %>% replace_na(list0))
row.names(dat.fij.pa) <- dat.fij.pa$island
dat.fij.pa <- dat.fij.pa[ , -1]

#.comb
dat.comb.red <- dat.comb[ ,c("species","island")] 
dat.comb.red$presence <- 1 
dat.comb.pa <- dat.comb.red %>%
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.comb.pa)))
names(list0) <- names(dat.comb.pa)
dat.comb.pa <- as.data.frame(dat.comb.pa %>% replace_na(list0))
row.names(dat.comb.pa) <- dat.comb.pa$island
dat.comb.pa <- dat.comb.pa[ , -1]
