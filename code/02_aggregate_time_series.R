## aggregate time series with time windows windows and store in a list -----

# Get MTBs for each time window
# stored as list due to varying vector size in each time window
MTB.list <- lapply(1:nowi,        
                   function(i){
                     st <- skp*(i-1) # start time of time window
                     st2 <- wsize + 1 + st
                     # select data for data window, with starting time st+1 and window size: wsize
                     # below we take the modulus %% (mod maxDay) to get data for a full year
                     if(st2<=maxDay){
                       ndafa <- dafa[dafa$doy > st & dafa$doy < st2,]
                     } else {
                       ndafa <- rbind(dafa[dafa$doy > st,],dafa[dafa$doy < st2%%maxDay,])
                     }
                     ndafa <- droplevels(ndafa) # use info for this specific time window, delete other data
                     # create list item = MTBs of each time window
                     MTBresult <- sort(unique(ndafa$NAME))
                     MTBresult # output
                   }
)

###############################################
# Find common MTB list over all time windows

comMTB <- MTB.list[[1]]
for (i in c(2:nowi)) {
  comMTB <- intersect(comMTB, MTB.list[[i]])
}
comMTB<-sort(comMTB)

##############################################
# Create list of presence-only data frame for each time window
# stored as list due to varying data frame size in each time window
# computation time: approx. 15 min
datawin.list <- lapply(1:nowi,        
                       function(i){
                         st <- skp*(i-1) # starting time of time window
                         st2 <- wsize + 1 + st
                         # select data for data window, with starting time st+1 and window size: wsize
                         if(st2<=maxDay){
                           ndafa <- dafa[dafa$doy > st & dafa$doy < st2,] # for 
                         } else {
                           ndafa <- rbind(dafa[dafa$doy > st,],dafa[dafa$doy < st2%%maxDay,])
                         }
                         ndafa <- droplevels(ndafa) # delete level info
                         # INFO: data frames retain info of all the entries, even when excluded, 
                         # e.g. species that are not in a species time window but in the original dataframe.
                         # When creating frequency tables all species from the original data frame are listed as columns names
                         # even if they are not elements of the time window.
                         ndafa <- dplyr::select(ndafa, -doy) # keep only MTB and species, unsorted
                         y <- table(ndafa$NAME, ndafa$species) # create frequency table for each time window, is automatically sorted
                         rownames(y) <- 1:length(unique(ndafa$NAME)) # change row names of frequency table
                         y[y > 0] <- 1 # convert to presence-only data
                         Winresult <- as.data.frame.matrix(y) # save table as a data frame
                         Winresult # output
                       }
)

file_name_fi <- paste("resu/winlist_wsize", wsize,"_skp", skp, ".Rdata", sep = "")
save(datawin.list, MTB.list, comMTB, file = file_name_fi)

