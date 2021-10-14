labor_level_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=12))
        colnames(df) <- c("year", "period","periodName",
                          "value","change.1.month",
                          "change.3.month", "change.6.month", "change.12.month",
                          "pctchange.1.month", "pctchange.3.month", "pctchange.6.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=12))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value","change.1.month",
                               "change.3.month", "change.6.month", 
                               "change.12.month", "pctchange.1.month", 
                               "pctchange.3.month", "pctchange.6.month",
                               "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <- c('LNS11000000', 'LNS11000001', 'LNS11000002', 'LNS11000003', 
                      'LNS11000006', 'LNS11000009','LNS11032183', 'LNS11000028', 
                      'LNS11000029', 'LNS11000031', 'LNS11000032', 'LNS11000034', 'LNS11000035')
        
        group <- c("Total","Men", "Women", "White", "Black", "Hispanic", "Asian", "White.Men.20", "White.Women.20",
                   "Black.Men.20","Black Women.20","Hispanic.Men.20", "Hispanic.Women.20")
        
        
        for(x in 1:length(input)){
                temp_df <- input[[x]]
                temp_df <- temp_df[,c(-4,-6)]
                data_vector <- unlist(temp_df)
                counter <- 0
                
                for(j in 1:12){
                        
                        for(i in 1:nrow(input[[1]])){
                                counter <- counter + 1
                                df[i,j] <- data_vector[counter]
                                
                        }
                        
                        
                }
                
                df[,4:12] <- lapply(df[,4:12], as.numeric)
                
                df$group <- group[x]
                
                df$group.id <- seriesid[x]
                
                df$date <- as.Date(paste(df$year, df$periodName, '01', sep = "-"), "%Y-%B-%d")
                
                df$change.feb.2020 <- ifelse(df$year >= 2020, df[df$year >=2020,'value'] - df[df$year == 2020 & df$periodName == 'February', 'value'], NA)
                
                temp_values <- ifelse(df$year >= 2020, ((df[df$year >=2020,'value'] - df[df$year == 2020 & df$periodName == 'February', 'value'])/df[df$year == 2020 & df$periodName == 'February', 'value']) * 100, NA)
                
                df$change.pct.feb.2020 <- round(temp_values, 1)
                
                df[df$year == 2020 & df$periodName == 'January', c('change.feb.2020', 'change.pct.feb.2020')] <- NA
                
                alldemo <- rbind(alldemo, df)
                
                alldemo2 <- alldemo[,c(13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,16,17)]
        }
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "labor__force_level" )
        return(alldemo2)
}