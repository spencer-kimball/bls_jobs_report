unemp_dur_funct <- function(input){
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
        
        seriesid <-  c("LNU03008275", "LNU03008307", "LNU03017227", "LNU03034306", 
                        "LNU03022433", "LNU03008311", "LNU03008349", "LNU03017229", 
                        "LNU03034313", "LNU03034327", "LNU03008354", "LNU03008392",
                        "LNU03017231", "LNU03034320", "LNU03034334", "LNU03008275")
        
        group <- c('Total.16+','White','Black', 'Asian', 'Hispanic','Men', 'White.Men', 'Black.Men', 
                   'Asian.Men','Hispanic.Men', 'Women', 'White.Women','Black.Women', 'Asian.Women',
                   'Hispanic.Women')
        
        
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
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "unemp_duration" )
        return(alldemo2)
}