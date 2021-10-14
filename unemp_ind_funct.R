unemp_ind_funct <- function(input){
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
        
        seriesid <-  c("LNU04000000", "LNU04035109", "LNU04032229", "LNU04032230", 
                       "LNU04032231", "LNU04032232", "LNU04034154", "LNU04034163", 
                       "LNU04034168", "LNU04034171", "LNU04032237", "LNU04032238", 
                       "LNU04032239", "LNU04034236", "LNU04034239", "LNU04032241", 
                       "LNU04032242", "LNU04028615", "LNU04035181")
        
        group <- c('Unemp.Rate.16+', 'Agriculture.Unadj','Nonagriculture.Unadj', 'Mining.Unadj','Construction.Unadj',
                   'Manufacturing.Unadj','Wholesale.Trade.Unadj','Retail.Trade.Unadj', 'Transportation+Warehousings.Unadj', 'Utilities.Unadj','Information.Unadj',
                   'Financial.Activities.Unadj', 'Prof.Biz.Services.Unadj', 'Education.Unadj', 'Health.Socialassis', 'Leisure.Hospitality.Unadj', 'Other.Unadj',
                   'Government.Unadj', 'Self.Emp+Unpaid.Family.Unadj')
        
        
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
                
                temp_values <- ifelse(df$year >= 2020, ((df[df$year >=2020,'value'] - df[df$year == 2020 & df$periodName == 'February', 'value'])/df[df$year >=2020,'value']) * 100, NA)
                
                df$change.pct.feb.2020 <- round(temp_values, 1)
                
                df[df$year == 2020 & df$periodName == 'January', c('change.feb.2020', 'change.pct.feb.2020')] <- NA
                
                alldemo <- rbind(alldemo, df)
                
                alldemo2 <- alldemo[,c(13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,16,17)]
        }
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "industry_unemp_rate" )
        return(alldemo2)
}