weekly_earnings_funct <- function(input){
        df <- data.frame(matrix(nrow=0, ncol=10))
        colnames(df) <- c("year", "period","periodName",
                          "value","change.3.month", "change.6.month", "change.12.month",
                           "pctchange.3.month", "pctchange.6.month",
                          "pctchange.12.month")
        
        alldemo <- data.frame(matrix(nrow=0, ncol=10))
        colnames(alldemo) <- c("year", "period","periodName",
                               "value","change.3.month", "change.6.month", 
                               "change.12.month","pctchange.3.month", "pctchange.6.month",
                               "pctchange.12.month")
        temp_df <- data.frame()
        
        seriesid <-  c('LEU0252881500','LEU0252881800', 'LEU0252882700', 'LEU0252883600', 
                       'LEU0252883900','LEU0252884200', 'LEU0252884500','LEU0252884800','LEU0252885100', 
                       'LEU0252885400', 'LEU0252885700','LEU0252886000', 'LEU0254468400','LEU0254468500','LEU0254468600') 
        
        group <- c("Med.Weekly.Earnings","Men.Unadj", "Women.Unadj", "White.Unadj", "White.Men.Unadj", "White.Women.Unadj", "Black.Unadj",  "Black.Men.Unadj","Black.Women.Unadj", 
                   "Hispanic.Unadj", "Hispanic.Men.Unadj", "Hispanic.Women.Unadj", "Asian.Unadj", "Asian.Men.Unadj", "Asian.Women.Unadj")
        
        
        for(x in 1:length(input)){
                temp_df <- input[[x]]
                temp_df <- temp_df[,c(-4,-6)]
                data_vector <- unlist(temp_df)
                counter <- 0
                
                for(j in 1:10){
                        
                        for(i in 1:nrow(input[[1]])){
                                counter <- counter + 1
                                df[i,j] <- data_vector[counter]
                                
                        }
                        
                        
                }
                df[,4:10] <- lapply(df[,4:10], as.numeric)
                df$group <- group[x]
                df$group.id <- seriesid[x] 
                alldemo <- rbind(alldemo, df) 
                alldemo2 <- alldemo[,c(11,12,1,2,3,4,5,6,7,8,9,10)]
        }
        sheet_write(alldemo2, ss = "145k5urPZXczvQR7isiJFxxWku_A3upqwr3kpfof9v-c", sheet = "weekly_earnings_median" )
        return(alldemo2)
}