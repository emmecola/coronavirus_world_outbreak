# INSTALL UPDATED PACKAGE
devtools::install_github("RamiKrispin/coronavirus")

# LOAD LIBRARIES
library(coronavirus)
library(dplyr)
library(ggplot2)
library(stringr)

# LOAD DATA
data("coronavirus")

# PLOT FUNCTION
plotCases <- function(dataframe,COUNTRY)
{
  filtered_covid <- coronavirus %>% filter(Country.Region==COUNTRY)
  
  df <- data.frame(date=character(), cases=character(), 
                   type=as.factor(character()),stringsAsFactors=TRUE)
  
  start <- min(filtered_covid$date)
  end <- max(filtered_covid$date)
  theDate <- start
  
  while (theDate <= end)
  {
    filtered_cum <- filtered_covid %>% filter(date<=theDate)
    d <- sum((filtered_cum %>% filter(type=="death"))$cases) # sum deaths
    r <- sum((filtered_cum %>% filter(type=="recovered"))$cases) # sum recovered
    c <- sum((filtered_cum %>% filter(type=="confirmed"))$cases) # sum confirmed
    i <- c - d - r # calculate currently infected
    
    df_temp <- data.frame(date=theDate,cases=c(i,d,r),
                          type=c("currently infected","dead","recovered"),
                          stringsAsFactors=FALSE)
    
    df <- rbind(df,df_temp)
    
    theDate <- theDate + 1
  }
  
  p <-ggplot(data=df, mapping=aes(x=date, y=cases, fill=factor(type, levels = c("recovered","currently infected","dead")))) + 
    geom_bar(stat="identity") + 
    scale_fill_manual("type", values = c("currently infected" = "sienna1", "dead" = "black", "recovered" = "royalblue")) + 
    ggtitle(paste("SARS-COV-2 in",COUNTRY,sep=" ")) + 
    scale_x_date(date_labels="%d/%m")

  ggsave(paste("plot_",str_replace(COUNTRY," ","_"),".png",sep=""), device='png', limitsize=TRUE)
  
  }

# PLOT ALL COUNTRIES
for (C in unique(coronavirus$Country.Region))
{plotCases(coronavirus,C)}
