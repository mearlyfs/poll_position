
library(tidyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(stringr)
library(readxl)
library(ggplot2)
library(directlabels)
library(readr)
library(RColorBrewer)
library(viridis)

####### SAVANTA

get_savanta_table <- function(url,date){

  # Function to read a standardly-formatted Savanta polling spreadsheet from the internet, and return a usable dataset
  #  args: url - the URL to the spreadsheet download
  #        date - the date the poll took place, as a string, "dd-mm-yyyy"
  
  fielddate <- date

  tempfile <- tempfile()
  download.file(url,tempfile,method = "auto",mode="wb")
  savanta_contents <- read_xlsx(tempfile)
  n_tabs <- savanta_contents[,1][[1]] %>% na.omit %>% unique %>% length
  n_tabs = n_tabs + 1

  # Start a blank dataset
  dataset <- data.frame(date=NA_character_,question=NA_character_)[0,]   # select row zero and the set of existing columns to get a blank dataset

  for(i in 2:n_tabs){
    print(paste("reading & processing tab",i))
    table_raw <- suppressMessages(read_xlsx(tempfile,sheet=i,skip=0 ))

    # Only proceed if it is a fully-formatted table (if so, it will have an unweighted and weighted total)
    if(length(grep("Unweighted Total",table_raw$`<< Contents`))>0|length(grep("Unweighted row",table_raw$`<< Contents`))>0){

      firstrow <- which(substr(table_raw$`<< Contents`,1,4)=="BASE")
      question_text <- table_raw$`<< Contents`[firstrow-1]
      
      # First get rid of possible blank columns on the left, and possible last row that only contains the attribution
      if(sum(is.na(table_raw[,1]))==nrow(table_raw)){
        table_raw <- table_raw[,-1]
      }

      # Delete the last row if it is just text
      if(is.na(table_raw[nrow(table_raw),2])){
        table_raw <- table_raw[-nrow(table_raw),]
      }

      # The spreadsheet is not formatted for nice machine reading. In its raw format, we need to copy the table labels:
      for(r in 2:nrow(table_raw)){
        if(is.na(table_raw[r,1])&!is.na(table_raw[r,2])&!is.na(table_raw[r-1,1])){
          table_raw[r,1] <- paste0(table_raw[r-1,1],"__percent")
        }
      }

      for(c in 2:ncol(table_raw)){
        if(is.na(table_raw[firstrow+1,c])&!is.na(table_raw[firstrow+1,c-1])){
          table_raw[firstrow+1,c] <- table_raw[firstrow+1,c-1]
        }
      }

      # Extract the table and reshape into a long format:
      table <- table_raw[(firstrow+4):nrow(table_raw),1:ncol(table_raw)]
      colnames(table)[1] <- "response"
      colnames(table)[2:ncol(table)] <- paste0(table_raw[firstrow+1,2:ncol(table_raw)],"__",table_raw[firstrow+2,2:ncol(table_raw)])
      table %<>% pivot_longer(!response,values_to="value",names_to="breakdown")
      table %<>% mutate(question=question_text)

      table %<>% separate(col=breakdown,into=c("breakdown","category"),sep="__")
      table %<>% mutate(breakdown=if_else(breakdown=="NA","Total",breakdown))

      table %<>% separate(col=response,into=c("response","valuetype"),sep="__",fill="right")
      table %<>% mutate(valuetype=if_else(is.na(valuetype),"number",valuetype))

      # Now that we have the table in the correct format, finish cleaning:
      table %<>% filter(!is.na(response))
      table %<>% filter(!substr(response,1,14)=="Columns Tested")
      table %<>% mutate(value=as.numeric(value))

      table %<>% pivot_wider(values_from=value,names_from=valuetype)

      # Add this question to the main dataset of question responses
      dataset %<>% bind_rows(table)
    }
  }

  dataset %<>% mutate(date=dmy(fielddate))

  return(dataset)

}

# Until scraping is set up, copy and paste URLs by hand. Download all the polls in the last few months:

sav1 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/04/Omni_W202_VI_Post_Tables_20240408_Private.xlsx",date="06-04-2024")
sav2 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/04/SunVI_PoliticalPolling_Final_Weighted_Tables_20240328_Private.xlsx",date="26-03-2024")
sav3 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/Omni_W201_MPP_Post_V2_Tables_Private.xlsx",date="23-03-2024")
sav4 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/Omni_W200_Party_PR_Tables_20240319_Private.xlsx",date="16-03-2024")
sav5 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/P037330-KA-Fraud-Victims-Tables.xlsx",date="27-02-2024")
sav6 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/Omni_W200_VI_Post_Private.xlsx",date="16-03-2024")
sav7 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/Omni_W200_Telegraph_PR_Tables_20240319_Private.xlsx",date="16-03-2024")
sav8 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/Omni_W199_VI_Post_Tables_Private.xlsx",date="09-03-2024")
sav9 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/Omni_W198_IWD_V2_tables_Private.xlsx",date="27-02-2024")
sav10 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W183_PH_Private.xlsx",date="04-11-2024")
sav11 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/03/Omni_W198_VI_Post_tables_Private.xlsx",date="02-03-2024")
sav12 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W197_IPR_Private.xlsx",date="24-02-2024")
sav13 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W197_VI_Post_Tables_20240227_Private.xlsx",date="24-02-2024")
sav14 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W196_LabourPR_tables_Private.xlsx",date="17-02-2024")
sav15 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W196_StrikesPR_tables_Private.xlsx",date="17-02-2024")
sav16 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W196_MPP_Post_tables_Private.xlsx",date="17-02-2024")
sav17 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W195_VI_Post_Private.xlsx",date="10-02-2024")
sav18 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W194_EconomicPolicy_PR_tables_Private.xlsx",date="03-02-2024")
sav19 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/02/Omni_W194_LFF_PR_Tables_Private.xlsx",date="03-02-2024")
sav20 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W193_VI_Post_tables_Private.xlsx",date="27-01-2024")
sav21 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W193_Economy_Tables_Private.xlsx",date="27-01-2024")
sav22 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W193_Covid_tables_Private.xlsx",date="27-01-2024")
sav24 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W192_MPP_Post_Private.xlsx",date="20-01-2024")
sav25 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W192_Internal_PR_Tables_20240123_Private.xlsx",date="20-01-2024")
sav26 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W191_VI_Post_Tables_Private.xlsx",date="13-01-2024")
sav27 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W190_Internal_PR_Private.xlsx",date="06-01-2024")
sav28 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W190_Republic_Tables_20240108_Private.xlsx",date="06-01-2024")
sav29 <- get_savanta_table("https://savanta.com/wp-content/uploads/2024/01/Omni_W189_BBC5Live_tables_Private.xlsx",date="16-12-2023")
sav30 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/12/Omni_W189_VI_Post_tables_Private.xlsx",date="16-12-2023")
sav31 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/12/Omni_W188_MPP_Post_Tables_20231213_Private.xlsx",date="09-12-2023")
sav32 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/12/Omni_W187_VI_Post_Tables_Private.xlsx",date="02-12-2023")
sav33 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W186_VI_Post_Private.xlsx",date="25-11-2023")
sav34 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W186_Republic_Tables_20231127_Private.xlsx",date="02-12-2023")
sav35 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W177_Healthwatch_Tables_20231124_Private.xlsx",date="23-09-2023")
sav36 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W185_MPP_Post_tables_Private.xlsx",date="18-11-2023")
sav37 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W184_HomelessAndPolicePR_tables_Private.xlsx",date="11-11-2023")
sav23 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W183_LFF_Private.xlsx",date="04-11-2023")
sav38 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W183_PH_Private.xlsx",date="04-11-2023")
sav39 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/10/Omni_W180_MPP_Post_Tables_20231018_Private.xlsx",date="14-10-2023")
sav40 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/11/Omni_W183_PH_Private.xlsx",date="04-11-2023")
sav41 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/09/Omni_W176_MPP_Post_tables_Private.xlsx",date="16-09-2023")
sav42 <- get_savanta_table("https://savanta.com/wp-content/uploads/2023/09/Omni_W173_MPP_Post_Tables_20230830_Private.xlsx",date="26-08-2023")

sav <- bind_rows(sav1,sav2,sav3,sav4,sav5,sav6,sav7,sav8,sav9,sav10,sav11,sav12,sav13,sav14,sav15,sav16,sav17,sav18,sav19,sav20,sav21,sav22,
                 sav23,sav24,sav25,sav26,sav27,sav28,sav29,sav30,sav31,sav32,sav33,sav34,sav35,sav36,sav37,sav38,sav39,sav40,sav41,sav42)

# Some general changes. Make the question neutral on year, for instance
sav %<>% mutate(question=str_replace_all(question,"2023","YYYY"))
sav %<>% mutate(question=str_replace_all(question,"2024","YYYY"))

sav %<>% mutate(category=str_replace_all(category," (a)",""),
                category=str_replace_all(category," (b)",""),
                category=str_replace_all(category," (c)",""),
                category=str_replace_all(category," (d)",""),
                category=str_replace_all(category," (e)",""),
                category=str_replace_all(category," (f)",""),
                category=str_replace_all(category," (g)",""),
                category=str_replace_all(category," (h)",""),
                category=str_replace_all(category," (i)",""),
                category=str_replace_all(category," (j)",""),
                category=str_replace_all(category," (k)",""),
                category=str_replace_all(category," (l)",""))

# Get rid of the Savanta ID
sav %<>% mutate(question=substr(question,str_locate(question," ")+1,100000))

# Make IDs and a cut-down version of the question for the sake of finding
sav %<>% mutate(question_short=question,
                question_short=str_replace_all(question_short," and "," "),
                question_short=str_replace_all(question_short," if "," "),
                question_short=str_replace_all(question_short," it "," "),
                question_short=str_replace_all(question_short," the "," "),
                question_short=str_replace_all(question_short," were "," "),
                question_short=str_replace_all(question_short," there "," "),
                question_short=str_replace_all(question_short," a "," "),
                question_short=str_replace_all(question_short," that "," "),
                question_short=str_replace_all(question_short," do "," "),
                question_short=str_replace_all(question_short," for "," "),
                question_short=str_replace_all(question_short," you "," "),
                question_short=str_replace_all(question_short," what "," "),
                question_short=str_replace_all(question_short," has "," "),
                question_short=substr(question_short,1,175))

# Attach a key to the questions:
questions <- sav %>% select(question,question_short) %>% unique
questions %<>% mutate(key=row_number())
sav %<>% full_join(questions)

write_csv(questions,"D:/Savanta questions.csv")
write_csv(sav,"D:/Savanta questions and data.csv")

# Check which questions have a time series:
question_dates <- sav %>% select(question_short,date) %>% unique
question_dates %<>% group_by(question_short) %>% mutate(n=n()) %>% filter(n()>1)

###### DRAW TEST GRAPHS WITH SAVANTA DATA

test_question <- "Healthcare: To what extent are you optimistic or pessimistic about the following issues in YYYY?"
test_response <- "Sum: Optimistic"

# OVERALL
  test_graph_data <- sav %>% filter(!response%in%c("Total","Unweighted Total","Unweighted row","Don't know")) %>%
                             filter(question==test_question) %>%
                             filter(breakdown=="Total",response==test_response) 
  test_graph <- test_graph_data %>% 
                  ggplot(aes(x=date,y=percent)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + 
                  theme_minimal() + labs(title=paste0("Savanta","\n",test_question,"\n",test_response),y="",x="") + scale_x_date(date_labels="%b %Y")

# BY AGE
  test_graph_data <- sav %>% filter(!response%in%c("Total","Unweighted Total","Unweighted row","Don't know")) %>%
                              filter(question==test_question,response==test_response) %>%
                              filter(breakdown%in%c("Age","Total"),response==test_response,!substr(category,1,3)%in%c("Net","NET"))
  test_graph_data_cols <- c("#000000",brewer.pal(9,"Set1")[c(1:5,7:7)])
  names(test_graph_data_cols) <- unique(c("Total",unique(test_graph_data$category)))  # manipulate to that "Total appears first in the vector
  
  test_graph <- test_graph_data %>% 
                ggplot(aes(x=date,y=percent,col=category)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + geom_dl(aes(label=category),method="last.bumpup") +
                theme_minimal() + labs(title=paste0("Savanta","\n",test_question,"\n",test_response),y="",x="") + scale_x_date(date_labels="%b %Y") + 
                scale_colour_manual(values = test_graph_data_cols,guide="none") + coord_cartesian(xlim=c(dmy("15-08-2023"),dmy("01-04-2024"))) 
  test_graph2 <- test_graph + facet_grid(category~.,)

  
# BY SEX
  test_graph_data <- sav %>% filter(!response%in%c("Total","Unweighted Total","Unweighted row","Don't know")) %>%
    filter(question==test_question,response==test_response) %>%
    filter(breakdown%in%c("Gender","Total"),response==test_response,!substr(category,1,3)%in%c("Net","NET"))
  
  test_graph_data_cols <- c("#000000",brewer.pal(9,"Set1")[c(1:5,7:7)])
  names(test_graph_data_cols) <- unique(c("Total",unique(test_graph_data$category)))  # manipulate to that "Total appears first in the vector
  
  test_graph <- test_graph_data %>% 
    ggplot(aes(x=date,y=percent,col=category)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + geom_dl(aes(label=category),method="last.bumpup") +
    theme_minimal() + labs(title=paste0("Savanta","\n",test_question,"\n",test_response),y="",x="") + scale_x_date(date_labels="%b %Y") + 
    scale_colour_manual(values = test_graph_data_cols,guide="none") + coord_cartesian(xlim=c(dmy("15-08-2023"),dmy("01-04-2024")))
  test_graph2 <- test_graph + facet_grid(category~.,)
  
# BY REGION
  
  test_graph_data <- sav %>% filter(!response%in%c("Total","Unweighted Total","Unweighted row","Don't know")) %>%
    filter(question==test_question,response==test_response) %>%
    filter(breakdown%in%c("Region","Total"),response==test_response,!substr(category,1,3)%in%c("Net","NET"))
  
  test_graph_data %<>% mutate(category=factor(category,levels=c("North-East","North-West","Yorkshire & Humberside","East Midlands","West Midlands","Eastern","London","South-East","South-West","Wales","Scotland","Northern Ireland","Total")))
  
  test_graph_data_cols <- c("#000000",brewer.pal(9,"Set1")[c(1:5,7:7)],brewer.pal(8,"Set2"))
  names(test_graph_data_cols) <- unique(c("Total",levels(test_graph_data$category)))  # manipulate to that "Total appears first in the vector
  
  test_graph <- test_graph_data %>% 
    ggplot(aes(x=date,y=percent,col=category)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + geom_dl(aes(label=category),method="last.bumpup") +
    theme_minimal() + labs(title=paste0("Savanta","\n",test_question,"\n",test_response),y="",x="") + scale_x_date(date_labels="%b %Y") + 
    scale_colour_manual(values = test_graph_data_cols,guide="none") + coord_cartesian(xlim=c(dmy("30-08-2023"),dmy("30-04-2024")))
  test_graph2 <- test_graph + facet_grid(category~.,)
  
# BY SOCIOECONOMIC
  
  test_graph_data <- sav %>% filter(!response%in%c("Total","Unweighted Total","Unweighted row","Don't know")) %>%
    filter(question==test_question,response==test_response) %>%
    filter(breakdown%in%c("Social grade","Total"),response==test_response,!substr(category,1,3)%in%c("Net","NET"))
  
  test_graph_data_cols <- c("#000000",brewer.pal(9,"Set1")[c(1:5,7:7)],brewer.pal(8,"Set2"))
  names(test_graph_data_cols) <- unique(c("Total",unique(test_graph_data$category)))  # manipulate to that "Total appears first in the vector
  
  test_graph <- test_graph_data %>% 
    ggplot(aes(x=date,y=percent,col=category)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) + geom_dl(aes(label=category),method="last.bumpup") +
    theme_minimal() + labs(title=paste0("Savanta","\n",test_question,"\n",test_response),y="",x="") + scale_x_date(date_labels="%b %Y") + 
    scale_colour_manual(values = test_graph_data_cols,guide="none") + coord_cartesian(xlim=c(dmy("30-08-2023"),dmy("01-04-2024")))
  test_graph2 <- test_graph + facet_grid(category~.,)
  
####### More In Common

get_MIC_table <- function(url,date){

  fielddate <- date

  tempfile <- tempfile()
  download.file(url,tempfile,method = "auto",mode="wb")
  MIC_contents <- read_xlsx(tempfile)

  # They don't say the number of tabs. Let's guess 30 and catch errors

  # Start a blank dataset
  dataset <- data.frame(date=NA_character_,question=NA_character_)[0,]   # select row zero and the set of existing columns to get a blank dataset

  for(i in 2:30){
    print(paste("reading & processing tab",i))
    tryCatch({

      # Section of code if the sheet exists

      table_raw <- suppressMessages(read_xlsx(tempfile,sheet=i,skip=0))
      colnames(table_raw)[1] <- "col1"
      question_text <- table_raw$col1[1]

        firstrow <- 1

        # First get rid of possible blank columns on the left, and possible last row that only contains the attribution
        if(sum(is.na(table_raw[,1]))==nrow(table_raw)){
          table_raw <- table_raw[,-1]
        }

        # Delete the last row if it is just text
        if(is.na(table_raw[nrow(table_raw),4])){
          table_raw <- table_raw[-nrow(table_raw),]
        }

        # The spreadsheet is not formatted for nice machine reading. In its raw format, we need to copy the table labels:
        for(c in 2:ncol(table_raw)){
          if(is.na(table_raw[firstrow+2,c])&!is.na(table_raw[firstrow+2,c-1])){
            table_raw[firstrow+2,c] <- table_raw[firstrow+2,c-1]
          }
        }

        # Extract the table and reshape into a long format:
        table <- table_raw[(firstrow+4):nrow(table_raw),1:ncol(table_raw)]
        colnames(table)[1] <- "response"
        colnames(table)[2:ncol(table)] <- paste0(table_raw[firstrow+2,2:ncol(table_raw)],"__",table_raw[firstrow+3,2:ncol(table_raw)])
        table %<>% pivot_longer(!response,values_to="value",names_to="breakdown")
        table %<>% mutate(question=question_text)

        table %<>% separate(col=breakdown,into=c("breakdown","category"),sep="__")
        table %<>% mutate(breakdown=if_else(breakdown=="NA","Total",breakdown))
        table %<>% mutate(breakdown=if_else(category=="All","Total",category))

        table %<>% mutate(valuetype=if_else(response%in%c("Unweighted N","Weighted N"),"number","percent")) # More In Common only reports percentages or totals

        # Now that we have the table in the correct format, finish cleaning:
        table %<>% filter(!is.na(response))
        table %<>% filter(!response=="Weight")
        table %<>% mutate(value=as.numeric(value))

        table %<>% pivot_wider(values_from=value,names_from=valuetype)

        # Add this question to the main dataset of question responses
        dataset %<>% bind_rows(table)

    },
    error=function(e){
      print(paste("sheet",i,"does not exist"))
      return(NULL)
    })
  }

  dataset %<>% mutate(date=dmy(fielddate))

  return(dataset)

}

mic1 <- get_MIC_table("https://www.moreincommon.org.uk/media/bd2p00vj/jan-big-issues-and-vi.xlsx",date="28-01-2024")
mic2 <- get_MIC_table("https://www.moreincommon.org.uk/media/pm3lpnfy/jan-2024-vi-top-issues.xlsx",date="10-01-2024")



















