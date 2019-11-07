#setting up ---------------

list.of.packages <- c("Rcpp","dplyr","RPostgreSQL","sqldf","shiny","DT","httr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
options(sqldf.driver = "SQLite")


#download data from confluence------------
confluence_get_download<- function(link) {
  read.csv(text=content(httr::GET(paste('https://cnfl.extge.co.uk/download/attachments/135701046',link,sep=''),authenticate('jwarrin', '9Jcgbfw2&')),'text'))
}

confluence_get_page<- function(link) {
  content(httr::GET(paste('https://cnfl.extge.co.uk/rest/api/content/',link,sep=''),authenticate('jwarrin', '9Jcgbfw2&')))
}

a<-confluence_get_page('135701046/child/attachment')

for(x in 1:length(a$results)){
  assign(substr(a$results[[x]]$title,1,nchar(a$results[[x]]$title)-4), confluence_get_download(a$results[[x]]$`_links`$download))
  }

d<-sqldf("select case when used_samples.lsid is not null then 'Yes' else 'No' end as Used, omics_in_labkey.*, used_samples.'Project',
         used_samples.'Destination', used_samples.'UKB.Dispatch.Date', used_samples.'Child.Sample.Type' 
         from omics_in_labkey 
         left join used_samples on omics_in_labkey.lsid = used_samples.lsid")

#ui and server-------
ui <- navbarPage(
  title = 'Omics Tracker',
  tabPanel('All omics samples', DT::dataTableOutput('tab1')
           ),
  tabPanel('Used samples', DT::dataTableOutput('tab2')
           ),
  tabPanel('Samples with issues', DT::dataTableOutput('tab3')
  ),
  tabPanel('Leftover DNA used', DT::dataTableOutput('tab4')
  )
)

server <- function(input, output) {
  
  tab<- function(tabData) {DT::renderDataTable(
    datatable( tabData, filter = 'top', extensions = c('Buttons'),
               options = list(pageLength = 25,
                              dom = 'Bfrtip',
                              autowidth = TRUE,
                              columnDefs = list(list(className = 'dt-center', width = '2000px', targets = "_all")),
                              buttons = c('colvis','csv'))))}
  
  output$tab1 <- tab(d)
  
  output$tab2 <- tab(used_samples)
  
  output$tab3 <- tab(samples_with_issues)
  
  output$tab4 <- tab(leftover_dna_used)
  
}

# Create Shiny app ----
shinyApp(ui, server)