#setting up ---------------

list.of.packages <- c("Rcpp","dplyr","RPostgreSQL","sqldf","shiny","DT","httr","rpivotTable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
options(sqldf.driver = "SQLite")

username = ''
password = ''

setwd('~')
setwd('..')
setwd(paste(getwd(),'/Genomics England Ltd/GE-Samples Team - Team Folder/Omics Tracker files',sep=''))

temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(substr(temp[i],1,nchar(temp[i])-4), read.csv(temp[i]))

# #download data from confluence------------
# confluence_get_download<- function(link) {
#   read.csv(text=content(httr::GET(paste('https://cnfl.extge.co.uk/download/attachments/135701046',link,sep=''),authenticate(username, password)),'text'))
# }
# 
# confluence_get_page<- function(link) {
#   content(httr::GET(paste('https://cnfl.extge.co.uk/rest/api/content/',link,sep=''),authenticate(username, password)))
# }
# 
# a<-confluence_get_page('135701046/child/attachment')
# 
# for(x in 1:length(a$results)){
#   assign(substr(a$results[[x]]$title,1,nchar(a$results[[x]]$title)-4), confluence_get_download(a$results[[x]]$`_links`$download))
# }

count_aliquots<-sqldf("select pid,omics_in_labkey.'Sample.Type', count(pid) as aliquots from omics_in_labkey group by pid,omics_in_labkey.'Sample.Type'")


d<-sqldf("select distinct case when used_samples.lsid is not null then 'Yes' else 'No' end as Used, omics_in_labkey.'clinic.ID' as 'Clinic ID',omics_in_labkey.PID
         as 'Participant ID', omics_in_labkey.LSID as 'Laboratory Sample ID', 
         omics_in_labkey.'Sample.Type' as 'Sample Type', disease_group as 'Disease Type',disease_sub_group as 'Disease Sub-type', aliquots as Aliquots
         from omics_in_labkey 
         left join used_samples on omics_in_labkey.lsid = used_samples.lsid
         left join disease_type on participant_id = omics_in_labkey.pid
         left join count_aliquots on count_aliquots.PID = omics_in_labkey.PID and count_aliquots.'Sample.Type' = omics_in_labkey.'Sample.Type'
         where omics_in_labkey.pid is not null
         order by omics_in_labkey.PID,omics_in_labkey.'Sample.Type', omics_in_labkey.LSID
")

d$`Sample Type`<-as.factor(d$`Sample Type`)
d$`Disease Type`<-as.factor(d$`Disease Type`)
d$`Disease Sub-type`<-as.factor(d$`Disease Sub-type`)
d$`Participant ID`<- as.character(d$`Participant ID`)
d$Used<- as.factor(d$Used)
d$`Clinic ID`<- as.factor(d$`Clinic ID`)
used_samples$LSID<- as.character(used_samples$LSID)
used_samples$UKB.Dispatch.Date<- as.Date(used_samples$UKB.Dispatch.Date, format = "%d/%m/%Y", origin="1900-01-01")
used_samples$Child.LSID<- as.character(used_samples$Child.LSID)
used_samples$Child.Volume<- as.numeric(used_samples$Child.Volume)
used_samples$Child.Concentration<- as.numeric(used_samples$Child.Concentration)
used_samples$RIN<- as.numeric(used_samples$RIN)
used_samples$Average.Fragment.Size<- as.numeric(used_samples$Average.Fragment.Size)
used_samples$OD.Ratio<- as.numeric(used_samples$OD.Ratio)


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
  ),
  tabPanel('Pivot table of all omics', rpivotTableOutput('tab5')
  )
)

server <- function(input, output) {
  
  tab<- function(tabData) {DT::renderDataTable(
    datatable( tabData, filter = 'top', extensions = c('Buttons'),
               options = list(pageLength = 25,
                              dom = 'Brtip',
                              autowidth = TRUE,
                              columnDefs = list(list(className = 'dt-center', width = '2000px', targets = "_all")),
                              buttons = c('colvis','csv'))))}
  
  output$tab1 <- tab(d)
  
  output$tab2 <- tab(used_samples)
  
  output$tab3 <- tab(samples_with_issues)
  
  output$tab4 <- tab(leftover_dna_used)
  
  output$tab5 <- rpivotTable::renderRpivotTable({
              rpivotTable(data = d)
    })
  
}

# Create Shiny app ----
shinyApp(ui, server)
