library(shiny)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(htmlTable)
library(visNetwork)
library(markdown)
library(shinythemes)
library(tidyr)
library(tableHTML)
library(shinyWidgets)
library(dplyr)
library(data.table)
library(igraph)
library(easyPubMed)
library(slickR)
library(knitr)
library(rmarkdown)
library(shinydashboard)
library(shinydashboardPlus)
library(dqshiny)
library(shinyBS)


options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

# Main Table zone ---------------------------------------------------
# Load and modify Data
# DSN <- read.csv("./data/new_net_info_smaller.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
DSN <- fread("./data/new_net_info_V7_pval.csv",header = TRUE)  %>% as.data.frame()

drugLinks = read.csv("./data/drug links.csv",header = TRUE, sep=",", stringsAsFactors = F)
drugMetaData = read.csv("./data/DB_full_meta_data.csv",header = TRUE, sep = ",", stringsAsFactors = F) 

# get drug1 names for their IDs
DB <- drugMetaData[which(drugMetaData[,1] %in% as.vector(unique(DSN[,1]))), c(1,2)] # drugMetaData[,1]: ID1; DSN[,1]: ID1; c(1,2): ID1, Name1  
colnames(DB) <- c("ID1","name1")

sider_Data <- fread("./data/drugBank_Sider_mapping.csv",header = TRUE) %>% as.data.frame()
statistics.dat.1 <- fread("./data/drugSimDB_statistics_table1.csv",header = TRUE) %>% as.data.frame()
download.dat <- fread("./data/download_table.csv",header = TRUE) %>% as.data.frame()

download.info <- data.frame(
  File = paste0("<b>",download.dat$File,"</b>"),
  Description = paste0("<i>",download.dat$Description,"</i>"),
  
  Link = paste0("<a href=\"",download.dat$Link,"\">",download.dat$Format, " [", download.dat$Size,"]","</a>")
)

# reactive values -------
mytabPP <- reactiveValues(Value = NULL)
mytabP <- reactiveValues(Value = NULL)
mytabSN <- reactiveValues(Value = NULL)
mytabSN_nodes <- reactiveValues(Value = NA)
mytabSN_edges <- reactiveValues(Value = NA)
druglist <- reactiveValues(Value = NULL)

selectorUIMolView <- function(id){
  ns = NS(id)
  
  tags$div(
    fluidRow(
      column(12, uiOutput(ns('new_uiMolview')))
    ),
    id = paste0('uiMolview', id)
  )
}

selectorUIPP <- function(id){
  ns = NS(id)
  
  tags$div(
    fluidRow(
      column(12, uiOutput(ns('new_uiPhP')))
    ),
    id = paste0('uiPhP', id)
  )
}

selectorUIP <- function(id){
  ns = NS(id)
  
  tags$div(
    fluidRow(
      column(12, uiOutput(ns('new_uiP')))
    ),
    id = paste0('uiP', id)
  )
}

selectorMolViewServer <- function(input, output, session){
  ns = session$ns
  drugName <- ns("txt") %>% gsub("-txt","",.,fixed = T)
  # print(drugName)
  pubChemID = drugLinks[which(drugLinks$Name == drugName), 7]
  # print(pubChemID)
  
  # render all the molview structures of query drug``
  output$new_uiMolview <- render_dq_box_group({
    dq_accordion(ns("foo"),
                 titles = ns("input") %>% gsub("-input","",.,fixed = T), 
                 contents = list(
                   # textInput(inputId = ns("txt"), label = "")
                   tags$iframe(src=paste0("https://embed.molview.org/v1/?mode=balls&cid=",pubChemID), style="width: 500px; height: 300px;")
                 ),
                 bg_color = NULL, 
                 options = list(animate = 200, collapsible = T),
                 # icons = c(open = "hand-point-down", closed = "hand-point-right")
                 icons = c(rotate = "angle-right")
                 
    )
  })
}

selectorPPServer <- function(input, output, session){
  ns = session$ns
  # drugName <- ns("txt") %>% gsub("-txt","",.,fixed = T)
  
  output$new_uiPhP <- render_dq_box_group({
    dq_accordion(ns("foo_PhP"),
                 titles = ns("input") %>% gsub("-input","",.,fixed = T), 
                 contents = list(
                   tags$div(
                     tableHTML(mytabPP$Value, collapse = 'separate', spacing = '2px 8px') %>%
                       add_css_header(css = list(c('height', 'background-color'), c('30px', ' #e6e6e6')),
                                      headers = 1:12) %>%
                       add_css_row(css = list('background-color', '#ecf0f1'),
                                   rows = odd(1:17)) %>%
                       add_css_row(css = list('background-color', '#ffffff'),
                                   rows = even(1:17)) %>%
                       add_css_column(list('padding-left', '12px'), columns = c(0,1)) %>%
                       add_css_header(list('padding-left', '12px'), headers = c(2)) %>%
                       add_css_column(list('font-weight', 'bold'), columns = c(0))
                   )
                 ),
                 bg_color = NULL, 
                 options = list(animate = 200, collapsible = T),
                 # icons = c(open = "hand-point-down", closed = "hand-point-right")
                 icons = c(rotate = "angle-right")
    )
  })
  
}

selectorPServer <- function(input, output, session){
  ns = session$ns
  
  # render Pharmacology property of a drug
  output$new_uiP <- render_dq_box_group({
    dq_accordion(ns("foo_P"),
                 titles = ns("input") %>% gsub("-input","",.,fixed = T), 
                 contents = list(
                   tags$div(
                     tableHTML(mytabP$Value, collapse = 'separate', spacing = '2px 8px') %>%
                       add_css_header(css = list(c('height', 'background-color'), c('30px', ' #e6e6e6')),
                                      headers = 1:12) %>%
                       add_css_row(css = list('background-color', '#ecf0f1'),
                                   rows = odd(1:17)) %>%
                       add_css_row(css = list('background-color', '#ffffff'),
                                   rows = even(1:17)) %>%
                       add_css_column(list('padding-left', '12px'), columns = c(0,1)) %>%
                       add_css_header(list('padding-left', '12px'), headers = c(2)) %>%
                       add_css_column(list('font-weight', 'bold'), columns = c(0))
                   )
                 ),
                 bg_color = NULL, 
                 options = list(animate = 200, collapsible = T),
                 # icons = c(open = "hand-point-down", closed = "hand-point-right")
                 icons = c(rotate = "angle-right")
                 
    )
  })
}

processMetaData <- function(aDrug){
  # print(aDrug)
  # print(class(aDrug))
  metaData <- drugMetaData[drugMetaData[,1] == aDrug,]
  # print(head(metaData))
  # renaming 
  metaData["LogP1_source"] <- paste0("(",metaData["LogP1_source"],")")
  metaData["LogP2_source"] <- paste0("(",metaData["LogP2_source"],")")
  metaData["pKa..strongest.acidic."] <- paste0(metaData["pKa..strongest.acidic."]," (strongest acidic)")
  metaData["pKa..strongest.basic."] <- paste0(metaData["pKa..strongest.basic."]," (strongest basic)")
  
  # Cleaning
  newTargets <- strsplit(as.character(metaData["Target.names"]),",")
  newTargets <- as.data.frame(newTargets[[1]])
  metaData["Target.names"] <- newTargets[which(newTargets[,1] != 'NA'),] %>% paste0(collapse = "; ")
  
  newCarriers <- strsplit(as.character(metaData["Carrier.names"]),",")
  newCarriers <- as.data.frame(newCarriers[[1]])
  metaData["Carrier.names"] <- newCarriers[which(newCarriers[,1] != 'NA'),] %>% paste0(collapse = "; ")
  
  newEnzymes <- strsplit(as.character(metaData["Enzyme.names"]),",")
  newEnzymes <- as.data.frame(newEnzymes[[1]])
  metaData["Enzyme.names"] <- newEnzymes[which(newEnzymes[,1] != 'NA'),] %>% paste0(collapse = "; ")
  
  newTransporters <- strsplit(as.character(metaData["Transporter.names"]),",")
  newTransporters <- as.data.frame(newTransporters[[1]])
  metaData["Transporter.names"] <- newTransporters[which(newTransporters[,1] != 'NA'),] %>% paste0(collapse = "; ")
  
  
  
  metaData <- tidyr::unite(metaData, "LogP1", c("LogP1","LogP1_source"),sep=" ",remove=T) %>% 
    tidyr::unite("LogP2", c("LogP2","LogP2_source"),sep=" ",remove=T) %>% 
    tidyr::unite("LogP", c("LogP1","LogP2"),sep="; ",remove=T) %>% 
    tidyr::unite("pKa", c("pKa..strongest.acidic.","pKa..strongest.basic."),sep="; ",remove=T)
  
  colnames(metaData) <- gsub("\\."," ", colnames(metaData)) # remove "." within the meta data columns
  
  return(metaData)
}

processSimilarityData <- function(session, input, aDrugName = "", aDrugID = "", isBatchMode = F){
  
  if(input$S_or_P_or_AdjP == "Scores")
    DSN = dplyr::select(DSN,-c("p_value","adjP_value"))
  else if(input$S_or_P_or_AdjP == "p-value")
    DSN = dplyr::select(DSN,-c("rowMeans","adjP_value"))
  else
    DSN = dplyr::select(DSN,-c("rowMeans","p_value"))
  
  if(!isBatchMode | length(druglist$Value) == 1){
    # single drug processing ------
    
    mytabSN$Value <<- if(!isBatchMode) as.data.frame(unique(DSN[which(DSN$ID1 == aDrugID),-c(1)])) else
                                        as.data.frame(unique(DSN[which(DSN$ID1 == druglist$Value),-c(1)]))
    colnames(mytabSN$Value) = c("id",  "Structure Similarity", "Target Similarity", 
                                "Pathway Similarity","GO_CC Similarity", "GO_MF Similarity",
                                "GO_BP Similarity", "Score")
    
    # get the meta-info for the interacting drugs
    metaInfo = drugMetaData[which(drugMetaData[,1] %in% mytabSN$Value[,1]),c(1,2,3)] # (1:ID, 2:Name, and 3:Group)
    metaInfo[,3] = gsub("\\,",", ", metaInfo[,3])
    colnames(metaInfo) <- c("id","name","Groups")
    
    mytabSN$Value = as.data.frame(dplyr::inner_join(mytabSN$Value, metaInfo,by = "id")) 
    mytabSN$Value <- cbind(mytabSN$Value["name"],
                           mytabSN$Value["Groups"],
                           mytabSN$Value["Structure Similarity"],
                           mytabSN$Value["Target Similarity"],
                           mytabSN$Value["Pathway Similarity"],
                           mytabSN$Value["GO_CC Similarity"],
                           mytabSN$Value["GO_MF Similarity"],
                           mytabSN$Value["GO_BP Similarity"],
                           mytabSN$Value["Score"]
    )
    colnames(mytabSN$Value)[1] = "Target"
    
    if(input$S_or_P_or_AdjP == "Scores")
      colnames(mytabSN$Value)[9] <- "Scores" 
    else if(input$S_or_P_or_AdjP == "p-value")
      colnames(mytabSN$Value)[9] <- "p-value"
    else
      colnames(mytabSN$Value)[9] <- "Adj. p-value"
    
    cols <- c(3:9)
    mytabSN$Value[,cols] <- apply(mytabSN$Value[,cols],2,function(x) as.numeric(as.character(x)))
    
    mytabSN$Value <- as.data.frame(mytabSN$Value[order(mytabSN$Value[,9]),])
    
    # 1.2 Network View 
    net_nodes = data.frame(name = c(aDrugName,unique(as.character(mytabSN$Value$Target))))
    net_nodes = drugMetaData[which(drugMetaData[,2] %in% net_nodes[,1]),c(1,2,3)]
    net_nodes[,3] = gsub("\\,",", ", net_nodes[,3]) # colnames are: id, name, Groups
    colnames(net_nodes) <- c("id","name","Groups")

    net_nodes = dplyr::left_join(net_nodes,sider_Data,by=c("id"="DrugBank ID"))
    
    net_edges = dplyr::inner_join(DSN[,c(1,2,9)], net_nodes[,c("id","name")], by = c("ID1"="id")) #cols: ID1, ID2, p_value, name
    colnames(net_edges)[4] = "Name1"
    
    net_edges = dplyr::inner_join(net_edges, net_nodes[,c("id","name")], by=c("ID2"="id")) #cols: ID1, ID2, p_value, Name1, name
    colnames(net_edges)[5] = "Name2"
    
    net_edges = dplyr::select(net_edges, c(4,5,3))
    
    mytabSN_nodes$Value <<- data.frame(id = net_nodes$name,
                                       name = net_nodes$name,
                                       label = net_nodes$name,
                                       title = paste0("<p>Drug: <b>",net_nodes$name,"</b>",
                                                      "<p>Status: <b>",net_nodes$Groups,"</b>",
                                                      "<p>Side-effects: <b><br>", net_nodes$SE.NAME1),
                                       group = net_nodes$Groups)
    
    
    mytabSN_edges$Value <<- data.frame(from=net_edges[,"Name1"],
                                       to=net_edges[,"Name2"],
                                       value = 1 - as.numeric(net_edges[,3]))
    
    mytabSN_edges$Value[1:2] = t(apply(mytabSN_edges$Value[1:2],1,sort))
    mytabSN_edges$Value = mytabSN_edges$Value[!duplicated(mytabSN_edges$Value[1:2]),]
    
    mytabSN_edges$Value$id = 1:nrow(mytabSN_edges$Value)
    # ------
  }
  else{
    # multi-drug processing -------
    
    mytabSN$Value <<- as.data.frame(unique(DSN[which(DSN$ID1 %in% druglist$Value & DSN$ID2 %in% druglist$Value),]))
    
    if(nrow(mytabSN$Value) == 0){
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "No data found for this query...",
        type = "error"
      )
      return(NA)
    }
    colnames(mytabSN$Value) = c("id1","id2",  "Structure Similarity", "Target Similarity", 
                                "Pathway Similarity","GO_CC Similarity", "GO_MF Similarity",
                                "GO_BP Similarity", "Score")
    
    print(head(mytabSN$Value))
    # get the meta-info for the interacting drugs
    metaInfo = drugMetaData[which(drugMetaData[,1] %in% druglist$Value),c(1,2,3)] # (1:ID, 2:Name, and 3:Group)
    metaInfo[,3] = gsub("\\,",", ", metaInfo[,3])
    colnames(metaInfo) <- c("id1","name1","Groups1")
    
    mytabSN$Value = as.data.frame(dplyr::inner_join(mytabSN$Value, metaInfo,by = "id1")) 
    colnames(metaInfo) <- c("id2","name2","Groups2")
    mytabSN$Value = as.data.frame(dplyr::inner_join(mytabSN$Value, metaInfo,by = "id2")) 
    
    mytabSN$Value <- cbind(mytabSN$Value["name1"],
                           mytabSN$Value["name2"],
                           mytabSN$Value["Groups1"],
                           mytabSN$Value["Groups2"],
                           mytabSN$Value["Structure Similarity"],
                           mytabSN$Value["Target Similarity"],
                           mytabSN$Value["Pathway Similarity"],
                           mytabSN$Value["GO_CC Similarity"],
                           mytabSN$Value["GO_MF Similarity"],
                           mytabSN$Value["GO_BP Similarity"],
                           mytabSN$Value["Score"]
    )
    colnames(mytabSN$Value)[1] = "Query"
    colnames(mytabSN$Value)[2] = "Target"
    # print(head(mytabSN$Value))
    # remove duplicate pairs
    mytabSN$Value[1:2] = t(apply(mytabSN$Value[1:2],1,sort))
    mytabSN$Value = mytabSN$Value[!duplicated(mytabSN$Value[1:2]),]
    # print(head(mytabSN$Value))
    if(input$S_or_P_or_AdjP == "Scores")
      colnames(mytabSN$Value)[11] <- "Scores" 
    else if(input$S_or_P_or_AdjP == "p-value")
      colnames(mytabSN$Value)[11] <- "p-value"
    else
      colnames(mytabSN$Value)[11] <- "Adj. p-value"
    
    cols <- c(5:11)
    mytabSN$Value[,cols] <- apply(mytabSN$Value[,cols],2,function(x) as.numeric(as.character(x)))
    
    mytabSN$Value <- as.data.frame(mytabSN$Value[order(mytabSN$Value[,11]),])
    
    
    net_nodes = data.frame(name = druglist$Value)
    net_nodes = drugMetaData[which(drugMetaData[,1] %in% net_nodes[,1]),c(1,2,3)]
    print(net_nodes)
    
    net_nodes[,3] = gsub("\\,",", ", net_nodes[,3]) # colnames are: id, name, Groups
    colnames(net_nodes) <- c("id","name","Groups")
    net_nodes = dplyr::left_join(net_nodes,sider_Data,by=c("name"="DRUG.NAME"))
    
    net_edges = dplyr::inner_join(DSN[,c(1,2,9)], net_nodes[,c("id","name")], by = c("ID1"="id")) #cols: ID1, ID2, p_value, name
    colnames(net_edges)[4] = "Name1"
    
    net_edges = dplyr::inner_join(net_edges, net_nodes[,c("id","name")], by=c("ID2"="id")) #cols: ID1, ID2, p_value, Name1, name
    colnames(net_edges)[5] = "Name2"
    
    net_edges = dplyr::select(net_edges, c(4,5,3))
    
    mytabSN_nodes$Value <<- data.frame(id = net_nodes$name,
                                       name = net_nodes$name,
                                       label = net_nodes$name,
                                       title = paste0("<p>Drug: <b>",net_nodes$name,"</b>",
                                                      "<p>Status: <b>",net_nodes$Groups,"</b>",
                                                      "<p>Side-effects: <b><br>", net_nodes$SE.NAME1),
                                       group = net_nodes$Groups)
    
    
    mytabSN_edges$Value <<- data.frame(from=net_edges[,"Name1"],
                                       to=net_edges[,"Name2"],
                                       value = 1 - as.numeric(net_edges[,3]))
    
    mytabSN_edges$Value[1:2] = t(apply(mytabSN_edges$Value[1:2],1,sort))
    mytabSN_edges$Value = mytabSN_edges$Value[!duplicated(mytabSN_edges$Value[1:2]),]
    
    mytabSN_edges$Value$id = 1:nrow(mytabSN_edges$Value)
    # ------
  }

}

# UI -----------------
ui <- fluidPage(
  navbarPage(title = "DrugSimDB", footer = "This work has been developed by A. K. M. Azad and Fatemeh Vafaee. 
             Copyright reserved.", position = "fixed-top", collapsible=T, 
             theme = shinytheme("flatly"),
             
             
             tabPanel("Home", icon = icon("home", lib = "font-awesome"),
                      
                      tags$style(type="text/css", "body {padding-top: 70px;}"),
                      tags$style(HTML("
                                      #first {
                                      border-radius: 5px; border: 1px solid #293954; padding-left: 15px; padding-right: 15px;
                                      }
                                      #sld {
                                          border-radius: 5px; border: 0px solid #293954; height: 500px; margin-top: 60px; 
                                      }
                                      #spread {
                                          text-align: justify;
                                      }
                                    ")),
                      tags$br(),
                      fluidRow(id = "first",
                               column(width=2,
                                      tags$br(),
                                      tags$br(),
                                      HTML("<div style=\" display: table; \"><img src=\"logo.png\",  style=\"height: 100px; width:150px; border-radius: 5px; \"></img></div>")
                                      # slickROutput("slick_Slideshow", width = '95%', height = '20px')
                               ),
                               column(width = 8, 
                                      fluidRow(column(id= "spread",width=12,
                                                      includeMarkdown("./data/description.md"))
                                      ),
                                      fluidRow(
                                        column(id = "sld", width=12,
                                               # slickROutput("slick_Slideshow", width = '95%', height = '20px')
                                               slickROutput("slick_Slideshow")  %>% withSpinner()
                                        )
                                      )
                               )
                               
                      ),tags$br(),
                      
             ),
             
             tabPanel("Search", icon = icon("search", lib = "font-awesome"),
                      tags$style(HTML("
                        #first {
                          border-radius: 5px; border: 1px solid #293954; padding-left: 15px; padding-right: 15px;
                        }
                        #second {
                          border-radius: 5px;border: 1px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                        #fd {
                          border-radius: 5px;border: 1px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                        #fh {
                          border-radius: 5px;border: 1px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                        #fc {
                          border-radius: 5px;border: 1px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                      ")),
                      tags$br(),
                      
                      fluidRow(id = "second", 
                               tags$script("$(document).on('click', '#contents button', function () {
                                            Shiny.onInputChange('select_button',this.id);
                                            });"),
                               column(12,
                                      
                                      # fluidRow(
                                      #   # strong("Search your drug name and click View:")
                                      # ),
                                      tags$br(),
                                      fluidRow(
                                        sidebarLayout(
                                          sidebarPanel(
                                            bsCollapse(open = 1, multiple = T,
                                                        bsCollapsePanel(title = strong("Search a drug name and click View:"), 
                                                                        value = 1, 
                                                                        style = 'primary',
                                                                        radioGroupButtons(
                                                                          inputId = "S_or_P_or_AdjP",
                                                                          choices = c("Scores","p-value","Adj. p-value"),
                                                                          justified = T,
                                                                          size = "xs",
                                                                          selected = "Adj. p-value"
                                                                        ),
                                                                        DT::dataTableOutput("contents") %>% withSpinner()
                                                        ),
                                                        # strong("OR"),
                                                        # bsCollapse(id = "b",
                                                        bsCollapsePanel(title = strong("Multi-drug query"), 
                                                                        value = 1,
                                                                        style = 'info',
                                                                        fluidRow(column(width = 8,
                                                                                        fileInput("file1", "Upload a list of drugs (CSV):", 
                                                                                                  multiple = FALSE,
                                                                                                  accept = c("text/csv",
                                                                                                             "text/comma-separated-values,text/plain",
                                                                                                             ".csv"))
                                                                        ),
                                                                        column(width = 4, 
                                                                               actionButton("uploadbtn", label = "Process", icon = icon(name = "paper-plane", lib = 'font-awesome')),
                                                                               tags$style(type='text/css', "#uploadbtn {margin-top: 25px;}")
                                                                               
                                                                        )
                                                                        ),
                                                                        tags$a(href="https://ndownloader.figshare.com/files/22437203","Example #1"),
                                                                        tags$a(href="https://ndownloader.figshare.com/files/22437200","Example #2"),
                                                                        tags$a(href="https://ndownloader.figshare.com/files/22437206","Example #3")
                                                                        
                                                        )
                                            )
                                            
                                          ),
                                          # sidebarPanel(strong("OR")),
                                          mainPanel(
                                            # TBD
                                            tabsetPanel(
                                              id = 'DrugInfo',
                                              tabPanel("Similarity Network", 
                                                       tags$br(),
                                                       
                                                       bsCollapse(id = "drugView", multiple = T,
                                                                  bsCollapsePanel(
                                                                    title = "NetworkView", style = 'warning',
                                                                    visNetworkOutput("SNView", height = '600px')  %>% withSpinner(),
                                                                    actionBttn(inputId = "popOver", style = "material-circle", icon(name = "question", lib = 'font-awesome'))
                                                                  ),
                                                                  
                                                                  bsCollapsePanel(
                                                                    title = "TableView", style = 'success',
                                                                    DT::dataTableOutput("tabSN")
                                                                  )
                                                       )
                                                       
                                                       
                                              ),
                                              # tabPanel("Physicochemical Properties", htmlOutput("tabPP")),
                                              tabPanel("Physicochemical Properties", 
                                                       # tableHTML_output("tabPP"),
                                                       tags$div(id = 'tabPP_placeholder'), # for the tabPP renderer
                                                       tags$br()
                                              ),
                                              tabPanel("Structure", 
                                                       tags$br(),
                                                       # htmlOutput("S_drugName"),
                                                       tags$div(id = 'molview_placeholder'), # for the molview renderer
                                                       
                                                       tags$br(),
                                                       # htmlOutput("S_periodicTable")),
                                                       actionButton("showP", "Show Molview Periodic Table")),
                                              # tabPanel("Pharmacology", htmlOutput("tabP"))
                                              tabPanel("Pharmacology", 
                                                       # tableHTML_output("tabP"),
                                                       tags$div(id = 'tabP_placeholder'), # for the tabP renderer
                                                       tags$br()
                                              )
                                            )
                                          )
                                        )
                                      )
                                      
                                      
                               )
                      )
             ),
             tabPanel("Download", icon = icon("download", lib = "font-awesome"),
                      tags$br(),
                      fluidRow(id='fd',
                               column(12,
                                      fluidRow(column(12,
                                                      includeMarkdown("./data/Download.md"))),
                                      fluidRow(column(12,
                                                      # tableHTML_output("tableDownload"))),
                                                      DT::dataTableOutput("tableDownload")  %>% withSpinner())),
                                      
                                      
                                      tags$br()
                               )
                      )
                      
             ),
             tabPanel("Statistics", icon = icon("chart-pie", lib = "font-awesome"),
                      tags$br(),
                      fluidRow(id='fd',
                               column(12,
                                      fluidRow(column(12,
                                                      includeMarkdown("./data/Statistics.md"))),
                                      fluidRow(column(12,
                                                      tableHTML_output("tableStat")  %>% withSpinner())),
                                      
                                      tags$br(),
                                      tags$br()
                               )
                      )
             ),
             tabPanel("Help", icon = icon("chalkboard-teacher", lib = "font-awesome"),
                      tags$br(),
                      fluidRow(id='fd',
                               column(12,
                                      includeHTML("./data/tutorial.html"))
                               # shiny::includeMarkdown("./data/tutorial.Rmd"))
                               # shiny::includeHTML("./data/tu")
                               # uiOutput('markdown'))
                               
                      )
             ),
             
             # tabPanel("Version history",
             #          tags$br(),
             #          fluidRow(id='fd',
             #                   column(12,
             #                          includeMarkdown("./data/demo.Rmd"))
             #                   # includeHTML("https://github.com/Akmazad/Drug-Repositioning/blob/master/README.md"))
             #          )
             # ),
             tabPanel("Contact", icon = icon("address-card", lib = "font-awesome"),
                      tags$br(),
                      fluidRow(id='fd',
                               column(12,
                                      includeMarkdown("./data/Contact.md"))
                      )
             )
  )
)

# server --------------------
server <- function(input, output, session) {
  # reactive values -----------------
  currentDrugName <- reactiveValues(Value = NULL)
  
  mytabS <- reactiveValues(Value = NULL)
  mytabS_periodicTable <- reactiveValues(Value = tags$img(src='molview_periodic_table.png', height='50%', width='50%'))
  pubChemID <- reactiveValues(Value = NULL)
  
  
  # file upload --------------------
  observeEvent(input$uploadbtn, {
    req(input$file1)
    tryCatch(
      {
        removeUI(
          selector = "div[id^='uiMolview']", multiple = T
        )
        removeUI(
          selector = "div[id^='uiPhP']", multiple = T
        )
        removeUI(
          selector = "div[id^='uiP']", multiple = T
        )
        # ------
        
        tempDrugList <- fread(input$file1$datapath, stringsAsFactors = T, header = F) %>% 
          as.data.frame()
        tempDrugList <- tempDrugList[,1] %>% as.character()
        
        drugIDlist <- c()
        for(i in 1:length(tempDrugList)){
          tempLine = tempDrugList[i]
          if(grepl("DB[0-9\\(\\)]+", tempLine)){
            drugID = tempLine
            drugName = drugLinks[which(drugLinks[,1] == drugID),2]
          }else{
            drugName = tempLine
            drugID = drugLinks[which(drugLinks[,2] == drugName),1]
          }
          
          # Molview ---------
          insertUI(
            selector = "#molview_placeholder",
            ui = selectorUIMolView(drugName)
          )
          callModule(selectorMolViewServer, drugName)
          # -------
          
          # drugID = drugLinks[which(drugLinks[,2] == drugName),1]
          drugIDlist = c(drugIDlist, drugID)
          
          metaData <- processMetaData(drugID)
          # print(head(metaData))
          
          # PP -------
          mytabPP$Value <<- t(as.data.frame(metaData[,c(4:19)]))
          colnames(mytabPP$Value) = paste("Drug Name:", drugName)
          
          insertUI(
            selector = "#tabPP_placeholder",
            ui = selectorUIPP(drugName)
          )
          callModule(selectorPPServer, drugName)
          # -------
          
          # P ------- 
          mytabP$Value <<- t(as.data.frame(metaData[,c(20:35)]))
          colnames(mytabP$Value) = paste("Drug Name:", drugName)
          
          insertUI(
            selector = "#tabP_placeholder",
            ui = selectorUIP(drugName)
          )
          callModule(selectorPServer, drugName)
          # -------
          
        }
        druglist$Value <- drugIDlist
        processSimilarityData(session, input, NA, NA, isBatchMode = T)
        druglist$Value <- drugLinks[match(druglist$Value, drugLinks$DrugBank.ID),2]
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })  
  #---------------
  # some control variable
  maxPubmed = 20
  
  gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
  
  
  # text filling zone
  # -------------------------------------------------------------------
  
  # Click-Event handling zone -----------------------------------------
  # drug name panel population -----------------
  re_df <- reactiveValues(data = data.frame(
    GenericName = DB$name1,
    Actions = paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary view" id=',DB$ID1,'>View</button>
             </div>
             '),
    stringsAsFactors = FALSE
  ))
  
  # Render DrugList
  output$contents <- DT::renderDataTable({
    re_df$data <- as.data.frame(re_df$data)[order(as.numeric(re_df$data[,1])),]
    DT::datatable(re_df$data, selection = 'none', rownames = FALSE, escape = FALSE, extensions = 'Scroller', 
                  options = list(deferRender = TRUE, scrollY = 500, scroller = TRUE))
  })
  
  # Search DrugList as you type
  observeEvent(input$page, {
    dataTableProxy("contents") %>% selectPage(input$page)
  })
  # -------
  
  
  
  # Render DSN
  observeEvent(input$select_button, {
    
    # Current Drug Name
    currentDrugName$Value = DB[which(DB$ID1 == input$select_button),"name1"] # 2: Drug Name
    
    processSimilarityData(session, input, currentDrugName$Value, input$select_button)
    
    # get and pre-process metadata -----
    metaData <- processMetaData(input$select_button)
    # -----
    
    # 4. Populate Pharmacology tab  ----------------
    mytabP$Value <<- t(as.data.frame(metaData[,c(20:35)]))
    colnames(mytabP$Value) = paste("Drug Name:", currentDrugName$Value)
    
    removeUI(
      selector = "div[id^='uiP']", multiple = T
    )
    
    insertUI(
      selector = "#tabP_placeholder",
      ui = selectorUIP(currentDrugName$Value)
    )
    callModule(selectorPServer, currentDrugName$Value)
    
    # 2. Populate Physiological Properties -------------
    mytabPP$Value <<- t(as.data.frame(metaData[,c(4:19)]))
    colnames(mytabPP$Value) = paste("Drug Name:", currentDrugName$Value)
    removeUI(
      selector = "div[id^='uiPhP']", multiple = T
    )
    
    insertUI(
      selector = "#tabPP_placeholder",
      ui = selectorUIPP(currentDrugName$Value)
    )
    callModule(selectorPPServer, currentDrugName$Value)
    # -------------------------------------
    
    # 3. Populate Structure tab --------------
    
    pubChemID = drugLinks[which(drugLinks$DrugBank.ID == input$select_button), 7]
    # pubChemID = NA
    if(!is.na(pubChemID)){
      pubChemID = as.character(pubChemID)
      # output$S_drugName = renderUI(strong(paste("Drug Name:",currentDrugName$Value)))
      # mytabS$Value = tags$iframe(src=paste0("https://embed.molview.org/v1/?mode=balls&cid=",pubChemID), style="width: 500px; height: 300px;")
      
      # remove old ones --------------
      # if(!is.null(druglist$Value)){
      #   if(length(druglist$Value) > 0){
      #     for(i in 1: length(druglist$Value)){
      #       print(paste0("to be deleted: ",druglist$Value[i]))
      #       removeUI(
      #         selector = paste0("#ui", druglist$Value[i])
      #       )
      #     }
      #   }
      # }
      # if(!is.null(currentDrugName$Value)){
      #   print(paste0("to be deleted: ",currentDrugName$Value))
      #   
      #   removeUI(
      #     selector = paste0("#ui", currentDrugName$Value)
      #   )
      # }
      removeUI(
        selector = "div[id^='uiMolview']", multiple = T
      )
      # 
      # ------
      
      
      insertUI(
        selector = "#molview_placeholder",
        ui = selectorUIMolView(currentDrugName$Value)
      )
      callModule(selectorMolViewServer, currentDrugName$Value)
      
    }else{
      mytabS$Value = "PubChem ID is not available for this drug!!"
    }
    # ------------------------
    
    # -----------------------------
    
  })
  
  output$slick_Slideshow <- renderSlickR({
    imgs <- list.files("www/slider images/", pattern = ".png", full.names = T)
    
    # slickR(imgs, width = '100%',height = '475px') +
    slickR(slick_div(imgs,
                     css = htmltools::css(
                       border_radius = '5px',  border = '1px solid #293954',
                       marginLeft = "auto", marginRight = "auto",
                       width = '100%', height = '400px'
                     )
    )
    ) +
      
      settings(autoplay = T, autoplaySpeed = 1500, respondTo = 'slider', dots = T, arrows = F)
  }
  # , env = parent.frame()
  )
  
  # 1. 
  output$tabSN <- DT::renderDataTable({
    mytabSN$Value$`Structure Similarity` = round(mytabSN$Value$`Structure Similarity`, 2)
    mytabSN$Value$`Target Similarity` = round(mytabSN$Value$`Target Similarity`, 2)
    mytabSN$Value$`Pathway Similarity` = round(mytabSN$Value$`Pathway Similarity`, 2)
    mytabSN$Value$`GO_CC Similarity` = round(mytabSN$Value$`GO_CC Similarity`, 2)
    mytabSN$Value$`GO_MF Similarity` = round(mytabSN$Value$`GO_MF Similarity`, 2)
    mytabSN$Value$`GO_BP Similarity` = round(mytabSN$Value$`GO_BP Similarity`, 2)
    mytabSN$Value[, ncol(mytabSN$Value)] = round(mytabSN$Value[, ncol(mytabSN$Value)], 2)
    
    DT::datatable(mytabSN$Value, rownames = F, escape = FALSE, extensions = c('Buttons','Responsive'), options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'pdf', 'print'),
      
      # following javascript forcefully converts doubles into 
      # scientific number
      # credit: https://stackoverflow.com/a/49635311/11400614
      rowCallback = JS(
        "function(row, data) {",
        "for (i = 1; i < data.length; i++) {",
        "if (data[i]<0.01 & data[i]!= 0){",
        "$('td:eq('+i+')', row).html(data[i].toExponential(1));",
        "}",
        "}",
        "}")
    ))
    
  })
  
  # ----------------- Network visualization
  # Few improvement notes: 
  # 1) Network nodes are drugs
  # 2) Network edges selection: A side panel (next to the net) should list
  #     a collection of pubmed literatures that mentions those drug-pairs (R package for pubmed search)
  updateCollapse(session = session, id = "drugView", open = "NetworkView")
  addPopover(session = session, 
             id="popOver", 
             title = "Tips", 
             placement = "top", 
             trigger = 'click',
             content = includeHTML("data/networkLegend.html"),
             # content = includeMarkdown("data/networkLegend.md"),
             
             options = list(
               html = T,
               boundary = 'window',
               width = '100%',
               animation = T
             ))
  output$SNView <- renderVisNetwork({
    if(!is.na(mytabSN_nodes$Value) && !is.na(mytabSN_edges$Value)){
      visNetwork(mytabSN_nodes$Value, mytabSN_edges$Value) %>%
        visIgraphLayout(layout = "layout_with_graphopt",
                        physics = F,
                        smooth = F,
                        type = "full",
                        randomSeed = 123) %>%
        visExport() %>%
        visOptions(highlightNearest = list(enabled =TRUE, hover = T), nodesIdSelection = TRUE, selectedBy = "id") %>%
        visOptions(selectedBy = "group") %>%
        # visOptions(collapse = list(enabled = TRUE, clusterOptions = list(shape = "square")))  %>%
        visInteraction(navigationButtons = TRUE) %>%
        
        visEvents(selectEdge = "function(properties) {
                                  Shiny.onInputChange('current_edges_selection', properties.edges);
                                }"
        ) 
    }
  })
  # observe({
  #   print(input$current_node_selection)
  #   
  #   if(!is.null(input$current_node_selection)){
  #     print(input$current_node_selection)
  #   }
  # })
  
  observe({
    # print("i'm here")
    if(!is.null(input$current_edges_selection)){
      edgeIDs = as.numeric(input$current_edges_selection)
      # print(edgeIDs)
      edgeList = lapply(edgeIDs, FUN = function(x){
        return(mytabSN_edges$Value[which(mytabSN_edges$Value[,4]==x),c("from","to")])
      }) 
      # print(edgeList)
      selectedDrugs = unique(unlist(edgeList, use.names = F))
      
      # -- Make Pubmed seach
      new_query <- paste0(paste0(selectedDrugs, collapse = "[All Fields] AND "), "[All Fields]")
      notif_id <- showNotification(paste0("Pumbed query: ", paste0(selectedDrugs, collapse = ", ")), type = "message", duration = NULL)
      
      dami_on_pubmed <- get_pubmed_ids(new_query)
      removeNotification(notif_id)
      
      # only if any search results found
      nResults = as.numeric(dami_on_pubmed$Count)
      if(nResults > 0){
        notif_id <- showNotification(paste0("Pubmed query: ", dami_on_pubmed$Count," articles found. Fetching data"), type = "message", duration = NULL)
        
        if(nResults > maxPubmed){
          
          dami_abstracts_xml <- fetch_pubmed_data(dami_on_pubmed, retstart = 1, retmax = maxPubmed)
          removeNotification(notif_id)
          notif_id <- showNotification(paste0("Pubmed query: processing results [top ",maxPubmed,"]"), type = "message", duration = NULL)
        }
        else{
          dami_abstracts_xml <- fetch_pubmed_data(dami_on_pubmed)
          removeNotification(notif_id)
          notif_id <- showNotification(paste0("Pubmed query: processing results"), type = "message", duration = NULL)
          
        }
        dami_abstracts_list <- articles_to_list(dami_abstracts_xml)
        
        # process the search results
        pmArticleList = lapply(dami_abstracts_list, function(x){
          x1 = article_to_df(pubmedArticle = x, autofill = FALSE)
          article_title = paste0("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/",x1$pmid[1], "\">", x1$title[1],"</a>")
          pmid_text = paste0("<a href=\"https://www.ncbi.nlm.nih.gov/pubmed/",x1$pmid[1], "\">PMID: ", x1$pmid[1],"</a>")
          return(paste0(article_title, "<br>",
                        paste0(paste(x1$firstname, x1$lastname), collapse = ","), "<br>",
                        x1$jabbrv[1], " ", x1$year[1], ", doi: ", x1$doi[1], "<br>",
                        pmid_text))
        })
        pmArticleList = data.frame(Article = unlist(pmArticleList))
        output$tbl_PubmedSearch = renderDataTable(pmArticleList, 
                                                  selection='single', rownames = F, escape=F,
                                                  options = list(lengthChange = FALSE
                                                  ))
        
        removeNotification(notif_id)
        notif_id <- showNotification(paste0("Pubmed query: processing done"), type = "message", duration = NULL)
        
        showModal(modalDialog( h5(paste0("Pubmed Search: ", new_query)),div(DT::dataTableOutput('tbl_PubmedSearch'), style= "font-size:80%"), size = "l"))
      }
      else{
        notif_id <- showNotification(paste0("Pubmed query: No data found"), type = "error", duration = NULL)
      }
      removeNotification(notif_id)
    }
  })
  output$tabPP <- render_tableHTML({
    HTML(
      tableHTML(mytabPP$Value, collapse = 'separate', spacing = '2px 8px') %>%
        add_css_header(css = list(c('height', 'background-color'), c('30px', ' #e6e6e6')),
                       headers = 1:12) %>%
        add_css_row(css = list('background-color', '#ecf0f1'),
                    rows = odd(1:17)) %>%
        add_css_row(css = list('background-color', '#ffffff'),
                    rows = even(1:17)) %>%
        add_css_column(list('padding-left', '12px'), columns = c(0,1)) %>%
        add_css_header(list('padding-left', '12px'), headers = c(2)) %>%
        add_css_column(list('font-weight', 'bold'), columns = c(0))
    )
  })
  output$tableDownload <- DT::renderDataTable({
    # DT::datatable(download.info, selection = 'none', rownames = FALSE, escape = FALSE, extensions = 'Scroller', 
    #               options = list(bFilter = F, paging = F, deferRender = F, scrollY = 500, scroller = F))
    DT::datatable(download.info, selection = 'none', rownames = FALSE, escape = FALSE, 
                  options = list(paging = F, deferRender = F))
    
  })
  output$tableStat <- render_tableHTML({
    HTML(
      tableHTML(statistics.dat.1, collapse = 'separate', spacing = '2px 8px', escape = F, rownames = F) %>%
        add_css_header(css = list(c('height', 'background-color'), c('30px', ' #e6e6e6')),
                       headers = 1:11) %>%
        add_css_row(css = list('background-color', '#ecf0f1'),
                    rows = odd(1:11)) %>%
        add_css_row(css = list('background-color', '#ffffff'),
                    rows = even(1:11)) %>%
        add_css_column(list('padding-left', '12px'), columns = c(0,1,2,3,4)) %>%
        add_css_header(list('padding-left', '12px'), headers = c(1,2,3,4)) %>%
        add_css_column(list('font-weight', 'bold'), columns = c(0))
    )
  })
  # output$frameS <- renderUI({
  # 
  #   mytabS$Value
  # })
  observeEvent(input$showP, {
    showModal(modalDialog(
      tags$img(src='molview_periodic_table.png', height='100%', width='100%'), size = 'l', easyClose = T
    ))
  })
  
  # 4.
  output$tabP <- render_tableHTML({
    HTML(
      tableHTML(mytabP$Value, collapse = 'separate', spacing = '2px 8px') %>%
        add_css_header(css = list(c('height', 'background-color'), c('30px', ' #e6e6e6')),
                       headers = 1:12) %>%
        add_css_row(css = list('background-color', '#ecf0f1'),
                    rows = odd(1:17)) %>%
        add_css_row(css = list('background-color', '#ffffff'),
                    rows = even(1:17)) %>%
        add_css_column(list('padding-left', '12px'), columns = c(0,1)) %>%
        add_css_header(list('padding-left', '12px'), headers = c(2)) %>%
        add_css_column(list('font-weight', 'bold'), columns = c(0))
    )
  })
  
  # output$markdown <- renderUI({
  #   HTML(markdown::markdownToHTML(knit('data/tutorial.Rmd', quiet = TRUE, envir = parent.frame())))
  # })
}

shinyApp(ui, server)

# Possible New features:
# 1. Clickable links for the target Drug names in the right-panel
#    - when clicked, the left DT is filtered with that drugname
#       and the right panel updated automatically. 
# May not be possible: pairs aren't symmetric

# Buglist
# 1. click node, selects all the neighborhood of degree 1, turn off this

# # deployment issue:
# library(BiocManager)
# options(repos = BiocManager::repositories())