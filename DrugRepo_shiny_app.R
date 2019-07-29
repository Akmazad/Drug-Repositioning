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

options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

ui <- fluidPage(
  
  navbarPage("Drug Repositioning", footer = "Copyright @VafaeeLab, 2019", theme = shinytheme("flatly"), 
             tabPanel("Home",
                      tags$style(HTML("
                        #first {
                          border-radius: 5px; border: 2px solid #293954; padding-left: 15px; padding-right: 15px;
                        }
                        #second {
                          border-radius: 5px;border: 2px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                        #fd {
                          border-radius: 5px;border: 2px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                        #fh {
                          border-radius: 5px;border: 2px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                        #fc {
                          border-radius: 5px;border: 2px solid #293954; align: bottom; padding-left: 15px; padding-right: 15px;
                        }
                      ")),
                      # 
                      fluidRow(id = "first", 
                               column(width=3,
                                      # tags$img(src="Drug rep.jpg", height="100%", width="100%", align='middle')),
                                      HTML("<div style=\" display: table; \"><img src=\"Drug rep.jpg\",  style=\"height: 100%; width:100%; \"></img></div>")),
                                      # div(style="width: 100%; height: 100%; display: table; text-align: center;",div(style="display: table-cell; vertical-align: middle;", img(src="Drug rep.jpg", height="50%", width="50%")))),
                               column(width=9, 
                                      includeMarkdown("./data/description.md"))
                      ),
                      fluidRow(
                        tags$br()
                      ),
                      fluidRow(id = "second", 
                               tags$script("$(document).on('click', '#contents button', function () {
                                            Shiny.onInputChange('select_button',this.id);
                                            });"),
                               column(12,
                                      fluidRow(
                                        tags$br()
                                      ),
                                      fluidRow(
                                        strong("Search your drug name and click View:")
                                      ),
                                      fluidRow(
                                      sidebarLayout(
                                        sidebarPanel(
                                                     radioGroupButtons(
                                                       inputId = "S_or_P_or_AdjP",
                                                       choices = c("Scores","p-value","Adj. p-value"),
                                                       justified = T,
                                                       size = "xs",
                                                       selected = "Adj. p-value"
                                                     ),
                                                     DT::dataTableOutput("contents") %>% withSpinner()
                                        ),
                                        mainPanel(
                                          # TBD
                                          tabsetPanel(
                                            id = 'DrugInfo',
                                            tabPanel("Similarity Network", 
                                                     tags$br(),
                                                     htmlOutput("tableView"),
                                                     tags$br(),
                                                     DT::dataTableOutput("tabSN"),
                                                     tags$br(),
                                                     tags$br(),
                                                     htmlOutput("networkView"),
                                                     visNetworkOutput("SNView")  %>% withSpinner()
                                            ),
                                            # tabPanel("Physicochemical Properties", htmlOutput("tabPP")),
                                            tabPanel("Physicochemical Properties", tableHTML_output("tabPP")),
                                            tabPanel("Structure", 
                                                     tags$br(),
                                                     htmlOutput("S_drugName"),
                                                     htmlOutput("frameS"),
                                                     tags$br(),
                                                     # htmlOutput("S_periodicTable")),
                                                     actionButton("showP", "Show Molview Periodic Table")),
                                            # tabPanel("Pharmacology", htmlOutput("tabP"))
                                            tabPanel("Pharmacology", tableHTML_output("tabP"))
                                          )
                                        )))
                      ))
             ),
             tabPanel("Download",
                      fluidRow(id='fd',
                               column(12,
                                      fluidRow(
                                        column(2, "Full Drug-similarity table", align="center"),
                                        
                                        column(10,
                                               downloadButton("downloadDSN", "Download"))
                                        # DT::dataTableOutput("download_table") %>% withSpinner())
                                      ),
                                      tags$hr(),
                                      fluidRow(
                                        column(2, "Drugbank Metadata", align="center"),
                                        
                                        column(10,
                                               downloadButton("downloadDB", "Download"))
                                        # DT::dataTableOutput("download_table") %>% withSpinner())
                                      )))
                      
                              
             ),
             tabPanel("Help",
                      fluidRow(id='fd',
                               column(12,
                                      includeMarkdown("./data/Contact.md"))
                                      # includeHTML("https://github.com/Akmazad/Drug-Repositioning/blob/master/README.md"))
                      )
             ),
             tabPanel("Contact",
                      fluidRow(id='fd',
                               column(12,
                                      includeMarkdown("./data/Contact.md"))
                      )
             )
  )
)

server <- function(input, output, session) {
  # Download DSN
  output$downloadDSN <- downloadHandler(
    filename = function() {
      paste("Full drug similarity table", ".csv", sep = "")
    },
    content = function(file) {
      # file.copy("./data/new_net_info.csv", file)
      file.copy("./data/net-information - Copy.csv", file)
    }
  )
  # Download DB metadata
  output$downloadDB <- downloadHandler(
    filename = function() {
      paste("Full Drugbank Metadata table", ".csv", sep = "")
    },
    content = function(file) {
      file.copy("./data/DB_full_meta_data.csv", file)
    }
  )
  
  gicon <- function(x) as.character(icon(x, lib = "glyphicon"))

  
  # text filling zone
  # Main Table zone ---------------------------------------------------
  # Load and modify Data
  # DSN <- read.csv("./data/new_net_info_smaller.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  DSN <- read.csv("./data/new_net_info_V3_pval.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  
  drugLinks = read.csv("./data/drug links.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  drugMetaData = read.csv("./data/DB_full_meta_data.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  
  # get drug1 names for their IDs
  DB <- drugMetaData[which(drugMetaData[,1] %in% as.vector(unique(DSN[,1]))), c(1,2)] # drugMetaData[,1]: ID1; DSN[,1]: ID1; c(1,2): ID1, Name1  
  colnames(DB) <- c("ID1","name1")
  
  
  # -------------------------------------------------------------------
  
  # Click-Event handling zone -----------------------------------------
  currentDrugName <- reactiveValues(Value = NULL)
  mytabSN <- reactiveValues(Value = NULL)
  mytabSN_nodes <- reactiveValues(Value = NA)
  mytabSN_edges <- reactiveValues(Value = NA)
  
  mytabPP <- reactiveValues(Value = NULL)
  
  mytabS <- reactiveValues(Value = NULL)
  mytabS_periodicTable <- reactiveValues(Value = tags$img(src='molview_periodic_table.png', height='50%', width='50%'))
  pubChemID <- reactiveValues(Value = NULL)
  
  mytabP <- reactiveValues(Value = NULL)
  
  
  
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
  

  # Render DSN
  observeEvent(input$select_button, {
    if(input$S_or_P_or_AdjP == "Scores")
      DSN = DSN[,-c(7,8)] 
    else if(input$S_or_P_or_AdjP == "p-value")
      DSN = DSN[,-c(6,8)]
    else
      DSN = DSN[,-c(6,7)]
    # Current Drug Name
    currentDrugName$Value = DB[which(DB$ID1 == input$select_button),2] # 2: Drug Name
    
    # 1. Populate Similarity Tab ------------------------------------------
    # 1.1 Table View
    output$tableView <- renderUI(strong("Table View:"))
    # get interacting Drug2s for the current drug
    mytabSN$Value <<- as.data.frame(unique(DSN[which(DSN$ID1 == input$select_button),-c(1)]))
    # colnames(mytabSN$Value) = c("id",  "Structure Similarity", "Target Similarity", 
    #                             "Pathway Similarity", "RowMeans", "P.Value", "adj.P.value")
    
    colnames(mytabSN$Value) = c("id",  "Structure Similarity", "Target Similarity", 
                                "Pathway Similarity", "P.Value")
    
    # get the meta-info for the interacting drugs
    metaInfo = drugMetaData[which(drugMetaData[,1] %in% mytabSN$Value[,1]),c(1,2,3)] # (1:ID, 2:Name, and 3:Group)
    metaInfo[,3] = gsub("\\,",", ", metaInfo[,3])
    colnames(metaInfo) <- c("id","name","Groups")
    
    # mytabSN$Value[,5] = ifelse(is.na(mytabSN$Value[,5]),"NA",mytabSN$Value[,5])
    
    
    mytabSN$Value = as.data.frame(merge(mytabSN$Value,metaInfo,by = "id")) 
    
    # mytabSN$Value <- as.data.frame(cbind(mytabSN$Value[,8],mytabSN$Value[,9],mytabSN$Value[,2],mytabSN$Value[,3],
    #                        mytabSN$Value[,4],mytabSN$Value[,7]))
    mytabSN$Value <- as.data.frame(cbind(mytabSN$Value[,6],mytabSN$Value[,7],mytabSN$Value[,2],mytabSN$Value[,3],
                                         mytabSN$Value[,4],mytabSN$Value[,5]))
    
    colnames(mytabSN$Value) = c("Target","Group",  "Structure Similarity", "Target Similarity",
                                "Pathway Similarity", "P.value")
    # mytabSN$Value[,2] <- ifelse(mytabSN$Value[,2]=="FDA", gicon("ok"), gicon("alert"))
    if(input$S_or_P_or_AdjP == "Scores")
      colnames(DSN)[5] <- "Scores" 
    else if(input$S_or_P_or_AdjP == "p-value")
      colnames(DSN)[5] <- "p-value"
    else
      colnames(DSN)[5] <- "Adj. p-value"
    
    cols <- c(3,4,5,6)
    mytabSN$Value[,cols] <- apply(mytabSN$Value[,cols],2,function(x) as.numeric(as.character(x)))
    mytabSN$Value <- as.data.frame(mytabSN$Value[order(mytabSN$Value$P.value),])
    
    # 1.2 Network View    
    mytabSN_nodes$Value <<- data.frame(id = c(currentDrugName$Value,unique(as.character(mytabSN$Value$Target))))
    # print(head(mytabSN_nodes$Value))
    mytabSN_edges$Value <<- data.frame(from = c(currentDrugName$Value), to = unique(as.character(mytabSN$Value$Target)))
    output$networkView <- renderUI(strong("Network View:"))
    
    # get and pre-process metadata
    metaData <- drugMetaData[drugMetaData[,1] == input$select_button,]
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
    
    # 2. Populate Physiological Properties
    mytabPP$Value <<- t(as.data.frame(metaData[,c(4:19)]))
    colnames(mytabPP$Value) = paste("Drug Name:", currentDrugName$Value)
    
    # 3. Populate Structure tab
    
    pubChemID = drugLinks[which(drugLinks$DrugBank.ID == input$select_button), 7]
    # pubChemID = NA
    if(!is.na(pubChemID)){
      output$S_drugName = renderUI(strong(paste("Drug Name:",currentDrugName$Value)))
      mytabS$Value = tags$iframe(src=paste0("https://embed.molview.org/v1/?mode=balls&cid=",pubChemID), style="width: 500px; height: 300px;")
      # mytabS_periodicTable$Value = includeHTML("./data/molviewPeriodicTable.html")
      # mytabS$Value = tags$iframe(src="https://embed.molview.org/v1/?mode=balls&smiles=O=C([C@]([H])(C([H])([H])C([H])([H])C([H])([H])/N=C(\N([H])[H])/N([H])[H])N([H])C([C@]([H])(C([H])([H])C([H])(C([H])([H])[H])C([H])([H])[H])N([H])C([C@]([H])(C([H])([H])OC(C([H])([H])[H])(C([H])([H])[H])C([H])([H])[H])N([H])C([C@@]([H])(C([H])([H])C1C([H])=C([H])C(=C([H])C=1[H])O[H])N([H])C([C@]([H])(C([H])([H])O[H])N([H])C([C@@]([H])(C([H])([H])C1=C([H])N([H])C2=C([H])C([H])=C([H])C([H])=C12)N([H])C([C@]([H])(C([H])([H])C1=C([H])N=C([H])N1[H])N([H])C([C@@]([H])1C([H])([H])C([H])([H])C(N1[H])=O)=O)=O)=O)=O)=O)=O)=O)N1C([H])([H])C([H])([H])C([H])([H])[C@@]1([H])C(N([H])N([H])C(N([H])[H])=O)=O", style="width: 500px; height: 300px;")
      
    }else{
      mytabS$Value = "PubChem ID is not available for this drug!!"
    }
    
    # 4. Populate Pharmacology tab
    mytabP$Value <<- t(as.data.frame(metaData[,c(20:35)]))
    colnames(mytabP$Value) = paste("Drug Name:", currentDrugName$Value)
    
  })
  
  # 1. 
  output$tabSN <- DT::renderDataTable({
    DT::datatable(mytabSN$Value, rownames = F, escape = FALSE, extensions = c('Buttons','Responsive'), options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      
      # following javascript forcefully converts doubles into 
      # scientific number
      # credit: https://stackoverflow.com/a/49635311/11400614
      rowCallback = JS(
        "function(row, data) {",
        "for (i = 1; i < data.length; i++) {",
        "if (data[i]>1000 | data[i]<1){",
        "$('td:eq('+i+')', row).html(data[i].toExponential(1));",
        "}",
        "}",
        "}")
      ))
    # DT::datatable(mytabSN$Value, rownames = F, selection = "single", escape = FALSE, extensions = c('Buttons','Scroller'), options = list(
    #   dom = 'Bfrtip',
    #   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #   deferRender = TRUE, scrollY = 500, scroller = TRUE
    # ))
    
  })
  
  output$SNView <- renderVisNetwork({
    if(!is.na(mytabSN_nodes$Value) && !is.na(mytabSN_edges$Value)){
      
      visNetwork(mytabSN_nodes$Value, mytabSN_edges$Value) %>%
        visExport() %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = list(enabled = TRUE, selected = currentDrugName$Value))
    }
  })
  
  # 2. 
  output$tabPP <- render_tableHTML({
    HTML(
      tableHTML(mytabPP$Value, collapse = 'separate', spacing = '2px 8px', headers = ) %>%
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
  
  # 3.
  output$frameS <- renderUI({
    mytabS$Value
  })
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
}

shinyApp(ui, server)

# Possible New features:
# 1. Clickable links for the target Drug names in the right-panel
#    - when clicked, the left DT is filtered with that drugname
#       and the right panel updated automatically. 
# May not be possible: pairs aren't symmetric