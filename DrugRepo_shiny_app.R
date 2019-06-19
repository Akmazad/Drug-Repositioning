library(shiny)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(htmlTable)
library(visNetwork)
library(markdown)
library(shinythemes)

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
                               # column(width=3,
                               #        # tags$img(src="Drug rep.jpg", height="100%", width="100%", align='middle')),
                               #        HTML("<div style=\" display: table; \"><img src=\"Drug rep.jpg\",  style=\"height: 100%; width:100%; \"></img></div>")),
                               #        # div(style="width: 100%; height: 100%; display: table; text-align: center;",div(style="display: table-cell; vertical-align: middle;", img(src="Drug rep.jpg", height="50%", width="50%")))),
                               column(width=12, 
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
                                            tabPanel("Physicochemical Properties", htmlOutput("tabPP")),
                                            tabPanel("Structure", 
                                                     tags$br(),
                                                     htmlOutput("S_drugName"),
                                                     htmlOutput("frameS"),
                                                     tags$br(),
                                                     # htmlOutput("S_periodicTable")),
                                                     actionButton("showP", "Show Molview Periodic Table")),
                                            tabPanel("Pharmacology", htmlOutput("tabP"))
                                          )
                                        )))
                      ))
             ),
             tabPanel("Download",
                     fluidRow(id='fd',
                              column(2, "Full table", align="center"),

                              column(10,
                                     downloadButton("downloadData", "Download"))
                                     # DT::dataTableOutput("download_table") %>% withSpinner())
                              )
                              
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
  # Download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Full table", ".csv", sep = "")
    },
    content = function(file) {
      file.copy("./data/drug links_new.csv", file)
    }
  )
  
  gicon <- function(x) as.character(icon(x, lib = "glyphicon"))

  
  # text filling zone
  # Main Table zone ---------------------------------------------------
  # Load and modify Data
  full_data <- read.csv("./data/net-information - Copy.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  drugLinks = read.csv("./data/drug links.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  
  DSN = as.data.frame(full_data[,-c(4:8, 12:16)])
  DB = as.data.frame(unique(DSN[,c(1,2,3)]))
  
  
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
    fda1 = DB$FDA.Status1,
    Actions = paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary view" id=',DB$ID1,'>View</button>
             </div>
             '),
    stringsAsFactors = FALSE
  ))
  
  # Render DSN
  output$contents <- DT::renderDataTable({
    re_df$data <- as.data.frame(re_df$data)[order(re_df$data[,1]),]
    colnames(re_df$data) = c("Generic Name", "FDA?", "")
    
    re_df$data[,2] <- ifelse(re_df$data[,2]=="FDA", gicon("ok"), gicon("alert"))
    
    DT::datatable(re_df$data, selection = 'none', rownames = FALSE, escape = FALSE, extensions = 'Scroller', 
                  options = list(deferRender = TRUE, scrollY = 500, scroller = TRUE))
  })
  
  # Search DSN as you type
  observeEvent(input$page, {
    dataTableProxy("contents") %>% selectPage(input$page)
  })
  

  observeEvent(input$select_button, {
    # Current Drug Name
    currentDrugName$Value = DB[which(DB$ID1 == input$select_button),2]
    
    # 1. Populate Similarity Tab ------------------------------------------
    # 1.1 Table View
    output$tableView <- renderUI(strong("Table View:"))
    mytabSN$Value <<- as.data.frame(unique(DSN[which(DSN$ID1 == input$select_button),-c(1,2,3,4)]))
    mytabSN$Value[,5] = ifelse(is.na(mytabSN$Value[,5]),"NA",mytabSN$Value[,5])
    colnames(mytabSN$Value) = c("Target", "FDA?",  "Structure Similarity", "Target Similarity", 
                                "Pathway Similarity", "p-Value")
    mytabSN$Value[,2] <- ifelse(mytabSN$Value[,2]=="FDA", gicon("ok"), gicon("alert"))
    mytabSN$Value <- mytabSN$Value[order(mytabSN$Value$Target),]
    
    # 1.2 Network View    
    mytabSN_nodes$Value <<- data.frame(id = c(currentDrugName$Value,unique(mytabSN$Value[,1])))
    mytabSN_edges$Value <<- data.frame(from = c(currentDrugName$Value), to = unique(mytabSN$Value[,1]))
    output$networkView <- renderUI(strong("Network View:"))
    
    # 2. Populate Physiological Properties
    mytabPP$Value <<- t(as.data.frame(unique(full_data[which(full_data$ID1 == input$select_button),c(5,6)])))
    colnames(mytabPP$Value) = paste("Drug Name:", currentDrugName$Value)
    
    # 3. Populate Structure tab
    
    pubChemID = drugLinks[which(drugLinks$DrugBank.ID == input$select_button), 7]
    # pubChemID = NA
    if(!is.na(pubChemID)){
      output$S_drugName = renderUI(strong(paste("Drug Name:",currentDrugName$Value)))
      mytabS$Value = tags$iframe(src=paste0("https://embed.molview.org/v1/?mode=2Dballs&cid=",pubChemID), style="width: 500px; height: 300px;")
      # mytabS_periodicTable$Value = includeHTML("./data/molviewPeriodicTable.html")
      # mytabS$Value = tags$iframe(src="https://embed.molview.org/v1/?mode=balls&smiles=O=C([C@]([H])(C([H])([H])C([H])([H])C([H])([H])/N=C(\N([H])[H])/N([H])[H])N([H])C([C@]([H])(C([H])([H])C([H])(C([H])([H])[H])C([H])([H])[H])N([H])C([C@]([H])(C([H])([H])OC(C([H])([H])[H])(C([H])([H])[H])C([H])([H])[H])N([H])C([C@@]([H])(C([H])([H])C1C([H])=C([H])C(=C([H])C=1[H])O[H])N([H])C([C@]([H])(C([H])([H])O[H])N([H])C([C@@]([H])(C([H])([H])C1=C([H])N([H])C2=C([H])C([H])=C([H])C([H])=C12)N([H])C([C@]([H])(C([H])([H])C1=C([H])N=C([H])N1[H])N([H])C([C@@]([H])1C([H])([H])C([H])([H])C(N1[H])=O)=O)=O)=O)=O)=O)=O)=O)N1C([H])([H])C([H])([H])C([H])([H])[C@@]1([H])C(N([H])N([H])C(N([H])[H])=O)=O", style="width: 500px; height: 300px;")
      
    }else{
      mytabS$Value = "PubChem ID is not available for this drug!!"
    }
    
    # 4. Populate Pharmacology tab
    mytabP$Value <<- t(as.data.frame(unique(full_data[which(full_data$ID1 == input$select_button),c(7,8)])))
    colnames(mytabP$Value) = paste("Drug Name:", currentDrugName$Value)
    
  })
  
  # 1. 
  output$tabSN <- DT::renderDataTable({
    DT::datatable(mytabSN$Value, rownames = F, escape = FALSE, extensions = c('Buttons', 'Scroller'), options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      deferRender = TRUE,
      scrollY = 300,
      scroller = TRUE))
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
  output$tabPP <- renderUI({
    HTML(
      htmlTable(
        mytabPP$Value
      )
    )
  })
  
  # 3. 
  output$frameS <- renderUI({
    mytabS$Value
  })
  # output$S_periodicTable <- renderUI({
  #   mytabS_periodicTable$Value
  # })
  observeEvent(input$showP, {
    showModal(modalDialog(
      # title = "Molview Periodic Table",
      # "This is an important message!",
      tags$img(src='molview_periodic_table.png', height='100%', width='100%'), size = 'l', easyClose = T
    ))
  })
  
  # 4.
  output$tabP <- renderUI({
    HTML(
      htmlTable(
        mytabP$Value
      )
    )
  })
  
  
  # output$download_table <- renderDataTable({
  #   
  # })
}

shinyApp(ui, server)
