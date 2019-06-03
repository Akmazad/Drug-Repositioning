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
                      ")),
                      # 
                      fluidRow(id = "first",
                               column(width=2,
                                      tags$img(src = "drug_image.png")),
                               column(width=10, 
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
                                            tabPanel("Structure", htmlOutput("frameS")),
                                            tabPanel("Pharmacology", htmlOutput("tabP"))
                                          )
                                        )))
                      ))
             ),
             tabPanel("Download",
                      verbatimTextOutput("Download")
             ),
             tabPanel("Help",
                      verbatimTextOutput("Help")
             ),
             tabPanel("Contact",
                      verbatimTextOutput("Contact")
             )
  )
)

server <- function(input, output, session) {
  gicon <- function(x) as.character(icon(x, lib = "glyphicon"))

  
  # text filling zone
  # Main Table zone ---------------------------------------------------
  # Load and modify Data
  full_data = read.csv("./data/net-information - Copy.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
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
  pubChemID <- reactiveValues(Value = NULL)
  
  mytabP <- reactiveValues(Value = NULL)
  
  
  # shinyInput <- function(FUN, len, ids, ...) {
  #   inputs <- character(len)
  #   for (i in seq_len(len)) {
  #     inputs[i] <- as.character(FUN(paste0(ids[i], ""), ...))
  #   }
  #   inputs
  # }
  re_df <- reactiveValues(data = data.frame(
    GenericName = DB$name1,
    fda1 = DB$FDA.Status1,
    # Actions = shinyInput(actionButton, length(DB$name1), DB$ID1,
    #                      label = "View", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
    Actions = paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary view" id=',DB$ID1,'>View</button>
             </div>
             '),
    stringsAsFactors = FALSE
  ))
  
  # Render DSN
  output$contents <- DT::renderDataTable({
    # re_df$data = cbind(re_df$data,icon)
    # re_df$data = as.data.frame(re_df$data[,c(1,4,3)])
    colnames(re_df$data) = c("Generic Name", "FDA?", "")
    
    re_df$data[,2] <- ifelse(re_df$data[,2]=="FDA", gicon("ok"), gicon("alert"))
    # DT::datatable(re_df$data, selection = 'none', rownames = FALSE, escape = FALSE)
    DT::datatable(re_df$data, selection = 'none', rownames = FALSE, escape = FALSE, extensions = 'Scroller', 
                  options = list(deferRender = TRUE, scrollY = 500, scroller = TRUE))
  })
  
  # Search DSN as you type
  observeEvent(input$page, {
    dataTableProxy("contents") %>% selectPage(input$page)
  })
  

  observeEvent(input$select_button, {
    currentDrugName$Value = DB[which(DB$ID1 == input$select_button),2]
    
    mytabSN$Value <<- as.data.frame(unique(DSN[which(DSN$ID1 == input$select_button),-c(1,2,3,4)]))
    
    mytabSN$Value[,5] = ifelse(is.na(mytabSN$Value[,5]),"NA",mytabSN$Value[,5])
    colnames(mytabSN$Value) = c("Target", "FDA?",  "Structure Similarity", "Target Similarity", "Pathway Similarity", "p-Value")
    mytabSN$Value[,2] <- ifelse(mytabSN$Value[,2]=="FDA", gicon("ok"), gicon("alert"))
    
    mytabSN_nodes$Value <<- data.frame(id = c(currentDrugName$Value,unique(mytabSN$Value[,1])))
    mytabSN_edges$Value <<- data.frame(from = c(currentDrugName$Value), to = unique(mytabSN$Value[,1]))
    
    mytabPP$Value <<- t(as.data.frame(unique(full_data[which(full_data$ID1 == input$select_button),c(5,6)])))
    colnames(mytabPP$Value) = paste("Drug Name:", currentDrugName$Value)
    
    pubChemID = drugLinks[which(drugLinks$DrugBank.ID == input$select_button), 7]
    if(!is.na(pubChemID)){
      mytabS$Value = tags$iframe(src=paste0("https://embed.molview.org/v1/?mode=balls&cid=",pubChemID), style="width: 500px; height: 300px;")
    }
    
    mytabP$Value <<- t(as.data.frame(unique(full_data[which(full_data$ID1 == input$select_button),c(7,8)])))
    colnames(mytabP$Value) = paste("Drug Name:", currentDrugName$Value)
    output$tableView <- renderUI(strong("Table View:"))
    output$networkView <- renderUI(strong("Network View:"))
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
  
  # 4.
  output$tabP <- renderUI({
    HTML(
      htmlTable(
        mytabP$Value
      )
    )
  })
}

shinyApp(ui, server)