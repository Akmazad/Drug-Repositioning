library(shiny)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(htmlTable)
library(visNetwork)

ui <- fluidPage(
  
  useShinyjs(),
  
  # Application title
  titlePanel("Drug Developer"),
  # tags$head(tags$script(src = "message-handler.js")),
  
    # 
    sidebarLayout(
    sidebarPanel(
      # TBD
      DT::dataTableOutput("contents") %>% withSpinner()
    ),
    mainPanel(
      # TBD
      tabsetPanel(
        id = 'DrugInfo',
        tabPanel("Similarity Network", 
                 DT::dataTableOutput("tabSN"), 
                 visNetworkOutput("SNView")  %>% withSpinner()
        ),
        tabPanel("Physicochemical Properties", htmlOutput("tabPP")),
        tabPanel("Structure", htmlOutput("frameS")),
        tabPanel("Pharmacology", htmlOutput("tabP"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Main Table zone ---------------------------------------------------
  # Load and modify Data
  full_data = read.csv("./data/net-information.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  drugLinks = read.csv("./data/drug links.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
  
  DSN = as.data.frame(full_data[,-c(3:7, 10:14)])
  DB = as.data.frame(unique(DSN[,c(1,2)]))

  
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
  

  shinyInput <- function(FUN, len, ids, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ids[i], ""), ...))
    }
    inputs
  }
  re_df <- reactiveValues(data = data.frame(
    GenericName = DB$name1,
    Actions = shinyInput(actionButton, length(DB$name1), DB$ID1,
                         label = "View", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
    stringsAsFactors = FALSE
  ))
  
    # Render DSN
  output$contents <- DT::renderDataTable({
    colnames(re_df$data) = c("Generic Name", "")
    DT::datatable(re_df$data, selection = 'none', rownames = FALSE, escape = FALSE)
  })
  
  # Search DSN as you type
  observeEvent(input$page, {
    dataTableProxy("contents") %>% selectPage(input$page)
  })
  
  observeEvent(input$select_button, {
    currentDrugName$Value = DB[which(DB$ID1 == input$select_button),2]
    mytabSN$Value <<- as.data.frame(unique(DSN[which(DSN$ID1 == input$select_button),-c(1,2,3)]))
    colnames(mytabSN$Value) = c("Target", "Structure Similarity", "Target Similarity", "Pathway Similarity", "p-Value")

    mytabSN_nodes$Value <<- data.frame(id = c(currentDrugName$Value,unique(mytabSN$Value[,1])))
    mytabSN_edges$Value <<- data.frame(from = c(currentDrugName$Value), to = unique(mytabSN$Value[,1]))
    
    mytabPP$Value <<- t(as.data.frame(unique(full_data[which(full_data$ID1 == input$select_button),c(4,5)])))
    colnames(mytabPP$Value) = paste("Drug Name:", currentDrugName$Value)
    
    # SMILE code of the drug
    # mytabS$Value = "CC(C)C[C@H](NC(=O)[C@@H](COC(C)(C)C)NC(=O)[C@H](CC1=CC=C(O)C=C1)NC(=O)[C@H](CO)NC(=O)[C@H](CC1=CNC2=CC=CC=C12)NC(=O)[C@H](CC1=CN=CN1)NC(=O)[C@@H]1CCC(=O)N1)C(=O)N[C@@H](CCCN=C(N)N)C(=O)N1CCC[C@H]1C(=O)NNC(N)=O"
    pubChemID = drugLinks[which(drugLinks$DrugBank.ID == input$select_button), 7]
    if(!is.na(pubChemID)){
      mytabS$Value = tags$iframe(src=paste0("https://embed.molview.org/v1/?mode=balls&cid=",pubChemID), style="width: 500px; height: 300px;")
    }
    
    mytabP$Value <<- t(as.data.frame(unique(full_data[which(full_data$ID1 == input$select_button),c(6,7)])))
    colnames(mytabP$Value) = paste("Drug Name:", currentDrugName$Value)
  })
  
  # 1. following two should be upon the clicking a drug
  output$tabSN <- DT::renderDataTable({
    DT::datatable(mytabSN$Value, rownames = F)
  })
  
  output$SNView <- renderVisNetwork({
    if(!is.na(mytabSN_nodes$Value) && !is.na(mytabSN_edges$Value)){
      
      visNetwork(mytabSN_nodes$Value, mytabSN_edges$Value, 
                 main = "Similariy Network", submain = currentDrugName$Value) %>%
        visExport() %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = list(enabled = TRUE, selected = currentDrugName$Value))
    }
  })
  
  # 2. 
  # output$tabPP <- DT::renderDataTable({
  #   DT::datatable(mytabPP$Value, options = list(orderClasses = TRUE))
  # })
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
  
  # output$tabS <- DT::renderDataTable({
  #   DT::datatable(myValue$Value, options = list(orderClasses = TRUE))
  # })
  # output$tabP <- DT::renderDataTable({
  #   DT::datatable(mytabP$Value, options = list(orderClasses = TRUE))
  # })
  output$tabP <- renderUI({
    HTML(
      htmlTable(
        mytabP$Value
      )
    )
  })
}

shinyApp(ui, server)
