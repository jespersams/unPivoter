  library(readxl)
  library(xlsx)
  library(reshape2)
  options(java.parameters = "-Xmx6000m")
  
  jgc <- function()
  {
    gc()
    .jcall("java/lang/System", method = "gc")
  } 

    ui <- fluidPage(titlePanel("Unpivoter tool"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "1. Choose Excel File",
                  accept = c(
                    ".xlsx",
                    ".xls")
        ),
        tags$hr(),
        selectInput("Sheet", "2. Select relevant datasheet",
                    c("")),
        tags$hr(),
        numericInput("varRow", "3. Select variable row", NULL, min = 1, max = 10),
        tags$hr(),
        numericInput("defRow", "4. Select definition row", NULL, min = 1, max = 10),
        tags$hr(),
        selectInput("IDs", "5. Choose ID/record/key variable(s)", c(""),multiple = TRUE),
        textInput("Questions", "6. Define questions to group", ""),
        actionButton("addQuestion", "Add group", width = "100%"),
        tags$hr(),
        textOutput("caption"),
        tags$hr(),
        textOutput('list'),
        actionButton("removeQuestion", "Remove latest group", width = "100%"),
        tags$hr(),
        downloadButton('downloadData', 'Download', style = "width:100%")
      ),
      mainPanel(
        tableOutput("defTable"),
        tags$hr(),
        tableOutput("table")
      )
    )
  )
  
  server <- function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)
    
output$table = renderTable({
  croppedSheet()[1:10,]
  })

output$defTable = renderTable({
y <- fullSheet()[1,]
x <- y[-1,]
colnames(x) <- paste0(strtrim(as.character(unlist(fullSheet()[input$defRow,])),15),"...")
return(x)
})



mySheets <- reactive ({
  if(is.null(input$file1))
   {return()} 
  else {
    inFile <- input$file1
    file.copy(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  sheets <- excel_sheets(
    paste(inFile$datapath, ".xlsx", sep=""))
     return( sheets)}
  })

fullSheet <- reactive ({
  if (is.null(input$file1))
     {
    return()
  } 
  else{
    inFile <- input$file1
    file.copy(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
    return(
      read_excel(
        paste(inFile$datapath, ".xlsx", sep=""), sheet = input$Sheet, col_names=FALSE)
    )
  }

})

myColumns = reactive({
  x <- as.character(unlist(fullSheet()[input$varRow,]))
  return(x)
})

croppedSheet <- reactive ({
  z <- input$varRow
  y <- fullSheet()
  x <- y[-c(1:z),]
    colnames(x) <- as.character(unlist(y[z,]))
  return(x)
})

questionArray <- reactive ({
  x <- c()
  x <- c(x,1:2)
  return (x)
})


observe({
  x <- mySheets()
  if (is.null(x))
    x <- character(0)
  updateSelectInput(session, "Sheet",
                    label = NULL,
                    choices = x,
                    selected = NULL
  )
})

  observeEvent(input$varRow, {
        x <- as.character(unlist(fullSheet()[input$varRow,]))

      updateSelectInput(session, "IDs",
                        label = NULL,
                        choices = x,
                        selected = NULL
      )

  }) 

myValues <- reactiveValues()
observeEvent(input$addQuestion, {
    myValues$dList <-c(isolate(myValues$dList), input$Questions)
    updateSelectInput(session, "Questions",
                      selected = ''
    )
  }
)

observeEvent(input$removeQuestion, {
  myValues$dList <- head( myValues$dList, -1)
}
)

output$list<-renderPrint({
  myValues$dList
})

output$downloadData <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.xlsx', sep='')
     },
     content = function(con) {  df.values <- croppedSheet()
     vec.names <- as.data.frame(list(as.character(unlist(fullSheet()[input$defRow,])),as.character(unlist(fullSheet()[input$varRow,]))))
     colnames(vec.names) <- c("Category", "variable")
     myWorkbook <- createWorkbook()
     df.values$ID <- do.call(paste, c(df.values[input$IDs], sep = ".")) 
     query.vector = myValues$dList
     excluded.columns <- c()
     untangle <- function(x){
       ### define query ###
       query.string <- x
       
       ### define subset ###
       needed.columns <- c(grep(query.string, names(df.values), value = TRUE))
       excluded.columns <<- append(excluded.columns,needed.columns)
       
       ### extract subdata ###
       df.wide <- df.values[,c("ID",needed.columns) ]
       
       ### unpivot data ###
       df.long <- melt(df.wide, id.vars = "ID")
       df.long <- df.long[order(df.long[,1]),]
       
       ### convert code to categories ###
       df.final <- (merge(vec.names, df.long, by = 'variable'))
       #df.final <- df.wide
       
       names(df.final)[names(df.final) == 'Category'] <- paste0(query.string,"Category")
       names(df.final)[names(df.final) == 'value'] <- paste0(query.string,"value")
       names(df.final)[names(df.final) == 'ID'] <- paste('ID', sep = ".")
       
       sapply(df.final,as.numeric)
       
       ### add unpivoted table to Excel-file ###
       mySheet <- createSheet(wb=myWorkbook, sheetName=query.string)
       addDataFrame(x=df.final, sheet=mySheet, row.names=FALSE)
       jgc()
     }
     sapply(query.vector, untangle)
     
     df.excluded <- df.values[!(names(df.values) %in% excluded.columns)]
     mySheet <- createSheet(wb=myWorkbook, sheetName="demographics")
     addDataFrame(x=df.excluded, sheet=mySheet, row.names=FALSE)
       saveWorkbook(myWorkbook, 'temp.xlsx')
       file.rename('temp.xlsx', con)
     }
   )


  }
  shinyApp(ui, server) 