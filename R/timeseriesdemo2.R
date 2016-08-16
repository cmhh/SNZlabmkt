#' Interface for Statistics New Zealand Labour Market Time Series Data.
#'
#' Simple shiny application to explore Statistics New Zealand labour market time series data.
#'
#' @author Chris Hansen
#'
#' @export
#'
#' @examples
#' \dontrun{
#' }
timeseriesdemo2 <- function(){

   pkg <- path.package("SNZlabmkt")
   addResourcePath('www', paste0(pkg, "/www/www"))

   shinyApp(

      ui = htmlTemplate(paste0(pkg, "/www/index.html"),
         controls=wellPanel(
            selectInput("subject", "Subject:",
               choices=c("Household Labour Force Survey"="HLF",
                         "Labour Cost Index"="LCI",
                         "Quarterly Employment Survey"="QES")),
            uiOutput("groupcontainer"),
            uiOutput("title1container"),
            uiOutput("title2container"),
            uiOutput("title3container"),
            uiOutput("title4container"),
            uiOutput("title5container"),
            uiOutput("unitscontainer")
         ),
         plot=plotOutput("plot", height="650px"),
         tab=div(DT::dataTableOutput("data")), style="padding: 10px;"
      ),

      server = function(input, output){

         sessionvars <- reactiveValues(meta=NULL)

         data1 <- reactive({
            req(input$subject)
            isolate({
               sessionvars$meta <- metadata[subject==input$subject]
               sessionvars$meta
            })
         })

         output$groupcontainer <- renderUI({
            req(input$subject, data1())
            choices <- unique(data1()$group)
            choices <- choices[order(choices)]
            selectInput("group", "Group:", choices=choices)
         })

         data2 <- reactive({
            req(input$subject, input$group, data1())
            isolate({
               sessionvars$meta <- data1()[group==input$group]
               sessionvars$meta
            })
         })

         ntitle <- reactive({
            req(input$subject, input$group, data2())
            data2()$ntitle[1]
         })

         output$title1container <- renderUI({
            req(input$subject, input$group, data2())
            choices <- unique(data2()$title1)
            selectInput("title1", "Title 1:", choices=choices, selected=choices, multiple=TRUE)
         })

         data3 <- reactive({
            req(input$subject, input$group, input$title1, data2())
            isolate({
               sessionvars$meta <- data2()[title1%in%input$title1]
               sessionvars$meta
            })
         })

         output$title2container <- renderUI({
            req(input$subject, input$group, input$title1, ntitle(), data3())
            if (ntitle()<2) return(NULL)
            choices <- unique(data3()$title2)
            selectInput("title2", "Title 2:", choices=choices, selected=choices, multiple=TRUE)
         })

         data4 <- reactive({
            req(input$subject, input$group, input$title1, input$title2, data3())
            isolate({
               sessionvars$meta <- data3()[title2%in%input$title2]
               sessionvars$meta
            })
         })

         output$title3container <- renderUI({
            req(input$subject, input$group, input$title1, input$title2, ntitle(), data4())
            if (ntitle()<3) return(NULL)
            choices <- unique(data4()$title3)
            selectInput("title3", "Title 3:", choices=choices, selected=choices, multiple=TRUE)
         })

         data5 <- reactive({
            req(input$subject, input$group, input$title1, input$title2, input$title3, data4())
            isolate({
               sessionvars$meta <- data4()[title3%in%input$title3]
               sessionvars$meta
            })
         })

         output$title4container <- renderUI({
            req(input$subject, input$group, input$title1, input$title2, input$title3, ntitle(), data4())
            if (ntitle()<4) return(NULL)
            choices <- unique(data5()$title4)
            selectInput("title4", "Title 4:", choices=choices, selected=choices, multiple=TRUE)
         })

         data6 <- reactive({
            req(input$subject, input$group, input$title1, input$title2, input$title3, input$title4, data5())
            isolate({
               sessionvars$meta <- data5()[title4%in%input$title4]
               sessionvars$meta
            })
         })

         output$title5container <- renderUI({
            req(input$subject, input$group, input$title1, input$title2, input$title3, ntitle(), data4())
            if (ntitle()<5) return(NULL)
            choices <- unique(data6()$title5)
            selectInput("title5", "Title 5:", choices=choices, selected=choices, multiple=TRUE)
         })

         plotdata <- reactive({
            res <- merge(sessionvars$meta, labmkt, by="ref", all=FALSE)
            res[,value:=value * 10^magnitude]
            res[,c("ref", "status", "group", paste0("title", 1:ntitle()), "period", "value", "units", "status"), with=FALSE]
         })

         output$unitscontainer <- renderUI({
            req(plotdata())
            choices <- unique(plotdata()$units)
            selectInput("units", "Units:", choices=choices)
         })

         output$plot <- renderPlot({
            req(input$units, plotdata())
            pd <- copy(plotdata())
            ggplot(pd[units==input$units], aes(period, value)) +
               geom_line(aes(color=ref)) + ylab(input$units) +
               theme(legend.position="bottom")
         })

         output$data <- DT::renderDataTable({
            req(input$units, plotdata())
            plotdata()[units==input$units]
         }, rownames=FALSE, options=list())

      }, onStart=NULL, options=list(), uiPattern="/")
}
