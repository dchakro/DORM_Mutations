library(ggplot2)
library(shiny)

tissues <- readLines("./data/ColumnNames.txt",warn = F)
'%nin%' <- Negate('%in%')
tissues <- tissues[tissues %nin% c("Gene","Mutation","counts","Frequency")]

fluidPage(
	theme = shinythemes::shinytheme("united"),
	titlePanel(h1(id="pageTitle","— Hotspot Explorer —",align="center"),windowTitle = "Hotspot Mutations"),
	h2(id="pageTitle","Browse recurrent mutations in human cancers.",align="center"),
   tags$style(HTML("#pageTitle{color: #ff5e19;}")),
	h3(br()),
	plotOutput('plot'),
    fluidRow(
        column(3,
               shinyWidgets::searchInput(
                  inputId = "Search",
                  label = "Enter search term(s):",
                  value="",
                  placeholder = "e.g. KRAS G12",
                  btnSearch = icon("search"),
                  btnReset = icon("remove"),
                  width = "100%")
        ),
        column(2,
               selectInput("size",
                   "No. of records:",
                   c(10,50,100,500,1000,10000),
                   selected = "100",
                   multiple = F,
                   selectize = F,
                   width = "120")
               ),
        column(3,
               selectInput("tissue",
                           "Select tissue:",
                           c("all",tissues),
                           selected = "all",
                           multiple = F,
                           selectize = F,
                           width = "200")
              ),
        column(1,shinyWidgets::actionBttn(
                 inputId = "reset_input",
                 label = "Reset",
                 style = "pill", 
                 color = "warning",
                 size = "md"
               )
               ,offset = 1)
        ),
	p(strong("Tip:"),'Use , or ; between gene names e.g.', strong(span("BRAF, KRAS",style='color:#ff5e19')), 'and space with Gene-Mutation pair, e.g.' ,
	  strong(span("KRAS G12",style='color:#ff5e19')),
	  align="left"),
  tableOutput("table"),
	p('Note: NS = Not specified'),
	strong("Information:"),
	p(
	  'This website hosts processed data that is available at the',
	  a(href = "https://cosmic-blog.sanger.ac.uk/cosmic-release-v94/",
	    'COSMIC database'),
	  '(v94, released 2021/05/28). We present the frequency of recurrent somatic mutations in different genes.',
	  'Visit the',
	  a(href = "https://cancer.sanger.ac.uk/cosmic",
	    'Catalogue Of Somatic Mutations In Cancer'),
	  '- world\'s largest and most comprehensive resource for exploring the somatic mutations identified from human cancers.'
	)
)
