library(ggplot2)
library(shiny)
library(shinythemes)
library(shinyWidgets)

fluidPage(
	theme = shinytheme("united"),
	titlePanel(title="",windowTitle = "Hotspot Mutations"),
  h1(id="pageTitle","Frequency of Mutations in COSMIC v92"),
   tags$style(HTML("#pageTitle{color: #ff5e19;}")),
	p('This website sources data from', 
		a(href="https://cosmic-blog.sanger.ac.uk/cosmic-release-v92/",
		'COSMIC database'),
		'(v92, released 2020/08/27) and presents the frequency of somatic mutations in different genes.'
	),
  p('Visit ',
       a(href="https://cancer.sanger.ac.uk/cosmic",
       'COSMIC'),
       '- the Catalogue Of Somatic Mutations In Cancer, world\'s largest and most comprehensive resource for exploring the somatic mutations identified from human cancers.'
    ),
    plotOutput('plot'),
    p('Try searching for:', strong(span("BRAF",style='color:#ff5e19')), '/' ,
    strong(span("KRAS G12C",style='color:#ff5e19')), '/' ,
    strong(span("lung",style='color:#ff5e19'))
    ,align="left"),
    fluidRow(
        column(4,
               searchInput(
                  inputId = "Search",
                  label = "Type search term(s):",
                  value="",
                  placeholder = "KRAS G12",
                  btnSearch = icon("search"),
                  btnReset = icon("remove"),
                  width = "100%")
        ),
        column(4,
                selectInput("size",
                   "Select plot size:",
                   c(10,50,100,500,1000,10000),
                   selected = "100",
                   multiple = F,
                   selectize = F,
                   width = "120")
               )
        ),
  tableOutput("table"),
	p('Note: NS = Not specified')
  
)
