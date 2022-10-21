library(ggplot2)
library(shiny)

tissues <- readLines("./data/ColumnNames.txt",warn = F)
'%nin%' <- Negate('%in%')
tissues <- tissues[tissues %nin% c("Protein","Mutation","counts","Frequency")]

function(request){
fluidPage(
	theme = shinythemes::shinytheme("united"),
	a('Lab Home',
	  href = "https://eleniuslabtools.utu.fi",
	  style = 'color:#EE5F21',
	  target = "_blank"),
	HTML('&emsp;'),
	a('About DORM',
	  href = "https://eleniuslabtools.utu.fi/main/docs/DORM.html",
	  style = 'color:#EE5F21',
	  target = "_blank"),
	HTML('&emsp;'),
	a('DORM-Residues', href = "https://eleniuslabtools.utu.fi/tools/DORM/Residues/", style =
	    'color:#EE5F21'),
	HTML('&emsp;'),
	a('FAQ',
	  href = "https://eleniuslabtools.utu.fi/main/docs/DORM-FAQ.html",
	  style = 'color:#EE5F21',
	  target = "_blank"),
	titlePanel(h1(id = "pageTitle", "— DORM —", align = "center"), windowTitle = "DORM - Elenius Lab Tools"),
	h4(id = "pageTitle",
	   "Database of Recurrent Mutations",
	   align = "center"), 
  
   tags$style(HTML("#pageTitle{color: #ff5e19;}")),
	h3(br()),
	plotOutput('plot'),
  hr(),
	a('Advanced search', href="https://eleniuslabtools.utu.fi/main/docs/AdvancedSearch.html",target="_blank"),
    fluidRow(
        column(3,
               shinyWidgets::searchInput(
                  inputId = "Search",
                  label = "Enter search term(s):",
                  value="",
                  placeholder = "e.g. KRAS G12",
                  btnSearch = icon("search"),
                  btnReset = icon("times"),
                  width = "100%")
        ),
        column(2,
               selectInput("size",
                   "No. of records:",
                   c(10,25,50,100,500,1000,10000),
                   selected = "50",
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
  # p("Browse recurrent mutations identified from human cancer samples with genome-wide screening."),
  bookmarkButton(icon = icon("link"),
                label="Generate link", 
                title = "Get a direct link to your search."),
	              # The settings and search terms are anonymously saved to the server.
  hr(),
	p(strong("Tip:"),'You can use a comma ( , ) or a semicolon ( ; ) to query multiple proteins e.g.', strong(span("BRAF, KRAS, EGFR",style='color:#EE5F21')),
	  align="left"),
	p(strong("Note:"),'The database only lists recurrent mutations (i.e. mutations with population frequency > 1) identified from genomes of cancer samples.',
	  align="left"),
  hr(),
  tableOutput("table"),
	hr(),
	p(strong("Data updated:"),"2022-06-08 (yyyy-mm-dd)" ),
	strong("Abbreviations:"),
	pre("Ter = Termination i.e. STOP codon \nins / del = insertion / deletion\ndup / fs= duplication / frameshift\next = extension\nNS = Not specified"),
	hr()# ,
	# strong("Information:"),
	# p(
	#   'This website hosts processed data that is available at the',
	#   a(href = "https://cosmic-blog.sanger.ac.uk/cosmic-release-v94/",
	#     'COSMIC database'),
	#   '(v94, released 2021/05/28). We present the frequency of recurrent somatic mutations in different proteins.',
	#   'Visit the',
	#   a(href = "https://cancer.sanger.ac.uk/cosmic",
	#     'Catalogue Of Somatic Mutations In Cancer'),
	#   '- world\'s largest and most comprehensive resource for exploring the somatic mutations identified from human cancers.'
	# )
)
}