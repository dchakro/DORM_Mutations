# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(shiny)
library(shinythemes)

fluidPage(
	theme = shinytheme("cerulean"),
# 	titlePanel(
# 	h1("Frequency of Mutations per residue - COSMIC v91", align = "center")),
	titlePanel("Frequency of Mutations per residue - COSMIC v91"),
	p('This website sources data from', 
		a(href="https://cosmic-blog.sanger.ac.uk/cosmic-release-v91/",
		'COSMIC database'),
		'(v91, released 2020/04/07) and presents the frequency of somatic mutations across different genes grouped by the amino acid residue.'
	),
    
    div(img(src ="https://seafile.utu.fi/f/c6df0c10da3a401f92df/?dl=1" ,width= 500), style="text-align: center;"),
    
    p('Visit ',
       a(href="https://cancer.sanger.ac.uk/cosmic",
       'COSMIC'),
       '- the Catalogue Of Somatic Mutations In Cancer, which is the world\'s largest and most comprehensive resource for exploring the somatic mutations identified from human cancers.'
    ),
    
    p('Try searching for:', strong(span("BRAF",style='color:#467FAC')), '/' ,
    strong(span("KRAS G12",style='color:#467FAC')), '/' ,
    strong(span("lung",style='color:#467FAC'))
    ,align="right"),
	# HTML(paste0('<center><img src="',normalizePath("Hotspot_resize.jpg"), 'width="500"></center>')),
	# Create a new row for the table.
	tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #000000;
                    }
###To change text and background color of the `Search` box ###
                    .dataTables_filter input {
                            color: #0E334A;
                            background-color: #FFFFFF
                           }

                    thead {
                    color: #467FAC;
                    }

                     tbody {
                    color: #000000;
                    }"
                    )),
	DT::dataTableOutput("table"),
	p('Note: NS = Not specified'),
	uiOutput("tweetURL")
  
)