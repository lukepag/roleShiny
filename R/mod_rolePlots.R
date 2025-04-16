#' rolePlots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session,name1,name2,check1,check2,name,func,type,checkBox,allSims Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny plotly roleR ggplot2 highcharter magrittr
#' @importFrom dplyr left_join
#' 
library(magrittr)
library(plotly)
library(gganimate)

mod_rolePlots_ui <- function(id,
                             has_traits = FALSE,
                             has_phylo = FALSE
){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(".rightAlign{float:right;}")),
    verbatimTextOutput("debug"),
    tabsetPanel(
      type = "tabs",
      tabPanel("Abundances", 
               fluidRow(
                 column(width = 12, 
                        imageOutput(ns("abundRank"))
                 )
               )
      ),
      
      if (has_traits) {
        tabPanel("Traits", 
                 fluidRow(
                   column(width = 5, 
                          plotlyOutput(ns("traitRank"))
                   ),
                   column(width = 5, 
                          plotlyOutput(ns("traitTime"))
                   )
                 )
        )
      }
      ,
      if (has_phylo) {
        tabPanel("Phylogenetics",
                 fluidRow(
                   column(width = 8,
                          plotlyOutput(ns("phylo"))
                   )
                 )
        )
      }
      
    )  
  )
}

#' rolePlots Server Functions
#'
#' @noRd 
mod_rolePlots_server <- function(id, sims_out, allSims, has_phylo = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(allSims())
      
      sumstats <- reactive({
        ss <- roleR::getSumStats(allSims(), 
                                 funs = list(abund = roleR::rawAbundance, 
                                             hillAbund = roleR::hillAbund, 
                                             rich = roleR::richness,
                                             traits = roleR::rawTraits,
                                             hillTrait = roleR::hillTrait), 
                                 moreArgs = list(hillAbund = list(q = 1:3)))
        ss[,"gen"] <- allSims()@info$generations
        return(ss)
      })
      
      raw <- reactive({
        abund <- tidy_raw_rank(sumstats(), "abund")
        traits <- tidy_raw_rank(sumstats(), "traits")
        return(list(abund = abund, traits = traits))
      })
      
      # Time plot (animated)
      fig_abundRank <- reactive({
        abund_rank <- raw()$abund
        gg_scatter(dat = abund_rank, dat_2 = sumstats(), yvar = "abund", is_abund = TRUE)
      })
      
      output$abundRank <- renderImage({
        # Create a temporary file to save the animation
        outfile <- tempfile(fileext = ".gif")
        
        # Render the animation as a GIF
        anim <- animate(fig_abundRank(), 
                        nframes = length(unique(sumstats()$gen)), 
                        fps = 10, 
                        width = 800, 
                        height = 400, 
                        renderer = gifski_renderer())
        
        # Save the animation
        gganimate::anim_save(outfile, animation = anim)
        
        # Return the image info for Shiny
        list(src = outfile,
             contentType = "image/gif",
             width = 800,
             height = 400,
             alt = "Animated time plot")
      }, deleteFile = TRUE)  # Delete the temporary file after rendering
    })
  })
}