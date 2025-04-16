#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr stringr ape ggtree magrittr

library(dplyr)
library(ggtree)
library(ggplot2)
library(plotly)
library(cowplot)

# function to get the date and time in a reasonable format to append to the end of files for a unique filename
file_suffix <- function() {
  Sys.time() |> 
    str_replace_all("\\:", "-") |> 
    str_replace_all(" ", "_")
}


# hill calculation

## Get one hill number from a list of a variable. Original python code written by Isaac Overcast, with slight modifications (correct = TRUE implemented by CMF)
## dists are the OTU Tajima's pi
## order is the q order of the Hill number
## correct indicates if you want to correct for species richness or not. Default is TRUE
hill_calc <- function(dists, order = 1, correct = FALSE) { 
  if (order == 0) {
    return(length(dists))
  }
  if (order == 1) {
    h1 = exp(entropy::entropy(dists))
    if (correct) {
      return(h1 / length(dists))
    } else return(h1)
    
  }
  else {
    tot = sum(dists)
    proportions = dists/tot
    prop_order = proportions**order
    h2 = sum(prop_order)**(1/(1-order))
    if (correct) {
      return(h2 / length(dists))
    } else return(h2)
  }
}


# process raw abundances for plotting
# ss = sumstats, output from getSumStats
# raw_string = the raw data you want to format. For now, choices are "



tidy_raw_rank <- function(ss, raw_string) {
  
  o <- lapply(1:nrow(ss), function(i) {
    
    x <- ss[[raw_string]][[i]]
    x <- sort(x[x > 0], decreasing = TRUE)
    
    g <- rep(ss$gen[i], length(x))
    
    return(cbind(g, x))
  })
  
  o <- as.data.frame(do.call(rbind, o))
  
  colnames(o) <- c('gen', raw_string)
  
  o_rank <- o |> 
    group_by(gen) |> 
    mutate(rank = row_number())
  
  return(o_rank)
}


# plotting functions to make plotting easier
## scatterplot
gg_scatter <- function(dat, dat_2, yvar, is_abund = TRUE) {
  # Define y-axis label
  y_lab <- ifelse(is_abund, "Abundance", "Other Metric")
  
  # Rank-abundance plot (left plot, static) - we'll handle this separately
  p <- ggplot(dat, aes_string(x = "rank", y = yvar, group = "gen")) +
    geom_point(color = "#107361", size = 2) +
    labs(x = "", y = y_lab) +
    theme_bw() +
    theme(legend.key.size = unit(3, "mm"))
  
  # Time graph with growing line animation (right plot)
  # Create cumulative dataset for line growth
  dat_cumulative <- do.call(rbind, lapply(unique(dat_2$gen), function(g) {
    subset <- dat_2[dat_2$gen <= g, c("gen", "hillAbund_1"), drop = FALSE]
    subset$frame <- g  # Assign frame as the current generation
    return(subset)
  }))
  
  # Add initial point at (0,0) for frame 0
  initial_point <- data.frame(gen = 0, hillAbund_1 = 0, frame = 0)
  dat_cumulative <- rbind(initial_point, dat_cumulative)
  
  # Time plot: growing line and current point
  l <- ggplot(dat_cumulative) +
    geom_line(aes(x = gen, y = hillAbund_1, group = 1), color = "black", alpha = 1.0) +
    geom_point(data = dat_cumulative[dat_cumulative$gen == dat_cumulative$frame, ],
               aes(x = gen, y = hillAbund_1), color = "#107361", size = 3) +
    labs(x = "Generation", y = "Hill Abundance 1") +
    theme_bw() +
    theme(legend.key.size = unit(3, "mm")) +
    transition_states(frame, transition_length = 0, state_length = 1) +
    labs(title = "Gen: {closest_state}")
  
  # Return only the animated time plot for now
  return(l)
}

## timeseries
gg_ts <- function(dat, yvar) {
  
  if (yvar == "all_hill") {
    y_var <- as.formula(paste0("~", "hillAbund_1"))
    y_var_2 <- as.formula(paste0("~", "hillAbund_2"))
    y_var_3 <- as.formula(paste0("~", "hillAbund_3"))
    
    pt <- dat |>
      as_tibble() |>
      plot_ly() |>
      add_lines(x = ~ gen, y = y_var, line = list(color = "#107361"), name = "q = 1") |>
      add_lines(x = ~ gen, y = y_var_2, line = list(color = "black"), name = "q = 2") |>
      add_lines(x = ~ gen, y = y_var_3, line = list(color = "yellow"), name = "q = 3") |>
      layout(
        xaxis = list(title = "Time step", rangeslider = list(visible = T), gridcolor = "grey92", zerolinecolor = "grey92"),
        yaxis = list(title = "Hill number", gridcolor = "grey92", zerolinecolor = "grey92"),
        plot_bgcolor='white',
        legend = list(x = 0.1, y = 0.95)
      )
    
  } else {
    y_var <- as.formula(paste0("~", yvar))
    
    if (stringr::str_detect(yvar, "1")) {
      y_name <- "q = 1"
    } else if (stringr::str_detect(yvar, "2")) {
      y_name <- "q = 2"
    } else if (stringr::str_detect(yvar, "3")) {
      y_name <- "q = 3"
    }
    
    pt <- dat |>
      as_tibble() |>
      plot_ly() |>
      add_lines(x = ~ gen, y = y_var, line = list(color = "#107361")) |>
      layout(
        xaxis = list(title = "Time step", rangeslider = list(visible = T), gridcolor = "grey92", zerolinecolor = "grey92"),
        yaxis = list(title = y_name, gridcolor = "grey92", zerolinecolor = "grey92"),
        plot_bgcolor='white'
      )
  }
  
  
  
  
  return(pt)
}

## phylogenetic tree

plotly_phylo <- function() {
  
  trees <- lapply(rep(c(10, 25, 50, 100), 3), ape::rtree)
  class(trees) <- "multiPhylo"
  
  g <- ggtree::ggtree(trees, aes(frame = .id)) + 
    ggtree::theme_tree2()
  # either remove animation labels or see "generation" label
  
  gp <- ggplotly(g) |> 
    animation_opts(250, transition = 100) |> 
    animation_slider(hide = TRUE)
  
  gp
}



