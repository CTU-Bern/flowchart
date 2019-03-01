

#' flowchart
#' @description \code{flowchart} helps taking the pain out of laying out a flowchart. It automatically sets the horizontal and vertical positions of boxes and links boxes with arrows.
#' 
#' @param dat dataframe with variables \code{level}, \code{group}, 
#' \code{incexc}, \code{text} and, optionally, \code{x}, \code{y}, 
#' \code{width}, \code{just}. See details.
#' @param gp colour to fill boxes with (single value)
#' @param group_exc_shift how much to move the exclusion group by, as a proportion of the figure width
#' @param arrow_obj getOption object to define the arrow type
#' @param term_arrow_type terminal arrow type (only used for terminal 
#' exclusions). Can take one of \code{v}, \code{h}, \code{L}, \code{-}, 
#' \code{Z} or \code{N}
#' @param tab return a table to make modifications easier
#' 
#' @details Eact row of \code{dat} represents a box in the flowchart. The vertical position on the flow chart is controlled by \code{level} (1 is at the top, 2 is below it and so on). Where multiple boxes must be on the same \code{level} (e.g. in an RCT), \code{group} is used to indicate which left horizontal position. The text in the box should be entered into a \code{text} variable. There should also be an \code{incexc} variable to indicate \code{inc}lusion or \code{exc}lusion. \code{Exc}lusion boxes are shifted \code{group_exc_shift} to the right of the \code{group}s position on the x axis.
#' 
#' @return dataframe containing \code{level}, \code{group}, \code{text}, 
#' \code{incexc}, \code{x}, \code{y}, \code{width}, which can be used to 
#' optimize the flowchart
#' @import dplyr
#' @import Gmisc
#' @import grid
#' @export flowchart
#' 
#' @examples
#' 
#' # standard example
#' dat <- data.frame(level = c(1, 2, 3, 4, 4, 5, 5, 6, 6),
#'                   group = c(NA, NA, NA, 1, 2, 1, 2, 1, 2),
#'                   text  = c("Assessed for eligibility", "Not eligible\nfor inclusion", 
#'                             "Randomized", "Group1", "Group2", "Excl Group1", "Exc\nGroup2", "Analysis1", 
#'                             "Analysis2"),
#'                   incexc = c("inc", "exc", "inc", "inc", "inc", "exc", "exc", 
#'                              "inc", "inc"))
#' dat
#' flowchart(dat)
#' 
#' 
#' # example with expression
#' dat <- data.frame(level = c(1, 2, 3, 4, 4, 5, 5, 6, 6),
#'                   group = c(NA, NA, NA, 1, 2, 1, 2, 1, 2),
#'                   text  = c("Assessed for eligibility", "Not eligible\nfor inclusion", 
#'                             "Randomized", "Group1", "Group2", "expression(paste(bold('Excl'), ' Group1'))", "Exc\nGroup2", "Analysis1", 
#'                             "Analysis2"),
#'                   incexc = c("inc", "exc", "inc", "inc", "inc", "exc", "exc", 
#'                              "inc", "inc"))
#' dat
#' flowchart(dat)
#' 
#' 
#' 
flowchart <- function(dat,
                      gp = gpar(fill = "lightgrey"),
                      group_exc_shift = 0.2,
                      arrow_obj = getOption("connectGrobArrow",
                                            default = arrow(ends = "last", 
                                                            type = "closed", 
                                                            length = unit(0.25, "cm")
                                                            )),
                      term_arrow_type = "L",
                      tab = FALSE){
  
  # coerce factors to strings 
  if(is.factor(dat$text)) dat$text <- as.character(dat$text)
  
  # xexists
  xexists <- "x" %in% names(dat)
  
  # derive x and y locations
  dat$row <- 1:nrow(dat)
  if(!xexists){ 
    dat <- dat %>% group_by(level) %>% mutate(n_lev = n(), x = 1/(n_lev+1), x_o = x)
  } else {
    dat <- dat %>% group_by(level) %>% mutate(n_lev = n(), x_o = x)
  }
  
  dat <- dat %>% group_by(group) %>% mutate(nth = 1:n())
  dat <- as.data.frame(dat)
  if(!xexists){
    dat$x[!is.na(dat$group)] <- (dat$x*dat$group)[!is.na(dat$group)]
    dat$x[is.na(dat$group) & dat$incexc == "exc"] <- 0.75
    w <- !is.na(dat$group) & dat$incexc == "exc"
    dat$x[w] <- (dat$x + dat$x_o*group_exc_shift)[w]
  }
  if(!"width" %in% names(dat)){
    dat$width <- .6/dat$n_lev
    dat$width[is.na(dat$group)] <- .3
    w <- dat$incexc == "exc"
    dat$width[w] <- dat$width[w]*.7
  }
  if(!"just" %in% names(dat)){
    dat$just <- "left"
  }
  
  dat$lines <- sapply(strsplit(dat$text, "\n", fixed = TRUE), length)
  
  dat <- dat %>% 
    group_by(level) %>% 
    mutate(n_lines = max(lines)) %>% 
    as.data.frame
  t <- tapply(dat$n_lines, dat$level, max)
  dat$cum_lines <- cumsum(t)[dat$level]
  ma <- function(x, n=3){
    c(x[1], stats::filter(x, rep(1/n, n), sides=2)[2:(length(x)-1)], x[length(x)])
  }
  dat$rolling <- ma(cumsum(t))[dat$level]
  tsum <- sum(t)
  
  
  
  # pad out text with different numbers of lines
  dat$linesdif <- with(dat, as.numeric(n_lines) - as.numeric(lines))
  pad <- dat$linesdif > 0
  exp <- grepl("^expression\\(", dat$text)
  if(any(pad)){
    # normal text
    w <- pad & !exp
    if(any(w)){
      dat$text[w] <- apply(dat[w,], 1, function(x){ 
        paste0(x["text"], 
               paste0(rep("\n", 
                          x["linesdif"]), 
                      collapse = "")
               )
        }
        )
    }
    w <- pad & exp
    if(any(w)){
      # expressions
      dat$text[w] <- apply(dat[w, ],
                           1,
                           function(x)
                             gsub("\\)$",
                                       paste0(
                                         rep("\n",
                                             x["linesdif"])
                                         , ")", collapse = "")
                                  ,x["text"])
                           )
    }
  }
  
  if(!"y" %in% names(dat)){
    dat$y <- 1 - dat$rolling*(1/(tsum+1))
  }
  
  
  grid.newpage()
  
  # boxes
  dat$box <- ""
  for(i in 1:nrow(dat)){
    box <- paste0("box", i)
    dat$box[i] <- box
    txt <- dat$text[i]
    x <- dat$x[i]
    tmp_y <- dat$y[i]
    width <- dat$width[i]
    just <- dat$just[i]
    if(!grepl("^expression", txt)) print(assign(box, boxGrob(txt, x=x, y=tmp_y, box_gp = gp, width = width, just = just)))
    if(grepl("^expression", txt)) print(assign(box, boxGrob(eval(parse(text = txt)), x=x, y=tmp_y, box_gp = gp, width = width, just = just)))
    
  }
  
  # connectors
  dat$group[is.na(dat$group)] <- ifelse(exists("dat$group"), min(dat$group, na.rm = TRUE)-1, 0)
  for(level in unique(dat$group)){
    dat0 <- dat[dat$group == level & dat$incexc == "inc", ]
    if(nrow(dat0) > 1){
      for(i in 2:nrow(dat0)){
        h <- i - 1
        print(connectGrob(eval(parse(text = paste0("box", dat0$row[h]))),
                          eval(parse(text = paste0("box", dat0$row[i]))), "N", arrow_obj = arrow_obj))
      }
    }
  }
  
  ## exclusion lines
  for(row in which(dat$incexc == "exc")){
    tmp <- dat[dat$group == dat$group[row] & dat$incexc == "inc", ]
    b1 <- tmp$box[max(which(tmp$row < row))] 
    b2 <- dat$box[row]
    mrow <- max(dat$row[dat$group == dat$group[row]])
    if(row < mrow){
      print(connectGrob(eval(parse(text = b1)),
                        eval(parse(text = b2)), "-", arrow_obj = arrow_obj))
    } else {
      print(connectGrob(eval(parse(text = b1)),
                        eval(parse(text = b2)), type = term_arrow_type, arrow_obj = arrow_obj))
    }
  }
  
  ## rando
  boxrando <- dat$box[max(dat$row[dat$group == min(dat$group)])]
  tmp <- dat[dat$group > min(dat$group) & dat$nth == 1, ]
  if(nrow(tmp) > 0){
    for(row in 1:nrow(tmp)){
      print(connectGrob(eval(parse(text = boxrando)),
                        eval(parse(text = tmp$box[row])), "N", arrow_obj = arrow_obj))
    }
  }
  if(tab) return(dat[,c("level", "group", "text", "incexc", "x", "y", "width")])  
}


















