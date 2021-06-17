#MissDelta
#plotting 10/21 profile data
#1/15/20


library("tidyverse")

library(readr)
october <- read_csv("Data/alldata_compiled_2019-10-21.csv")
#View(alldata_compiled_2019_10_21)

aug_main <- august[79:203, ]

#creating graphing shortcut
g <- ggplot(data = aug_main) + ggtitle("8/14/19")
+ theme(plot.title = element_text(size = 20))

#[NO3] vs cumulative distance
no3_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = no3_mgL_scan)) +
  labs(x = "distance (m)", y = "NO3 (mg/L)")

#[TOC] vs cumulative distance
toc_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = toc_mgL_scan)) +
  labs(x = "distance (m)", y = "TOC (mg/L)")

#[DOC] vs cumulative distance
doc_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = doc_mgL_scan)) +
  labs(x = "distance (m)", y = "DOC (mg/L)")

#temp vs cumulative distance
temp_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = temp_c)) +
  labs(x = "distance (m)", y = "temperature (?C)")

#[DO] vs cumulative distance
do_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = do_mgl)) +
  labs(x = "distance (m)", y = "DO (mg/L)")

#pH vs cumulative distance
ph_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = ph)) +
  labs(x = "distance (m)", y = "pH")

#fdom vs cumulative distance
fdom_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = fdom_qsu)) +
  labs(x = "distance (m)", y = "fdom")

#do percent vs cumulative distance
do_percent_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = do_per)) +
  labs(x = "distance (m)", y = "DO percent")

#turbidity NTU vs cumulative distance
turb_aug <- g + geom_line(mapping = aes(x = dist_cum_m, y = turb_NTU_scan)) +
  labs(x = "distance (m)", y = "turbidity")

library(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(no3_aug, toc_aug, doc_aug, temp_aug, do_aug, ph_aug, 
          fdom_aug, do_percent_aug, cols = 2)
