
##################################################################
##                         Make Figures                         ##
##################################################################


# Load packages -----------------------------------------------------------

library(patchwork)

# Self Report -------------------------------------------------------------
# Source files ------------------------------------------------------------

source("code/analysis/arousal_ratings.R")
source("code/analysis/arousal_ratings.R")


# Create plot -------------------------------------------------------------

self_report <- v_plot + a_plot + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

self_report

ggsave("output/Self_report_plot.png",
       height = 3,
       width = 5)
