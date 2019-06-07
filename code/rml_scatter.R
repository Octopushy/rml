library(tidyverse)
library(extrafont)
library(ggrepel)
library(cowplot)
library(gridExtra)
library(grid)

rml <- read_csv("data/clean/rml_pm_tob_07_17.csv") %>% 
    select(-c(se_12_17:se_26_plus)) %>% 
    gather(key = age_grp, value = prev, `12_17`:`26_plus`) %>% 
    mutate(pass = ifelse(rml == "never", "no", "yes"), 
           rml = as_factor(rml), 
           rml = fct_relevel(rml, "before", "after", "never"), 
           rml = fct_relabel(rml, Hmisc::capitalize)) 
    

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), 
                                       nrow = 1, position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
}

rml_avg <- rml %>% 
    group_by(year, rml, age_grp) %>% 
    summarize(prev = mean(prev)) %>% 
    mutate(state = "avg")

rml_colors <- c("#FFA300", "#AE2573", "gray50")

no_rml <- ggplot(data = subset(rml, pass == "no" & age_grp == "12_17"), 
                 aes(x = year, y = prev, group = state)) +
    geom_line(aes(color = rml), alpha = 0.3) + 
    geom_line(data = subset(rml_avg, rml == "Never" & age_grp == "12_17"), 
              aes(x = year, y = prev), color = "black", size = 1.1) + 
    scale_color_manual(values = rml_colors[[3]]) + 
    scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017))

no_rml_18 <- ggplot(data = subset(rml, pass == "no" & age_grp == "18_25"), aes(x = year, y = prev, group = state)) +
    geom_line(aes(color = rml), alpha = 0.3) + 
    geom_line(data = subset(rml_avg, rml == "Never" & age_grp == "18_25"), 
              aes(x = year, y = prev), color = "black", size = 1.1) + 
    scale_color_manual(values = rml_colors[[3]]) + 
    scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017))

no_rml_26 <- ggplot(data = subset(rml, pass == "no" & age_grp == "26_plus"), aes(x = year, y = prev, group = state)) +
    geom_line(aes(color = rml), alpha = 0.3) + 
    geom_line(data = subset(rml_avg, rml == "Never" & age_grp == "26_plus"), 
              aes(x = year, y = prev), color = "black", size = 1.1) + 
    scale_color_manual(values = rml_colors[[3]]) + 
    scale_x_continuous(breaks = c(2007, 2009, 2011, 2013, 2015, 2017))

gg_12 <- no_rml + 
    geom_line(data = subset(rml, pass == "yes" & age_grp == "12_17"), 
                aes(x = year, y = prev, group = state, color = rml)) + 
    scale_color_manual(values = rml_colors, breaks = c("Before", "After", "Never")) + 
    theme_minimal() + 
    labs(y = "Past-month use",  
         color = "RML status", 
         subtitle = "12-17 year-olds") + 
    theme(text = element_text(family = "Roboto Condensed", size = 33), 
          axis.title.x = element_blank(), 
          plot.margin = unit(c(5.5, 5.5, 10, 5.5), "pt"))

gg_18 <- no_rml_18 + 
    geom_line(data = subset(rml, pass == "yes" & age_grp == "18_25"), 
                aes(x = year, y = prev, color = rml, group = state)) + 
    scale_color_manual(values = rml_colors, breaks = c("Before", "After", "Never")) +
    # scale_y_continuous(breaks = c(10, 20, 30)) + 
    theme_minimal() + 
    labs(color = "RML status", 
         subtitle = "18-25 year-olds") + 
    theme(text = element_text(family = "Roboto Condensed", size = 33), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.margin = unit(c(5.5, 5.5, 10, 5.5), "pt"))

gg_26 <- no_rml_26 + 
    geom_line(data = subset(rml, pass == "yes" & age_grp == "26_plus"), 
                aes(x = year, y = prev, color = rml, group = state)) + 
    scale_color_manual(values = rml_colors, breaks = c("Before", "After", "Never")) +
    theme_minimal() + 
    labs(x = "\nYear", 
         y = "Past-month use",
         color = "RML status", 
         subtitle = "26+ year-olds") + 
    theme(text = element_text(family = "Roboto Condensed", size = 33))

# rml_cleve <- rml %>%
#     filter(age_grp == "12_17", 
#            rml != "never") %>% 
#     group_by(rml, state) %>% 
#     summarize(avg = mean(prev)) %>% 
#     ggplot(aes(x = avg, y = fct_reorder(state, avg), color = rml)) + 
#     geom_line(aes(group = state), color = "black") + 
#     geom_point(size = 3) + 
#     scale_color_manual(values = rml_colors) + 
#     theme_minimal() + 
#     theme(text = element_text(family = "Roboto Condensed", size = 30)) + 
#     labs(x = "Past-month use, 12-17 year-olds", 
#          y = "")

ggsave("results/scatter_grid_tob.png", 
       plot = grid_arrange_shared_legend(gg_12, gg_18, gg_26, ncol = 2, nrow = 2), 
       dpi = 600, width = 14, height = 11)

# rml_12 <- rml_12 %>% 
#     group_by(state) %>% 
#     summarise(prev = max(prev)) %>% 
#     mutate(max = "yes") %>% 
#     right_join(rml_12)
# 
# no_rml + 
#     geom_jitter(data = subset(rml_12, pass == "yes"), aes(x = year, y = prev, color = rml)) + 
#     geom_text_repel(data = subset(rml_12, pass == "yes" & max == "yes" & state != "Nevada"), 
#                     mapping = aes(label = state), size = 3, segment.size = 0, force = 1) + 
#     scale_color_manual(values = rml_colors) +
#     theme_minimal() + 
#     labs(x = "Year", 
#          y = "Past-month use", 
#          color = "RML status", 
#          title = "Marijuana use, 2007-17",
#          caption = "Gray states do not have RML")
