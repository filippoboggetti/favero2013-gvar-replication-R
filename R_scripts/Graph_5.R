################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  This script generate the graph 5 in the paper of Favero (2013).
###
### OUTPUT:       One graph Fig.5 stored in the folder "plots"
###               
################################################################################


## -----------------------------------------------------------------------------

rm(list=ls())

Dataset<-read_rds("Database.RDS/Dataset_monthly_global.rds")

figure5<-ggplot() +
  geom_line(data = Dataset, aes(x = Date, y = glob_sp2_ir, 
                                color = "Ireland", linetype = "Ireland"), 
            size = 1.25) +
  geom_line(data = Dataset, aes(x = Date, y = glob_sp2_nl,
                                color = "Netherlands", linetype = "Netherlands"), 
            size = 1.25) +
  labs(title = "Global Spreads based on the debt/GDP distance") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box = "horizontal", 
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = c("Ireland" = "red", "Netherlands" = "blue")) +
  scale_linetype_manual(values = c("Ireland" = "solid", "Netherlands" = "dashed")) +
  guides(
    color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))),
    linetype = FALSE 
  ) +
  geom_vline(xintercept = as.Date("2007-01-07"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2009-01-09"), linetype = "dashed", color = "black")

folder_name <- "plots"
folder_path <- file.path(getwd(), folder_name)

file_name <- file.path(folder_path, "Figure5.pdf")

ggsave(filename = file_name, plot = figure5, width = 15, height = 6, dpi = 300)

rm(list=ls())

## -----------------------------------------------------------------------------







