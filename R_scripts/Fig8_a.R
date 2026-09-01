################################################################################
###
### AUTHOR:       Filippo Felice Boggetti
### DATE:         Spring 2024
### DESCRIPTION:  Replication of Figure 8.1 (upper part) 
###               
### OUTPUT:       one Figure: stored in "plots"
###               
################################################################################

## -----------------------------------------------------------------------------
rm(list=ls())                                                                   # Clear the environment 
## -----------------------------------------------------------------------------
Dataset<-read_rds("Database.RDS/Dataset_monthly_global.rds")

Dataset$Date<-as.Date(Dataset$Date, format="%Y:%Q")

plot <- ggplot(Dataset, aes(x=Date)) +
  geom_line(aes(y=W1_PT_gr), color="blue", size=1.25) +  
  geom_line(aes(y=W2_PT_gr), color="red", linetype="dotdash", size=1.25) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits=c(0, 0.5), oob=scales::rescale_none) + 
  labs(title="Weights of Greece in the determination of PT Global Spreads",
       subtitle="Distance in terms of government deficit and debt") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_vline(xintercept = as.Date(c("2002-04-01", "2005-10-01")), color="grey", linetype="solid") +
  annotate("rect", xmin = as.Date("2002-04-01"), xmax = as.Date("2002-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("rect", xmin = as.Date("2005-10-01"), xmax = as.Date("2005-12-31"), ymin = -Inf, ymax = Inf, alpha = 0.2)

#print(plot)

if (!dir.exists("plots")) {
  dir.create("plots")
}

ggsave(filename = "plots/Fig8_a.pdf", plot = plot, device = "pdf", width = 8, height = 6)
































