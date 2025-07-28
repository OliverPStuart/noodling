
# Data downloaded from:
# https://www.premiertimingsystems.ie/2025-Race-Results/Cork-Front-Runners-Pride-5k-Results

# Formatted manually

library(ggplot2)
library(magrittr)
library(dplyr)

results <- read.delim("2025_pride_run_results.txt")
results$Minutes <- results$Seconds/60
results$Pace <- results$Minutes/5

runners <- results %>% filter(Category=="Runners") %>%
  mutate(FRBW=ifelse(Club.Location == "FrontRunners Cork AC",T,F))

overall_mean = mean(runners$Minutes)
frbw_mean = mean(runners$Minutes[runners$FRBW])

ggplot() +
  geom_histogram(runners %>% filter(FRBW==F),
                 mapping=aes(x=Minutes),
                 fill="lightgrey",colour="lightgrey",
                 bins=25,position="identity") + 
  geom_histogram(runners %>% filter(FRBW),
                 mapping=aes(x=Minutes,fill=cut(Minutes,25)),
                 bins=25,position="identity") +
  geom_vline(xintercept=c(mean_times$Mean),linewidth=0.8,linetype="dashed") +
  scale_x_continuous(limits=c(15,60),expand=c(0,0),
                     breaks=c(15,seq(20,60,10))) + 
  scale_y_continuous(limits=c(0,52),expand=c(0,0)) + 
  theme(panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.line.x=element_line(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="aliceblue"),
        legend.background=element_blank(),
        legend.position="none",
        plot.margin = unit(c(2,2,1,2), "cm"),
        plot.title = element_text(vjust=7,size=20,hjust=-0.18),
        plot.subtitle = element_text(vjust=11,size=12,hjust=-0.1)) + 
  labs(x="Chip time\n(minutes)",
       title="Cork Pride 5k race results",
       subtitle="Saturday 26 July, 2025",
       caption="Data: Premier Timing Systems") + 
  scale_fill_discrete(h = c(10, 360), c = 150, l = 80) + 
  annotate("text",x=25.5,y=46,
           label=paste0("FRBW average\n",
                        round(frbw_mean,1),
                        " minutes"),
           hjust=1) + 
  annotate("text",x=31.7,y=46,
           label=paste0("Overall average\n",
                        round(overall_mean,1),
                        " minutes"),
           hjust=0)
