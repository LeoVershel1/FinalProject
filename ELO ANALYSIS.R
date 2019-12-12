# NFL ELO ANALYSIS PROJECT

library(ggplot2)
library(ggpubr)

setwd("/Users/leovershel/Desktop/stuff for cs")

blue_color=c("#1f5179")

HISTORY_ELO.data = read.csv("nfl_elo.csv")  #importing dataset
LATEST_ELO.data = read.csv("nfl_elo_latest.csv")  #importing dataset

#creating point and elo differential stats
LATEST_ELO.data$LATEST_ELO_DIFFERENTIAL = LATEST_ELO.data$elo1_pre - LATEST_ELO.data$elo2_pre 
LATEST_ELO.data$LATEST_PT_DIFFERENTIAL = LATEST_ELO.data$score1 - LATEST_ELO.data$score2 
HISTORY_ELO.data$HISTORY_ELO_DIFFERENTIAL = HISTORY_ELO.data$elo1_pre - HISTORY_ELO.data$elo2_pre
HISTORY_ELO.data$HISTORY_PT_DIFFERENTIAL = HISTORY_ELO.data$score1 - HISTORY_ELO.data$score2

#creating my 2007 patriots data subset
PATS2007_ELO.data = subset(HISTORY_ELO.data, HISTORY_ELO.data$season=='2007' & (HISTORY_ELO.data$team1=='NE' | HISTORY_ELO.data$team2=='NE'))
PATS2007_ELO.data$ABS_HISTORY_ELO_DIFFERENTIAL = abs(PATS2007_ELO.data$elo1_pre - PATS2007_ELO.data$elo2_pre)
PATS2007_ELO.data$ABS_HISTORY_PT_DIFFERENTIAL = abs(PATS2007_ELO.data$score1 - PATS2007_ELO.data$score2)

#creating my 2018-19 eagles data subset
PHI_L_ELO.data = subset(LATEST_ELO.data, LATEST_ELO.data$team1=='PHI' | LATEST_ELO.data$team2=='PHI')
PHI_L_ELO.data$LATEST_ELO_DIFFERENTIAL = PHI_L_ELO.data$elo1_pre - PHI_L_ELO.data$elo2_pre
PHI_L_ELO.data$LATEST_PT_DIFFERENTIAL = PHI_L_ELO.data$score1 - PHI_L_ELO.data$score2

#graphing 2007 patriots
ggplot(PATS2007_ELO.data, aes(x = ABS_HISTORY_ELO_DIFFERENTIAL, y = ABS_HISTORY_PT_DIFFERENTIAL)) + geom_point(color=blue_color,size=4) +
  labs(title = "NE Patriots 2007-08 Season, 18W-1L", x= "ELO Differential", y= "Point Differential") +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), 
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        panel.background = element_rect(fill="#FFFFFF", size=2),
        panel.grid.major = element_line(color="#000000", size=0.5), 
        panel.grid.minor = element_line(color="#000000", size=0.25),  
  )

#graphing 20018-19 eagles
ggplot(PHI_L_ELO.data, aes(x = LATEST_ELO_DIFFERENTIAL, y = LATEST_PT_DIFFERENTIAL)) + geom_point(color=blue_color,size=4) +
  labs(title = "Phillidelphia Eagles 2018-19 Season, 9W-7L", x= "ELO Differential", y= "Point Differential") +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), 
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        panel.background = element_rect(fill="#FFFFFF", size=2),
        panel.grid.major = element_line(color="#000000", size=0.5), 
        panel.grid.minor = element_line(color="#000000", size=0.25),  
        )

#graphing 2018-19 season
ggplot(LATEST_ELO.data, aes(x = LATEST_ELO_DIFFERENTIAL, y = LATEST_PT_DIFFERENTIAL)) + geom_point(color=blue_color,size=4) +
  labs(title = "NFL 2018-19 SEASON", x= "ELO Differential", y= "Point Differential") +
  stat_smooth(method=lm) +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), 
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        panel.background = element_rect(fill="#FFFFFF", size=2),
        panel.grid.major = element_line(color="#000000", size=0.5), 
        panel.grid.minor = element_line(color="#000000", size=0.25),  
  )

#graphing nfl history
ggplot(HISTORY_ELO.data, aes(x = HISTORY_ELO_DIFFERENTIAL, y = HISTORY_PT_DIFFERENTIAL)) + geom_point(color=blue_color,size=0.25) +
  labs(title = "NFL HISTORY: Elo Differential vs. Point Differential", x= "ELO Differential", y= "Point Differential") +
  stat_smooth(method=lm) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5), 
        axis.text=element_text(size=16), 
        axis.title=element_text(size=16),
        panel.background = element_rect(fill="#FFFFFF", size=2),
        panel.grid.major = element_line(color="#000000", size=0.5), 
        panel.grid.minor = element_line(color="#000000", size=0.25),  
  )



#calculating p values
hist.p = compare_means(HISTORY_ELO_DIFFERENTIAL ~ HISTORY_PT_DIFFERENTIAL, data = HISTORY_ELO.data)
mean(hist.p$p)

late.p = compare_means(LATEST_ELO_DIFFERENTIAL ~ LATEST_PT_DIFFERENTIAL, data = LATEST_ELO.data)
mean(late.p$p)

phi.p = compare_means(LATEST_ELO_DIFFERENTIAL ~ LATEST_PT_DIFFERENTIAL, data = PHI_L_ELO.data)
mean(phi.p$p)

pats.p = compare_means(ABS_HISTORY_ELO_DIFFERENTIAL ~ ABS_HISTORY_PT_DIFFERENTIAL, data = PATS2007_ELO.data)
mean(pats.p$p)


#calculating r^2
eruption.lm = lm(HISTORY_ELO_DIFFERENTIAL ~ HISTORY_PT_DIFFERENTIAL, data = HISTORY_ELO.data)
summary(eruption.lm)$r.squared

eruption.lm = lm(LATEST_ELO_DIFFERENTIAL ~ LATEST_PT_DIFFERENTIAL, data = LATEST_ELO.data)
summary(eruption.lm)$r.squared

eruption.lm = lm(LATEST_ELO_DIFFERENTIAL ~ LATEST_PT_DIFFERENTIAL, data = PHI_L_ELO.data)
summary(eruption.lm)$r.squared

eruption.lm = lm(ABS_HISTORY_ELO_DIFFERENTIAL ~ ABS_HISTORY_PT_DIFFERENTIAL, data = PATS2007_ELO.data)
summary(eruption.lm)$r.squared






