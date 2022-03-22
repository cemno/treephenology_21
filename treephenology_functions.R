# Plot PLS chill and forcing period
library(ggplot2)
plot_PLS_chill_force<-function(plscf,
                               chill_metric="Chill_Portions",
                               heat_metric="GDH",
                               chill_label="CP",
                               heat_label="GDH",
                               chill_phase=c(-48,62),
                               heat_phase=c(-5,105.5))
{
  PLS_gg<-plscf[[chill_metric]][[heat_metric]]$PLS_summary
  PLS_gg[,"Month"]<-trunc(PLS_gg$Date/100)
  PLS_gg[,"Day"]<-PLS_gg$Date-PLS_gg$Month*100
  PLS_gg[,"Date"]<-ISOdate(2002,PLS_gg$Month,PLS_gg$Day)
  PLS_gg[which(PLS_gg$JDay<=0),"Date"]<-ISOdate(2001,PLS_gg$Month[which(PLS_gg$JDay<=0)],PLS_gg$Day[which(PLS_gg$JDay<=0)])
  PLS_gg[,"VIP_importance"]<-PLS_gg$VIP>=0.8
  PLS_gg[,"VIP_Coeff"]<-factor(sign(PLS_gg$Coef)*PLS_gg$VIP_importance)
  
  chill_start_date<-ISOdate(2001,12,31)+chill_phase[1]*24*3600
  chill_end_date<-ISOdate(2001,12,31)+chill_phase[2]*24*3600
  heat_start_date<-ISOdate(2001,12,31)+heat_phase[1]*24*3600
  heat_end_date<-ISOdate(2001,12,31)+heat_phase[2]*24*3600
  
  
  temp_plot<- ggplot(PLS_gg) +
    annotate("rect",
             xmin = chill_start_date,
             xmax = chill_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "blue") +
    annotate("rect",
             xmin = heat_start_date,
             xmax = heat_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "red") +
    annotate("rect",
             xmin = ISOdate(2001,12,31) + min(plscf$pheno$pheno,na.rm=TRUE)*24*3600,
             xmax = ISOdate(2001,12,31) + max(plscf$pheno$pheno,na.rm=TRUE)*24*3600,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "black") +
    geom_vline(xintercept = ISOdate(2001,12,31) + median(plscf$pheno$pheno,na.rm=TRUE)*24*3600, linetype = "dashed") +
    geom_ribbon(aes(x=Date,
                    ymin=MetricMean - MetricStdev ,
                    ymax=MetricMean + MetricStdev ),
                fill="grey") +
    geom_ribbon(aes(x=Date,
                    ymin=MetricMean - MetricStdev * (VIP_Coeff==-1),
                    ymax=MetricMean + MetricStdev * (VIP_Coeff==-1)),
                fill="red") +
    geom_ribbon(aes(x=Date,
                    ymin=MetricMean - MetricStdev * (VIP_Coeff==1),
                    ymax=MetricMean + MetricStdev * (VIP_Coeff==1)),
                fill="dark green") +
    geom_line(aes(x=Date,y=MetricMean )) +
    facet_wrap(vars(Type), scales = "free_y",
               strip.position="left",
               labeller = labeller(Type = as_labeller(c(Chill=paste0("Chill (",chill_label,")"),Heat=paste0("Heat (",heat_label,")")) )) ) +
    ggtitle("Daily chill and heat accumulation rates") +
    theme_bw(base_size=15) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(size =12),
          plot.title = element_text(hjust = 0.5),
          axis.title.y=element_blank()
    )
  
  VIP_plot<- ggplot(PLS_gg,aes(x=Date,y=VIP)) +
    annotate("rect",
             xmin = chill_start_date,
             xmax = chill_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "blue") +
    annotate("rect",
             xmin = heat_start_date,
             xmax = heat_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "red") +
    annotate("rect",
             xmin = ISOdate(2001,12,31) + min(plscf$pheno$pheno,na.rm=TRUE)*24*3600,
             xmax = ISOdate(2001,12,31) + max(plscf$pheno$pheno,na.rm=TRUE)*24*3600,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "black") +
    geom_vline(xintercept = ISOdate(2001,12,31) + median(plscf$pheno$pheno,na.rm=TRUE)*24*3600, linetype = "dashed") +
    geom_bar(stat='identity',aes(fill=VIP>0.8)) +
    facet_wrap(vars(Type), scales="free",
               strip.position="left",
               labeller = labeller(Type = as_labeller(c(Chill="VIP for chill",Heat="VIP for heat") )) ) +
    scale_y_continuous(limits=c(0,max(plscf[[chill_metric]][[heat_metric]]$PLS_summary$VIP))) +
    ggtitle("Variable Importance in the Projection (VIP) scores") +
    theme_bw(base_size=15) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(size =12),
          plot.title = element_text(hjust = 0.5),
          axis.title.y=element_blank()
    ) +
    scale_fill_manual(name="VIP", 
                      labels = c("<0.8", ">0.8"), 
                      values = c("FALSE"="grey", "TRUE"="blue")) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  coeff_plot<- ggplot(PLS_gg,aes(x=Date,y=Coef)) +
    annotate("rect",
             xmin = chill_start_date,
             xmax = chill_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "blue") +
    annotate("rect",
             xmin = heat_start_date,
             xmax = heat_end_date,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "red") +
    annotate("rect",
             xmin = ISOdate(2001,12,31) + min(plscf$pheno$pheno,na.rm=TRUE)*24*3600,
             xmax = ISOdate(2001,12,31) + max(plscf$pheno$pheno,na.rm=TRUE)*24*3600,
             ymin = -Inf,
             ymax = Inf,
             alpha = .1,fill = "black") +
    geom_vline(xintercept = ISOdate(2001,12,31) + median(plscf$pheno$pheno,na.rm=TRUE)*24*3600, linetype = "dashed") +
    geom_bar(stat='identity',aes(fill=VIP_Coeff)) +
    facet_wrap(vars(Type), scales="free",
               strip.position="left",
               labeller = labeller(Type = as_labeller(c(Chill="MC for chill",Heat="MC for heat") )) ) +
    scale_y_continuous(limits=c(min(plscf[[chill_metric]][[heat_metric]]$PLS_summary$Coef),
                                max(plscf[[chill_metric]][[heat_metric]]$PLS_summary$Coef))) +
    ggtitle("Model coefficients (MC)") +
    theme_bw(base_size=15) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.y = element_text(size =12),
          plot.title = element_text(hjust = 0.5),
          axis.title.y=element_blank()
    ) +
    scale_fill_manual(name="Effect direction", 
                      labels = c("Advancing", "Unimportant","Delaying"), 
                      values = c("-1"="red", "0"="grey","1"="dark green")) +
    ylab("PLS coefficient") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  library(patchwork)
  
  plot<- (VIP_plot +
            coeff_plot +
            temp_plot +
            plot_layout(ncol=1,
                        guides = "collect")
  ) & theme(legend.position = "right",
            legend.text = element_text(size=8),
            legend.title = element_text(size=10),
            axis.title.x=element_blank())
  
  plot
  
}
ggplot_PLS <- function(PLS_results)
{
  library(ggplot2)
  PLS_gg <- PLS_results$PLS_summary
  PLS_gg[, "Month"] <- trunc(PLS_gg$Date / 100)
  PLS_gg[, "Day"] <- PLS_gg$Date - PLS_gg$Month * 100
  PLS_gg[, "Date"] <- ISOdate(2002, PLS_gg$Month, PLS_gg$Day)
  PLS_gg[which(PLS_gg$JDay <= 0), "Date"] <-
    ISOdate(2001,
            PLS_gg$Month[which(PLS_gg$JDay <= 0)],
            PLS_gg$Day[which(PLS_gg$JDay <= 0)])
  PLS_gg[, "VIP_importance"] <- PLS_gg$VIP >= 0.8
  PLS_gg[, "VIP_Coeff"] <-
    factor(sign(PLS_gg$Coef) * PLS_gg$VIP_importance)
  
  VIP_plot <- ggplot(PLS_gg, aes(x = Date, y = VIP)) +
    geom_bar(stat = 'identity', aes(fill = VIP > 0.8)) +
    scale_fill_manual(
      name = "VIP",
      labels = c("<0.8", ">0.8"),
      values = c("FALSE" = "grey", "TRUE" = "blue")
    ) +
    theme_bw(base_size = 15) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
  
  coeff_plot <- ggplot(PLS_gg, aes(x = Date, y = Coef)) +
    geom_bar(stat = 'identity', aes(fill = VIP_Coeff)) +
    scale_fill_manual(
      name = "Effect direction",
      labels = c("Advancing", "Unimportant", "Delaying"),
      values = c("-1" = "red", "0" = "grey", "1" = "dark green")
    ) +
    theme_bw(base_size = 15) +
    ylab("PLS coefficient") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
  
  temp_plot <- ggplot(PLS_gg) +
    geom_ribbon(aes(
      x = Date,
      ymin = Tmean - Tstdev,
      ymax = Tmean + Tstdev
    ),
    fill = "grey") +
    geom_ribbon(aes(
      x = Date,
      ymin = Tmean - Tstdev * (VIP_Coeff == -1),
      ymax = Tmean + Tstdev * (VIP_Coeff == -1)
    ),
    fill = "red") +
    geom_ribbon(aes(
      x = Date,
      ymin = Tmean - Tstdev * (VIP_Coeff == 1),
      ymax = Tmean + Tstdev * (VIP_Coeff == 1)
    ),
    fill = "dark green") +
    geom_line(aes(x = Date, y = Tmean)) +
    theme_bw(base_size = 15) +
    ylab(expression(paste(T[mean], " (°C)")))
  
  library(patchwork)
  plot <- (VIP_plot +
             coeff_plot +
             temp_plot +
             plot_layout(ncol = 1,
                         guides = "collect")) &
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      axis.title.x = element_blank()
    )
  
  plot
}

plot_scenarios_gg<-function(past_observed,
                            past_simulated,
                            future_data,
                            metric,
                            axis_label)
{
  rng<-range(past_observed[[metric]],
             past_simulated[[metric]],
             future_data[[metric]])  
  past_plot<-ggplot() +
    geom_boxplot(data = past_simulated,
                 aes_string("as.numeric(Year)",metric,group="Year"),
                 fill="skyblue") +
    scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
    labs(x = "Year", y = axis_label) +
    facet_grid(~ Scenario) +
    theme_bw(base_size = 15) +  
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle=45, hjust=1)) +
    geom_point(data = past_observed,
               aes_string("End_year",metric),
               col="blue")
  
  future_plot_list<-list()
  
  for(y in c(2050,2085))
  {
    future_plot_list[[which(y == c(2050,2085))]] <-
      ggplot(data= future_data[which(future_data$Year==y),]) +
      geom_boxplot(aes_string("GCM", metric, fill="GCM")) +
      facet_wrap(vars(RCP)) +
      scale_x_discrete(labels = NULL, expand = expansion(add = 1)) +
      scale_y_continuous(limits = c(0, round(round(1.1*rng[2])))) +
      geom_text_npc(aes(npcx = "center", npcy = "top", label = Year),
                    size = 5) +
      theme_bw(base_size = 15) +
      theme(axis.ticks.y = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.margin = margin(0, 0, 0, 0, "cm"),
            legend.background = element_rect(),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold"),
            legend.box.spacing = unit(0, "cm"),
            plot.subtitle = element_text(hjust = 0.5,
                                         vjust = -1,
                                         size = 15 * 1.05,
                                         face = "bold")) 
  }
  
  plot<- (past_plot +
            future_plot_list +
            plot_layout(guides = "collect",
                        widths = c(1,rep(1.8,length(future_plot_list))))
  ) & theme(legend.position = "bottom",
            legend.text = element_text(size=8),
            legend.title = element_text(size=10),
            axis.title.x=element_blank())
  plot
  
}