# Plot PLS chill and forcing period
library(ggplot2)
apply_const_temp <-
  function(temp, A0, A1, E0, E1, Tf, slope, portions=1200, deg_celsius=TRUE)
  {
    temp_vector <- rep(temp, times=portions)
    res <- chillR::DynModel_driver(temp=temp_vector,
                                   A0=A0, A1=A1,
                                   E0=E0, E1=E1,
                                   Tf=Tf,
                                   slope=slope,
                                   deg_celsius=deg_celsius)
    return(res$y[length(res$y)])
  }

gen_bell <- function(par, temp_values=seq(-5, 20, 0.1)) {
  E0 <- par[5]
  E1 <- par[6]
  A0 <- par[7]
  A1 <- par[8]
  Tf <- par[9]
  slope <- par[12]
  
  y <- c()
  for(i in seq_along(temp_values)) {
    y[i] <- apply_const_temp(temp=temp_values[i],
                             A0=A0, A1=A1, E0=E0, E1=E1, Tf=Tf, slope=slope)
  }
  return(invisible(y))
}

GDH_response<-function(T, par)
{Tb<-par[11]
Tu<-par[4]
Tc<-par[10]
GDH_weight <- rep(0, length(T))
GDH_weight[which(T >= Tb & T <= Tu)] <-
  1/2 * (1 + cos(pi + pi * (T[which(T >= Tb & T <= Tu)] - Tb)/(Tu - Tb)))
GDH_weight[which(T > Tu & T <= Tc)] <-
  (1 + cos(pi/2 + pi/2 * (T[which(T >  Tu & T <= Tc)] -Tu)/(Tc - Tu)))
return(GDH_weight)
}
pheno_trend_ggplot<-function(temps,
                             pheno,
                             chill_phase,
                             heat_phase,
                             phenology_stage="Bloom")
{
  library(fields)
  library(reshape2)
  library(metR)
  library(ggplot2)
  library(colorRamps)
  
  # first, a sub-function (function defined within a function) to
  # compute the temperature means
  
  mean_temp_period<-function(temps,
                             start_JDay,
                             end_JDay, 
                             end_season = end_JDay)
  { temps_JDay<-make_JDay(temps)
  temps_JDay[,"Season"]<-temps_JDay$Year
  if(start_JDay>end_season)
    temps_JDay$Season[which(temps_JDay$JDay>=start_JDay)]<-
    temps_JDay$Year[which(temps_JDay$JDay>=start_JDay)]+1
  if(start_JDay>end_season)
    sub_temps<-subset(temps_JDay,JDay<=end_JDay|JDay>=start_JDay)
  if(start_JDay<=end_JDay)
    sub_temps<-subset(temps_JDay,JDay<=end_JDay&JDay>=start_JDay)
  mean_temps<-aggregate(sub_temps[,c("Tmin","Tmax")],
                        by=list(sub_temps$Season),
                        FUN=function(x) mean(x, na.rm=TRUE))
  mean_temps[,"n_days"]<-aggregate(sub_temps[,"Tmin"],
                                   by=list(sub_temps$Season),
                                   FUN=length)[,2]
  mean_temps[,"Tmean"]<-(mean_temps$Tmin+mean_temps$Tmax)/2
  mean_temps<-mean_temps[,c(1,4,2,3,5)]
  colnames(mean_temps)[1]<-"End_year"
  return(mean_temps)
  }
  
  mean_temp_chill<-mean_temp_period(temps = temps,
                                    start_JDay = chill_phase[1],
                                    end_JDay = chill_phase[2],
                                    end_season = heat_phase[2])
  
  mean_temp_heat<-mean_temp_period(temps = temps,
                                   start_JDay = heat_phase[1],
                                   end_JDay = heat_phase[2],
                                   end_season = heat_phase[2])
  
  mean_temp_chill<-
    mean_temp_chill[which(mean_temp_chill$n_days >= 
                            max(mean_temp_chill$n_days)-1),]
  mean_temp_heat<-
    mean_temp_heat[which(mean_temp_heat$n_days >= 
                           max(mean_temp_heat$n_days)-1),]
  mean_chill<-mean_temp_chill[,c("End_year","Tmean")]
  colnames(mean_chill)[2]<-"Tmean_chill"
  mean_heat<-mean_temp_heat[,c("End_year","Tmean")]
  colnames(mean_heat)[2]<-"Tmean_heat"
  phase_Tmeans<-merge(mean_chill,mean_heat, by="End_year")
  
  colnames(pheno)<-c("End_year","pheno")
  Tmeans_pheno<-merge(phase_Tmeans,pheno, by="End_year")
  
  # Kriging interpolation
  k<-Krig(x=as.matrix(Tmeans_pheno[,c("Tmean_chill","Tmean_heat")]),
          Y=Tmeans_pheno$pheno)
  pred<-predictSurface(k)
  colnames(pred$z)<-pred$y
  rownames(pred$z)<-pred$x
  melted<-melt(pred$z)
  colnames(melted)<-c("Tmean_chill","Tmean_heat","value")
  
  ggplot(melted,aes(x=Tmean_chill,y=Tmean_heat,z=value)) +
    geom_contour_fill(bins=60) +
    scale_fill_gradientn(colours=alpha(matlab.like(15)),
                         name=paste(phenology_stage,"date \n(day of the year)")) +
    geom_contour(col="black") +
    geom_text_contour(stroke = 0.2) +
    geom_point(data=Tmeans_pheno,
               aes(x=Tmean_chill,y=Tmean_heat,z=NULL),
               size=0.7)  +
    ylab(expression(paste("Forcing phase ", T[mean]," (",degree,"C)"))) +
    xlab(expression(paste("Chilling phase ", T[mean]," (",degree,"C)"))) +
    theme_bw(base_size=15)
}

Chill_model_sensitivity<-
  function(latitude,
           temp_models=list(Dynamic_Model=Dynamic_Model,GDH=GDH),
           month_range=c(10,11,12,1,2,3),
           Tmins=c(-10:20),
           Tmaxs=c(-5:30))
  {
    mins<-NA
    maxs<-NA
    metrics<-as.list(rep(NA,length(temp_models)))
    names(metrics)<-names(temp_models)
    month<-NA
    
    for(mon in month_range)
    {
      days_month<-as.numeric(difftime( ISOdate(2002,mon+1,1),
                                       ISOdate(2002,mon,1) ))
      if(mon==12) days_month<-31
      weather<-make_all_day_table(data.frame(Year=c(2001,2002),
                                             Month=c(mon,mon),
                                             Day=c(1,days_month),
                                             Tmin=c(0,0),Tmax=c(0,0)))
      
      
      for(tmin in Tmins)
        for(tmax in Tmaxs)
          if(tmax>=tmin)
          {
            weather$Tmin<-tmin
            weather$Tmax<-tmax
            hourtemps<-stack_hourly_temps(weather,
                                          latitude=latitude)$hourtemps$Temp
            for(tm in 1:length(temp_models))
              metrics[[tm]]<-c(metrics[[tm]],
                               do.call(temp_models[[tm]],
                                       list(hourtemps))[length(hourtemps)]/
                                 (length(hourtemps)/24))
            mins<-c(mins,tmin)
            maxs<-c(maxs,tmax)
            month<-c(month,mon)
          }
    }
    results<-cbind(data.frame(Month=month,Tmin=mins,Tmax=maxs),
                   as.data.frame(metrics))
    results<-results[!is.na(results$Month),]
  }


Chill_sensitivity_temps<-function(chill_model_sensitivity_table,
                                  temperatures,
                                  temp_model,
                                  month_range=c(10,11,12,1,2,3),
                                  Tmins=c(-10:20),
                                  Tmaxs=c(-5:30),
                                  legend_label="Chill/day (CP)")
{
  library(ggplot2)
  library(colorRamps)
  
  cmst<-chill_model_sensitivity_table
  cmst<-cmst[which(cmst$Month %in% month_range),]
  cmst$Month_names<- factor(cmst$Month, levels=month_range,
                            labels=month.name[month_range])  
  
  DM_sensitivity<-
    ggplot(cmst,
           aes_string(x="Tmin",y="Tmax",fill=temp_model)) +
    geom_tile() +
    scale_fill_gradientn(colours=alpha(matlab.like(15), alpha = .5),
                         name=legend_label) +
    xlim(Tmins[1],Tmins[length(Tmins)]) +
    ylim(Tmaxs[1],Tmaxs[length(Tmaxs)])
  
  temperatures<-
    temperatures[which(temperatures$Month %in% month_range),]
  temperatures[which(temperatures$Tmax<temperatures$Tmin),
               c("Tmax","Tmin")]<-NA
  temperatures$Month_names <-
    factor(temperatures$Month,
           levels=month_range,
           labels=month.name[month_range])  
  
  DM_sensitivity +
    geom_point(data=temperatures,
               aes(x=Tmin,y=Tmax,fill=NULL,color="Temperature"),
               size=0.1) +
    facet_wrap(vars(Month_names)) +
    scale_color_manual(values = "black",
                       labels = "Daily temperature \nextremes (°C)",
                       name="Observed at site" ) +
    guides(fill = guide_colorbar(order = 1),
           color = guide_legend(order = 2)) +
    ylab("Tmax (°C)") +
    xlab("Tmin (°C)") + 
    theme_bw(base_size=15)
  
}
plot_PLS_chill_force<-function(plscf,
                               chill_metric="Chill_Portions",
                               heat_metric="GDH",
                               chill_label="CP",
                               heat_label="GDH",
                               chill_phase=c(0,0),
                               heat_phase=c(0,0))
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