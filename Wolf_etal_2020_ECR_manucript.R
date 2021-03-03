# Title: GLM and plots for Wolf_etal_2020 ECR Manuscript
# Purpose: Generate Generalized Linear Model and plots
# Author: JW
# Input: Final dataset in .csv format
# Date: December 28 2020
# Output: GLM results and Figure 1

# Environment setup
pkgs<- c('tidyverse','ggplot2', 'extrafont', 'sjPlot', 'patchwork',
         'colorBlindness', 'hrbrthemes')
invisible(lapply(pkgs, require, character=TRUE))
dat<- read.csv("output/Dataset_Final.csv")

# Reducing dataset to relevant variables for analyses/plots
dat_reduce <- dat %>%
  select(c(Num_publications_first, Num_publications_last,
           Num_preprints_first, Num_preprints_last, Institution_size_first, 
           ECR_status_first_simplified, ECR_status_last_simplified))

# Ensuring all relevant data columns are factors
dat_reduce$Institution_size_first<- as.factor (dat_reduce$Institution_size_first)
dat_reduce$ECR_status_first_simplified<-as.factor(dat_reduce$ECR_status_first_simplified)
dat_reduce$ECR_status_last_simplified<- as.factor(dat_reduce$ECR_status_last_simplified)

# GLM
# Assume poission distribution if I leave response as # preprints and offset # of total publications as a predictor  
# Quasipoisson as poisson is over dispersed
model1<- glm(Num_preprints_first ~ ECR_status_first_simplified + 
           ECR_status_last_simplified+
           offset(log(Num_publications_first))+
           Institution_size_first+
           Institution_size_first*ECR_status_first_simplified,
         data=dat_reduce,
         family = quasipoisson)
summary(model1)

#Figure 1a
mycol <- c("#56B4E9", "#D55E00")
ggbox_1a <- ggplot(data=subset(dat_reduce,!is.na(gender_first)),
                   aes(x= ECR_status_first_simplified, y=Num_preprints_first,
                       fill=ECR_status_first_simplified))+
  geom_violin(draw_quantiles = c(.25, .5, .75, .95), alpha=0.5, scale= "count")+
  geom_boxplot(width=0.08, outlier.shape = NA, color = "gray40",fill = "white", 
               coef =0)+
  ylab ("Number of preprint articles") +
  xlab("")+
  scale_x_discrete(labels=c("ECR","Non-ECR"))+
  scale_fill_manual(values=mycol)+
  theme_classic(
    base_size = 15,
    base_family ="Times",
  )+
  theme(legend.position = "none")+
  annotate("text", label = "bold(A)", 
           x=0.5, y = 20, parse = TRUE,
           size = 7)

#Figure 1b
ggbox_1b <- ggplot(data=subset(dat_reduce,!is.na(gender_last)),
                   aes(ECR_status_last_simplified,Num_preprints_last,
                       fill=ECR_status_last_simplified))+
  geom_violin(draw_quantiles = c(.25, .5, .75, .95), alpha=0.5, scale= "count")+
  geom_boxplot(width=0.062, outlier.shape = NA, color = "gray40", fill = "white",
               coef =0)+
  ylab ("Number of preprint articles") +
  xlab ("")+
  scale_x_discrete(labels=c("ECR","Non-ECR"))+
  scale_fill_manual(values=mycol)+
  theme_classic(
    base_size = 15,
    base_family ="Times",
  )+
  theme(legend.position = "none")+
  theme(legend.position = "none")+
  annotate("text", label = "bold(B)", 
           x=0.5, y = 44, parse = TRUE,
           size = 7) 
#figure 1C
interact <- plot_model(hx, type = "int",
                       title="",
                       axis.title = c("", "Number of preprint articles"),
                       #   axis.labels = c("ECR_status_first_simplified ="),
                       legend.title = "")+
  theme_classic(
    base_size = 15,
    base_family ="Times",
  )+
  annotate("text", label = "bold(C)", 
           x=0.5, y = 7.2, parse = TRUE,
          size = 7) 
# Combine using patchwork
combined_total <- ggbox_1a+
  ggbox_1b+
interact
  plot_layout(ncol = 3)
combined_total
