#Title: GLM and plots for Wolf_etal_2020 ECR Manuscript
#Purpose: Generate Generalized Linear Model and plots
#Author: JW
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
  select(c(Num_authors, H_index_first, H_index_last, Num_publications_first, Num_publications_last,
           Num_preprints_first, Num_preprints_last, Institution_size_first, TypeOf_Institution,
           ImpactFactor, gender_first, gender_last, ECR_status_first_simplified, ECR_status_last_simplified,
           Continent))

# Ensuring all relevant data columns are factors
dat_reduce$Institution_size_first<- as.factor (dat_reduce$Institution_size_first)
dat_reduce$gender_first<- as.factor (dat_reduce$gender_first)
dat_reduce$ECR_status_first_simplified<-as.factor(dat_reduce$ECR_status_first_simplified)
dat_reduce$TypeOf_Institution<-as.factor(dat_reduce$TypeOf_Institution)

# T-test to check if there is a significant difference in preprints between genders of first and last authors
t.test(dat_reduce$Num_preprints_first~dat_reduce$gender_first)
t.test(dat_reduce$Num_preprints_last~dat_reduce$gender_last)

# GLM
# Assume poission distribution if I leave response as # preprints and offset # of total publications as a predictor  
# Quasipoisson as poisson is over dispersed
model1<- glm(Num_preprints_first ~ ECR_status_first_simplified + 
           ECR_status_last_simplified+
           H_index_first+
           offset(log(Num_publications_first))+
           Institution_size_first,
         data=dat_reduce,
         family = quasipoisson)
summary(model1)

#Figure 1a
mycol <- c("#56B4E9", "#D55E00")
ggbox_first <- ggplot(data=subset(dat_reduce,!is.na(gender_first)),
                      aes(x= gender_first, y=Num_preprints_first,
                          fill=gender_first))+
  geom_violin(draw_quantiles = c(.25, .5, .75, .95), alpha=0.5, scale= "count")+
  geom_boxplot(width=0.08, outlier.shape = NA, color = "gray40",fill = "white", 
               coef =0)+
  ylab ("Number of preprint articles") +
  xlab ("Gender")+
  scale_x_discrete(labels=c("Female","Male"))+
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
ggbox_last <- ggplot(data=subset(dat_reduce,!is.na(gender_last)),
                     aes(gender_last,Num_preprints_last,
                         fill=gender_last))+
  geom_violin(draw_quantiles = c(.25, .5, .75, .95), alpha=0.5, scale= "count")+
  geom_boxplot(width=0.08, outlier.shape = NA, color = "gray40", fill = "white",
               coef =0)+
  ylab ("Number of preprint articles") +
  xlab ("Gender")+
  scale_x_discrete(labels=c("Female","Male"))+
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

# Combine figures 1a and 1b
combined_fig1<- ggbox_first+ggbox_last