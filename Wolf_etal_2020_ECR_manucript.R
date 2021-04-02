# Title: Scripts for Wolf_etal_2020 ECR Manuscript
# Author: JW
# Input: Final dataset in .csv format

# Environment setup
pkgs<- c('tidyverse','ggplot2', 'extrafont', 'sjPlot', 'patchwork')
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

# 1) Create a series of models, building up from null model to full model by adding one term at a time.

# null model
model_null<- glm(Num_preprints_first ~ 1 +
                   offset(log(Num_publications_first)),
                data=dat_reduce,
                family = quasipoisson)

# add first author ECR status together with offset, which is associated
model_first<- glm(Num_preprints_first ~ ECR_status_first_simplified + 
                offset(log(Num_publications_first)),
                data=dat_reduce,
                family = quasipoisson)

# add last author ECR status
model_firstlast<- glm(Num_preprints_first ~ ECR_status_first_simplified + 
                offset(log(Num_publications_first))+
                ECR_status_last_simplified,
                data=dat_reduce,
                family = quasipoisson)

# add institution size
model_firstlastsize<- glm(Num_preprints_first ~ ECR_status_first_simplified + 
                        offset(log(Num_publications_first))+
                        ECR_status_last_simplified+
                        Institution_size_first,
                        data=dat_reduce,
                        family = quasipoisson)

# add the interaction 
model_firstlastsizeint<- glm(Num_preprints_first ~ ECR_status_first_simplified + 
                          offset(log(Num_publications_first))+
                          ECR_status_last_simplified+
                          Institution_size_first+
                          ECR_status_first_simplified:Institution_size_first,
                          data=dat_reduce,
                          family = quasipoisson)

# 2) Compare models stepwise to see if adding each term made the model significantly better than the previous simpler model.

anova(model_null, model_first, test="Chi") # does adding first author status significantly improve the model?
anova(model_first, model_firstlast, test="Chi") # does adding last author status significantly improve the model?
anova(model_firstlast, model_firstlastsize, test="Chi") # does adding institution size significantly improve the model?
anova(model_firstlastsize, model_firstlastsizeint, test="Chi") # does adding the interaction significantly improve the model?

# 3) Final Generalized Linear Model 

model1<- glm(Num_preprints_first ~ ECR_status_first_simplified + 
           ECR_status_last_simplified+
           offset(log(Num_publications_first))+
           Institution_size_first+
           Institution_size_first*ECR_status_first_simplified,
         data=dat_reduce,
         family = quasipoisson)
summary(model1)

# 4) Figures

#Figure 1a
mycol<- c("#56B4E9", "#D55E00")
mycol2 <- c("#56B4E9", "#D55E00","#E1BE6A")
ggbox_1a <- ggplot(data=dat_reduce,
                   aes(x= ECR_status_first_simplified, y=Num_preprints_first,
                       fill=ECR_status_first_simplified))+
  geom_violin(draw_quantiles = c(.25, .5, .75, .95), alpha=0.5)+#, scale = "count")+
  geom_boxplot(width=0.4, outlier.shape = NA, color = "black",fill = "white", 
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
           x=0.5, y = 5, parse = TRUE,
           size = 7)
#Figure 1b
ggbox_1b <- ggplot(data=dat_reduce,
                   aes(ECR_status_last_simplified,Num_preprints_last,
                       fill=ECR_status_last_simplified))+
  geom_violin(draw_quantiles = c(.25, .5, .75, .95), alpha=0.5)+#, scale ="count")+
  geom_boxplot(width=0.4, outlier.shape = NA, color = "black", fill = "white",
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
           x=0.5, y = 5, parse = TRUE,
           size = 7) 
#figure 1c
interact <- plot_model(hx, type = "int",
                       title="",
                       colors =mycol2,
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
combined_total <- ggbox_1a+ ggbox_1b+interact+
  plot_layout(ncol = 3)

ggsave("graphics/Fig1_final.tiff", 
       dpi = 400, plot = combined_total,
       width = 13, 
       height = 8, 
       unit = "in",
       compression ="lzw")
