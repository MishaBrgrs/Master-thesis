# Analysis of Microscopy data

#install packages
install.packages("ggplot2")
install.packages("readxl")
install.packages("tidyr")
install.packages("reshape2")
install.packages("dplyr")
install.packages("vioplot")
install.packages("vipor")
install.packages("ggbeeswarm")
install.packages("ggpubr")
install.packages("PairedData")
install.packages("nortest")
install.packages("car")
install.packages("Hmisc")

#load packages
library("ggplot2")
library("readxl")
library("tidyr")
library("reshape2")
library("dplyr")
library("vioplot")
library("vipor")
library("ggbeeswarm")
library("ggpubr")
library("PairedData")
library("nortest")
library("car") 
library("Hmisc")


#load data from excel and transform
###median
#cavefish
median <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/Microskopy/Microscopy Results.xlsx", sheet = "median")
cfLPS <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/Microskopy/Microscopy Results.xlsx", sheet = "Master CF LPS")
sfLPS <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/Microskopy/Microscopy Results.xlsx", sheet = "Master SF LPS")


median<- median %>%
  mutate(training = case_when(
    endsWith(treatment, "β-Glucan + LPS") ~ "trained",
    endsWith(treatment, "PBS + LPS") ~ "untrained",
    endsWith(treatment, "β-Glucan + PBS") ~ "trained",
    endsWith(treatment, "PBS + PBS") ~ "untrained"
  ))

median<- median  %>%
  mutate(exposure = case_when(
    endsWith(treatment, "β-Glucan + LPS") ~ "exposed",
    endsWith(treatment, "PBS + LPS") ~ "exposed",
    endsWith(treatment, "β-Glucan + PBS") ~ "control",
    endsWith(treatment, "PBS + PBS") ~ "control"
  ))

cfLPS<- cfLPS  %>%
  mutate(exposure = case_when(
    endsWith(treatment, "LPS") ~ "exposed",
    endsWith(treatment, "PBS") ~ "control",
  ))

sfLPS<- sfLPS  %>%
  mutate(exposure = case_when(
    endsWith(treatment, "LPS") ~ "exposed",
    endsWith(treatment, "PBS") ~ "control",
  ))

###create subsets
cf.LPS.ATP <- subset(cfLPS, Dye == "ATPred")
cf.LPS.TMRE <- subset(cfLPS, Dye == "TMRE")

sf.LPS.ATP <- subset(sfLPS, Dye == "ATPred")
sf.LPS.TMRE <- subset(sfLPS, Dye == "TMRE")

cf.b.median <- subset(median, ID == "Cavefish")
sf.b.median <- subset(median, ID == "Surface fish")

cf.b.ATP<- subset(cf.b.median, Dye == "ATPred")
cf.b.TMRE<- subset(cf.b.median, Dye == "TMRE")
sf.b.ATP<- subset(sf.b.median, Dye == "ATPred")
sf.b.TMRE<- subset(sf.b.median, Dye == "TMRE")

cf.PP <- subset(cf.b.median, treatment == "PBS + PBS")
cf.PP.ATP <- subset(cf.PP, Dye == "ATPred")
cf.PP.TMRE <- subset(cf.PP, Dye == "TMRE")

cf.PL <- subset(cf.b.median, treatment == "PBS + LPS")
cf.PL.ATP <- subset(cf.PL, Dye == "ATPred")
cf.PL.TMRE <- subset(cf.PL, Dye == "TMRE")

cf.BP <- subset(cf.b.median, treatment == "β-Glucan + PBS")
cf.BP.ATP <- subset(cf.BP, Dye == "ATPred")
cf.BP.TMRE <- subset(cf.BP, Dye == "TMRE")

cf.BL <- subset(cf.b.median, treatment == "β-Glucan + LPS")
cf.BL.ATP <- subset(cf.BL, Dye == "ATPred")
cf.BL.TMRE <- subset(cf.BL, Dye == "TMRE")

#surface fish
sf.PP <- subset(sf.b.median, treatment == "PBS + PBS")
sf.PP.ATP <- subset(sf.PP, Dye == "ATPred")
sf.PP.TMRE <- subset(sf.PP, Dye == "TMRE")

sf.PL <- subset(sf.b.median, treatment == "PBS + LPS")
sf.PL.ATP <- subset(sf.PL, Dye == "ATPred")
sf.PL.TMRE <- subset(sf.PL, Dye == "TMRE")

sf.BP <- subset(sf.b.median, treatment == "β-Glucan + PBS")
sf.BP.ATP <- subset(sf.BP, Dye == "ATPred")
sf.BP.TMRE <- subset(sf.BP, Dye == "TMRE")

sf.BL <- subset(sf.b.median, treatment == "β-Glucan + LPS")
sf.BL.ATP <- subset(sf.BL, Dye == "ATPred")
sf.BL.TMRE <- subset(sf.BL, Dye == "TMRE")

###plot LPS values
bpcfLPSA <- ggplot(data = cf.LPS.ATP, aes(x = ID, y = Int, fill = exposure, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="ATPred/MTG Integrated 
       Density [log10]", title = "Cavefish",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=0))+
  theme(legend.position = "bottom", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10')
bpcfLPSA

bpcfLPST <- ggplot(data = cf.LPS.TMRE, aes(x = ID, y = Int, fill = exposure, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="TMRE/MTG Integrated 
       Density [log10]", title = "",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=0))+
  theme(legend.position = "bottom", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10')
bpcfLPST

bpsfLPSA <- ggplot(data = sf.LPS.ATP, aes(x = ID, y = Int, fill = exposure, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="", title = "Surface fish",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=0))+
  theme(legend.position = "bottom", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10')
bpsfLPSA

bpsfLPST <- ggplot(data = sf.LPS.TMRE, aes(x = ID, y = Int, fill = exposure, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="", title = "",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=0))+
  theme(legend.position = "bottom", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10')
bpsfLPST

##combine plots
p.LPS<- ggarrange(bpcfLPSA,
                  bpsfLPSA,
                  bpcfLPST,
                  bpsfLPST,
                     labels = c("A", "", "B", ""),
                     ncol = 2, nrow = 2 ,common.legend = TRUE, legend = "bottom") +
  labs(title = "", y="")+
  theme(plot.title = element_text(size=30, face='bold'))
p.LPS

###plot the median values
cf.b.ATP$training <- factor(cf.b.ATP$training, levels = c("untrained", "trained"))
cf.p.ATP<-ggplot(cf.b.ATP, aes(x=exposure,median,  fill = training)) +
  geom_dotplot(binaxis = "y",stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.015)+
  scale_color_manual() +
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(x = "", y = "ATPred/MTG Integrated 
       Density [log10]", title = "Cavefish",fill ="") +
  theme(plot.title = element_text(size=25, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 20), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10', limits = c(0.1,0.43))
cf.p.ATP

cf.b.TMRE$training <- factor(cf.b.TMRE$training, levels = c("untrained", "trained"))
cf.p.TMRE<-ggplot(cf.b.TMRE, aes(x=exposure,median,  fill = training)) +
  geom_dotplot(binaxis = "y",stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.049)+
  scale_color_manual() +
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(x = "Cavefish", y = "TMRE/MTG Integrated 
       Density [log10]", title = "",fill ="") +
  theme(plot.title = element_text(size=12, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=20), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 20), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits = c(0.001,0.1))
cf.p.TMRE

sf.b.ATP$training <- factor(sf.b.ATP$training, levels = c("untrained", "trained"))
sf.p.ATP<-ggplot(sf.b.ATP, aes(x=exposure,median,  fill = training)) +
  geom_dotplot(binaxis = "y",stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.015)+
  scale_color_manual() +
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(x = "", y = "", title = "Surface fish",fill ="") +
  theme(plot.title = element_text(size=25, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=12))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans= "log10", limits = c(0.1,0.43))
sf.p.ATP

sf.b.TMRE$training <- factor(sf.b.TMRE$training, levels = c("untrained", "trained"))
sf.p.TMRE<-ggplot(sf.b.TMRE, aes(x=exposure,median,  fill = training)) +
  geom_dotplot(binaxis = "y",stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.049)+
  scale_color_manual() +
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(x = "Surface fish", y = "", title = "", fill ="") +
  theme(plot.title = element_text(size=12, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=20), axis.title.y = element_text(size=12))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10')
sf.p.TMRE

###combine median plots
p.median<- ggarrange(cf.p.ATP,
                     sf.p.ATP,
                     cf.p.TMRE,
                     sf.p.TMRE,
                       labels = c("A", "", "B", ""),
                       ncol = 2, nrow = 2 ,common.legend = TRUE, legend = "bottom") +
  labs(title = "", y="")+
  theme(plot.title = element_text(size=30, face='bold'))
p.median

###glm model with median
###visualise in histogram
ggplot(data.frame(x = cATPb$), aes(x = x)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Data Distribution", x = "Values", y = "Frequency")

# Q-Q plot for normality check
qqnorm(sf.b.TMRE$median)
qqline(sf.b.TMRE$median)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(sf.b.TMRE$median)
print(shapiro_test)


###gamma glm
gamma_model <- glm(mean~ training * exposure, family = Gamma, data = cf.b.TMRE)
summary(gamma_model)

#### Gaussian GLM
glm_model <- glm(median ~ training * exposure,  data = sf.b.TMRE, family = gaussian)
summary(glm_model)

### two-way ANOVA
anova_result <- aov(median ~ training * exposure, data = cf.b.ATP)
summary(anova_result)

###general values
cfLPS <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/Microskopy/Microscopy Results.xlsx", sheet = "Master CF LPS") 
cfLPS <- cfLPS[-3:-8] 
cfLPS <- cfLPS[-4:-6] 
names(cfLPS) <- c("Fish", "treatment", "Dye", "Int") 

cfLPS <- cfLPS %>%
  mutate(ID = case_when(
    startsWith(Fish, "C") ~ "Cavefish",
    startsWith(Fish, "S") ~ "Surface fish"
  ),)

cfbeta <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/Microskopy/Microscopy Results.xlsx", sheet = "CF beta Master")
cfbeta <- cfbeta[-3:-8]
cfbeta <- cfbeta[-4:-6]
names(cfbeta) <- c("Fish", "treatment", "Dye", "Int")

cfbeta <- cfbeta %>%
  mutate(ID = case_when(
    startsWith(Fish, "C") ~ "Cavefish",
    startsWith(Fish, "S") ~ "Surface fish"
  ))
cfbeta<- cfbeta %>%
  mutate(training = case_when(
    endsWith(treatment, "β-Glucan + LPS") ~ "trained",
    endsWith(treatment, "PBS + LPS") ~ "untrained",
    endsWith(treatment, "β-Glucan + PBS") ~ "trained",
    endsWith(treatment, "PBS + PBS") ~ "untrained"
  ))

cfbeta<- cfbeta  %>%
  mutate(exposure = case_when(
    endsWith(treatment, "β-Glucan + LPS") ~ "exposed",
    endsWith(treatment, "PBS + LPS") ~ "exposed",
    endsWith(treatment, "β-Glucan + PBS") ~ "control",
    endsWith(treatment, "PBS + PBS") ~ "control"
  ))

cf.b.ATP <- subset(cfbeta, Dye == "ATPred")
cf.b.TMRE <- subset(cfbeta, Dye == "TMRE")

sf.b.ATP <- subset(sfbeta, Dye == "ATPred")
sf.b.TMRE <- subset(sfbeta, Dye == "TMRE")

sfLPS <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/Microskopy/Microscopy Results.xlsx", sheet = "Master SF LPS")
sfLPS <- sfLPS[-3:-8]
sfLPS <- sfLPS[-4:-6]
names(sfLPS) <- c("Fish", "treatment", "Dye", "Int")

sfLPS <- sfLPS %>%
  mutate(ID = case_when(
    startsWith(Fish, "C") ~ "Cavefish",
    startsWith(Fish, "S") ~ "Surface fish"
  ))

sfbeta <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/Microskopy/Microscopy Results.xlsx", sheet = "SF beta Master")
sfbeta <- sfbeta[-3:-8]
sfbeta <- sfbeta[-4:-6]
names(sfbeta) <- c("Fish", "treatment", "Dye", "Int")

sfbeta <- sfbeta %>%
  mutate(ID = case_when(
    startsWith(Fish, "C") ~ "Cavefish ",
    startsWith(Fish, "S") ~ "Surface fish"))

sfbeta<- sfbeta %>%
  mutate(training = case_when(
    endsWith(treatment, "β-Glucan + LPS") ~ "trained",
    endsWith(treatment, "PBS + LPS") ~ "untrained",
    endsWith(treatment, "β-Glucan + PBS") ~ "trained",
    endsWith(treatment, "PBS + PBS") ~ "untrained"
  ))

sfbeta<- sfbeta  %>%
  mutate(exposure = case_when(
    endsWith(treatment, "β-Glucan + LPS") ~ "exposed",
    endsWith(treatment, "PBS + LPS") ~ "exposed",
    endsWith(treatment, "β-Glucan + PBS") ~ "control",
    endsWith(treatment, "PBS + PBS") ~ "control"
  ))

###glm with general data
ggplot(data.frame(x = cATPb$Int), aes(x = x)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Data Distribution", x = "Values", y = "Frequency")

# Q-Q plot for normality check
qqnorm(sf.b.TMRE$median)
qqline(sf.b.TMRE$median)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(sf.b.TMRE$median)
print(shapiro_test)


###gamma glm
gamma_model <- glm(Int~ training * exposure, family = Gamma, data = cATPb)
summary(gamma_model)

###plot normal beta values
cf.b.ATP$training <- factor(cf.b.ATP$training, levels = c("untrained", "trained"))
bpcfLPSA <- ggplot(data = cf.b.ATP, aes(x = exposure,ID, y = Int, fill = training, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="ATPred/MTG Integrated 
       Density [log10]", title = "Cavefish",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=0))+
  theme(legend.position = "", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10', limits=c(0.05, 5))
bpcfLPSA

cf.b.TMRE$training <- factor(cf.b.TMRE$training, levels = c("untrained", "trained"))
bpcfLPST <- ggplot(data = cf.b.TMRE, aes(x = exposure,ID, y = Int, fill = training, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="TMRE/MTG Integrated 
       Density [log10]", title = "",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=20))+
  theme(legend.position = "", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10', limits=c(0.0005, 5))
bpcfLPST

sf.b.ATP$training <- factor(sf.b.ATP$training, levels = c("untrained", "trained"))
bpsfLPSA <- ggplot(data = sf.b.ATP, aes(x = exposure,ID, y = Int, fill = training, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="", title = "Surface fish",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=0))+
  theme(legend.position = "", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10', limits=c(0.05, 5))
bpsfLPSA

sf.b.TMRE$training <- factor(sf.b.TMRE$training, levels = c("untrained", "trained"))
bpsfLPST <- ggplot(data = sf.b.TMRE, aes(x = exposure,ID, y = Int, fill = training, main = "Top left")) + 
  geom_boxplot() +
  geom_quasirandom(dodge.width=0.8, alpha=0.25, size=0.8, width=0.1)+
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(y="", title = "",fill ="")+
  theme(plot.title = element_text(size=25, face='bold'),axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=20))+
  theme(legend.position = "", legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 15), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10', limits=c(0.0005, 5))
bpsfLPST

###combine average beta plots
p.beta<- ggarrange(bpcfLPSA,
                   bpsfLPSA,
                   bpcfLPST,
                   bpsfLPST,
                     labels = c("A", "", "B", ""),
                     ncol = 2, nrow = 2 ,common.legend = TRUE, legend = "bottom") +
  labs(title = "", y="")+
  theme(plot.title = element_text(size=30, face='bold'))
p.beta


#combine plots
LPSfigure <- ggarrange(bpcfLPS,
                    bpsfLPS,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2 ,common.legend = TRUE, legend = "bottom") +
  labs(title = "ATPred and TMRE intensity after 3 h LPS incubation", y="Flourescens intensity in Log10")+
  theme(plot.title = element_text(size=30, face='bold'))
LPSfigure

betafigure <- ggarrange(bpcfbeta,
                        bpsfbeta,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2 ,common.legend = TRUE, legend = "bottom") +
  labs(title = "ATPred and TMRE intensity", y="Flourescens intensity in Log10")+
  theme(plot.title = element_text(size=25, face='bold'))
betafigure

figure <- ggarrange(LPSfigure,
                    betafigure,
                    ncol = 1, nrow = 2) +
  labs(title = "", y="Flourescens intensity in Log10")+
  theme(plot.title = element_text(size=25, face='bold'))
figure

#create a master table and plot
LPSmaster <- rbind.data.frame(cfLPS, sfLPS)
betamaster <- rbind.data.frame(cfbeta, sfbeta)
master <- rbind.data.frame(LPSmaster, betamaster)

master$concatenated <- paste(master$ID, master$treatment)




###START OF ANALYZIS
###create subsets 
###LPS
ATP <- subset(LPSmaster, Dye == "ATPred")
TMRE <- subset(LPSmaster, Dye == "TMRE")

cATP <- subset(ATP, ID == "Cavefish")
cTMRE <- subset(TMRE, ID == "Cavefish")
sATP <- subset(ATP, ID == "Surface fish")
sTMRE <- subset(TMRE, ID == "Surface fish")

cATPp <- subset(cATP, treatment == "PBS")
cATPl <- subset(cATP, treatment == "LPS")
sATPp <- subset(sATP, treatment == "PBS")
sATPl <- subset(sATP, treatment == "LPS")
cTMREp <- subset(cTMRE, treatment == "PBS")
cTMREl <- subset(cTMRE, treatment == "LPS")
sTMREp <- subset(sTMRE, treatment == "PBS")
sTMREl <- subset(sTMRE, treatment == "LPS")

#beta
bATP <- subset(betamaster, Dye == "ATPred")
bTMRE <- subset(betamaster, Dye == "TMRE")

cATPb <- subset(bATP, ID == "Cavefish")
cTMREb <- subset(bTMRE, ID == "Cavefish")
sATPb <- subset(bATP, ID == "Surface fish")
sTMREb <- subset(bTMRE, ID == "Surface fish")

cATPbpl <- subset(cATPb, treatment == "PBS + LPS")
cATPbpp <- subset(cATPb, treatment == "PBS + PBS")
cATPbbl <- subset(cATPb, treatment == "β-Glucan + LPS")
cATPbbp <- subset(cATPb, treatment == "β-Glucan + PBS")
sATPbpl <- subset(sATPb, treatment == "PBS + LPS")
sATPbpp <- subset(sATPb, treatment == "PBS + PBS")
sATPbbl <- subset(sATPb, treatment == "β-Glucan + LPS")
sATPbbp <- subset(sATPb, treatment == "β-Glucan + PBS")

cTMREbpl <- subset(cTMREb, treatment == "PBS + LPS")
cTMREbpp <- subset(cTMREb, treatment == "PBS + PBS")
cTMREbbl <- subset(cTMREb, treatment == "β-Glucan + LPS")
cTMREbbp <- subset(cTMREb, treatment == "β-Glucan + PBS")
sTMREbpl <- subset(sTMREb, treatment == "PBS + LPS")
sTMREbpp <- subset(sTMREb, treatment == "PBS + PBS")
sTMREbbl <- subset(sTMREb, treatment == "β-Glucan + LPS")
sTMREbbp <- subset(sTMREb, treatment == "β-Glucan + PBS")

#check data for normality with Shapiro-Wil test and QQ plot

shapiro_test <- shapiro.test(cATPbpl$Int)
print("Shapiro-Wilk test:")
print(shapiro_test)

# QQ plot
qqnorm(cATPbpl$Int)
qqline(cATPbpl$Int)

# Step 3: Test for normality using Anderson-Darling test
ad_test <- ad.test(cATPbpl$Int)
print("Anderson-Darling test:")
print(ad_test)

# Step 4: Visualize the data distribution using a histogram
ggplot(data.frame(x = cATPbpl$Int), aes(x = x)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Data Distribution", x = "Values", y = "Frequency")

#all data sets = not normally distributed - Right-Skewed Exponential Pop

# Perform Wilcoxon Rank Sum Test to compare surface and cave as two independent groups
#3h LPS only
wilcox_test_result <- wilcox.test(cATPp$Int, cATPl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 9081, p-value = 9.984e-06
median(cATPp$Int)
median(cATPl$Int)

wilcox_test_result <- wilcox.test(sATPp$Int, sATPl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 6232, p-value = 2.083e-15
median(sATPp$Int)
median(sATPl$Int)

wilcox_test_result <- wilcox.test(cTMREp$Int, cTMREl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 8502, p-value = 2.067e-07
median(cTMREp$Int)
median(cTMREl$Int)

wilcox_test_result <- wilcox.test(sTMREp$Int, sTMREl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 9522, p-value = 7.476e-05
median(sTMREp$Int)
median(sTMREl$Int)

#Beta Glucan Training ATPred
wilcox_test_result <- wilcox.test(cATPbpl$Int, sATPbpl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 16771, p-value = 1.602e-06

wilcox_test_result <- wilcox.test(cATPbpp$Int, sATPbpp$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 8440, p-value = 1.378e-07

wilcox_test_result <- wilcox.test(cATPbbl$Int, sATPbbl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 6365, p-value = 7.508e-15

wilcox_test_result <- wilcox.test(cATPbbp$Int, sATPbbp$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 9419, p-value = 4.406e-05

#beta gulcan training TMRE
wilcox_test_result <- wilcox.test(cTMREbpl$Int, sTMREbpl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 13600, p-value = 0.334

wilcox_test_result <- wilcox.test(cTMREbpp$Int, sTMREbpp$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 10817, p-value = 0.01659

wilcox_test_result <- wilcox.test(cTMREbbl$Int, sTMREbbl$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 8205, p-value = 2.823e-08

wilcox_test_result <- wilcox.test(cTMREbbp$Int, sTMREbbp$Int)
print("Wilcoxon Rank Sum Test (Mann-Whitney U Test):")
print(wilcox_test_result) #W = 8159, p-value = 2.051e-08

# Perform Wilcoxon Signed Rank Test for comparsion of beta glucan treatmin within the populations
#cavefish + surface beta und PBS - LPS und beta und LPS - PBS
#ATPred 
wilcox_test_result <- wilcox.test(cATPbbl$Int, cATPbpl$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 87691, p-value = 0,00126
median(cATPbbl$Int)# 0.150804
median(cATPbpl$Int)# 0.1788516

wilcox_test_result <- wilcox.test(sATPbbl$Int, sATPbbp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 1384, p-value < 2.2e-16
median(sATPbbl$Int)# 0.2399209
median(sATPbbp$Int)# 0.1439115


wilcox_test_result <- wilcox.test(cATPbpl$Int, cATPbpp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 6750, p-value = 0.598
median(cATPbpp$Int)# 0.2280614
median(cATPbpl$Int)# 0.203396

wilcox_test_result <- wilcox.test(sATPbpl$Int, sATPbpp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 6366, p-value = 0.9004
median(sATPbpl$Int)# 0.2512445
median(sATPbpp$Int)# 0.2804831

#TMRE
wilcox_test_result <- wilcox.test(cTMREbbl$Int, cTMREbbp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 6260, p-value = 0.7598
median(cTMREbbl$Int)# 0.003265671
median(cTMREbbp$Int)# 0.002294768

wilcox_test_result <- wilcox.test(sTMREbbl$Int, sTMREbbp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 10655, p-value = 6.97e-13
median(sTMREbbl$Int)# 0.006100976
median(sTMREbbp$Int)# 0.002407809

wilcox_test_result <- wilcox.test(cTMREbpl$Int, cTMREbpp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 8262, p-value = 0.001914
median(cTMREbpl$Int)# 0.04373074
median(cTMREbpp$Int)# 0.03723508

wilcox_test_result <- wilcox.test(sTMREbpl$Int, sTMREbpp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 9289, p-value = 1.217e-06
median(sTMREbpl$Int)# 0.06865747
median(sTMREbpp$Int)# 0.03910883

###create an table for median an p values
tab <- matrix(c(median(cATPbbl$Int), median(cATPbpl$Int), median(sATPbbl$Int), median(sATPbpl$Int),
                median(cTMREbbl$Int), median(cTMREbpl$Int), median(sTMREbbl$Int), median(sTMREbpl$Int)), ncol = 2, nrow = 4)
colnames(tab) <- c("Cavefish", "Surface fish")
rownames(tab) <- c("ATPred β-Glucan", "ATPred LPS","TMRE β-Glucan", "TMRE LPS")
tab <- as.table(tab)
tab


###added
#surface fish
#ATPre
wilcox_test_result <- wilcox.test(sATPl$Int, sATPbpl$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 12612, p-value < 2.2e-16

wilcox_test_result <- wilcox.test(sATPp$Int, sATPbpp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 9274, p-value = 1.384e-06

wilcox_test_result <- wilcox.test(sATPl$Int, sATPbbl$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 10994, p-value = 8.657e-15

wilcox_test_result <- wilcox.test(sATPp$Int, sATPbbp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 8196, p-value = 0.002783

#TMRE
wilcox_test_result <- wilcox.test(sTMREl$Int, sTMREbpl$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 12802, p-value < 2.2e-16

wilcox_test_result <- wilcox.test(sTMREp$Int, sTMREbpp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 12283, p-value < 2.2e-16

wilcox_test_result <- wilcox.test(sTMREl$Int, sTMREbbl$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 12021, p-value < 2.2e-16

wilcox_test_result <- wilcox.test(sTMREp$Int, sTMREbbp$Int, paired = TRUE)
print("Wilcoxon Signed Rank Test:")
print(wilcox_test_result) #V = 11052, p-value = 3.954e-15

