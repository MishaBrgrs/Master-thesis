###qPCR analysis
#install packages
install.packages("DescTools")
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



#load packages
library("DescTools")
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

#beta GLucan training
#load data from excel and transform
IL1b <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "IL1ba")
IL6 <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "IL6a")
TNFa <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "TNFaa")
GCSF <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "GCSFa")
RPL32 <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "RPL32a")
RPL13a <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "RPL13aa")


#IL1b
IL1b <- IL1b %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

IL1b <- IL1b[!duplicated(IL1b$Name), ]

b.IL1b<- IL1b %>%
  mutate(Training = case_when(
    endsWith(Name, "B-L") ~ "trained",
    endsWith(Name, "P-L") ~ "untrained",
    endsWith(Name, "B-P") ~ "trained",
    endsWith(Name, "P-P") ~ "untrained"
  ))
b.IL1b <- na.omit(b.IL1b)
b.IL1b<- b.IL1b %>%
  mutate(exposure = case_when(
    endsWith(Name, "B-L") ~ "exposed",
    endsWith(Name, "P-L") ~ "exposed",
    endsWith(Name, "B-P") ~ "control",
    endsWith(Name, "P-P") ~ "control"
  ))
c.b.IL1b <- subset(b.IL1b, ID == "Cavefish")
s.b.IL1b <- subset(b.IL1b, ID == "Surface fish")

c.b.IL1b$ref <- paste(c.b.IL1b$Training, c.b.IL1b$exposure)
s.b.IL1b$ref <- paste(s.b.IL1b$Training, s.b.IL1b$exposure)

ref.c.b.IL1b <- subset(c.b.IL1b, ref == "trained control")
ref.s.b.IL1b <- subset(s.b.IL1b, ref == "trained control")

# Calculate the average of all values in the column
c.IL1b.average <- mean(ref.c.b.IL1b$Cp, na.rm = TRUE)
s.IL1b.average <- mean(ref.s.b.IL1b$Cp, na.rm = TRUE)

# Print the average
print(c.IL1b.average)
print(s.IL1b.average)

#IL6
IL6 <- IL6 %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

IL6 <- IL6[!duplicated(IL6$Name), ]

b.IL6<- IL6 %>%
  mutate(Training = case_when(
    endsWith(Name, "B-L") ~ "trained",
    endsWith(Name, "P-L") ~ "untrained",
    endsWith(Name, "B-P") ~ "trained",
    endsWith(Name, "P-P") ~ "untrained"
  ))
b.IL6 <- na.omit(b.IL6)
b.IL6<- b.IL6 %>%
  mutate(exposure = case_when(
    endsWith(Name, "B-L") ~ "exposed",
    endsWith(Name, "P-L") ~ "exposed",
    endsWith(Name, "B-P") ~ "control",
    endsWith(Name, "P-P") ~ "control"
  ))
c.b.IL6 <- subset(b.IL6, ID == "Cavefish")
s.b.IL6 <- subset(b.IL6, ID == "Surface fish")

c.b.IL6$ref <- paste(c.b.IL6$Training, c.b.IL6$exposure)
s.b.IL6$ref <- paste(s.b.IL6$Training, s.b.IL6$exposure)

ref.c.b.IL6 <- subset(c.b.IL6, ref == "trained control")
ref.s.b.IL6 <- subset(s.b.IL6, ref == "trained control")

# Calculate the average of all values in the column
c.IL6.average <- mean(ref.c.b.IL6$Cp, na.rm = TRUE)
s.IL6.average <- mean(ref.s.b.IL6$Cp, na.rm = TRUE)

# Print the average
print(c.IL6.average)
print(s.IL6.average)

#TNFa
TNFa <- TNFa %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

TNFa <- TNFa[!duplicated(TNFa$Name), ]

b.TNFa<- TNFa %>%
  mutate(Training = case_when(
    endsWith(Name, "B-L") ~ "trained",
    endsWith(Name, "P-L") ~ "untrained",
    endsWith(Name, "B-P") ~ "trained",
    endsWith(Name, "P-P") ~ "untrained"
  ))
b.TNFa <- na.omit(b.TNFa)
b.TNFa<- b.TNFa %>%
  mutate(exposure = case_when(
    endsWith(Name, "B-L") ~ "exposed",
    endsWith(Name, "P-L") ~ "exposed",
    endsWith(Name, "B-P") ~ "control",
    endsWith(Name, "P-P") ~ "control"
  ))
c.b.TNFa <- subset(b.TNFa, ID == "Cavefish")
s.b.TNFa <- subset(b.TNFa, ID == "Surface fish")

c.b.TNFa$ref <- paste(c.b.TNFa$Training, c.b.TNFa$exposure)
s.b.TNFa$ref <- paste(s.b.TNFa$Training, s.b.TNFa$exposure)

ref.c.b.TNFa <- subset(c.b.TNFa, ref == "trained control")
ref.s.b.TNFa <- subset(s.b.TNFa, ref == "trained control")

# Calculate the average of all values in the column
c.TNFa.average <- mean(ref.c.b.TNFa$Cp, na.rm = TRUE)
s.TNFa.average <- mean(ref.s.b.TNFa$Cp, na.rm = TRUE)

# Print the average
print(c.TNFa.average)
print(s.TNFa.average)


#GCSF
GCSF <- GCSF %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

GCSF <- GCSF[!duplicated(GCSF$Name), ]

b.GCSF<- GCSF %>%
  mutate(Training = case_when(
    endsWith(Name, "B-L") ~ "trained",
    endsWith(Name, "P-L") ~ "untrained",
    endsWith(Name, "B-P") ~ "trained",
    endsWith(Name, "P-P") ~ "untrained"
  ))
b.GCSF <- na.omit(b.GCSF)
b.GCSF<- b.GCSF %>%
  mutate(exposure = case_when(
    endsWith(Name, "B-L") ~ "exposed",
    endsWith(Name, "P-L") ~ "exposed",
    endsWith(Name, "B-P") ~ "control",
    endsWith(Name, "P-P") ~ "control"
  ))
c.b.GCSF <- subset(b.GCSF, ID == "Cavefish")
s.b.GCSF <- subset(b.GCSF, ID == "Surface fish")

c.b.GCSF$ref <- paste(c.b.GCSF$Training, c.b.GCSF$exposure)
s.b.GCSF$ref <- paste(s.b.GCSF$Training, s.b.GCSF$exposure)

ref.c.b.GCSF <- subset(c.b.GCSF, ref == "trained control")
ref.s.b.GCSF <- subset(s.b.GCSF, ref == "trained control")

# Calculate the average of all values in the column
c.GCSF.average <- mean(ref.c.b.GCSF$Cp, na.rm = TRUE)
s.GCSF.average <- mean(ref.s.b.GCSF$Cp, na.rm = TRUE)

# Print the average
print(c.GCSF.average)
print(s.GCSF.average)


####housekeeping genes
#RPL32
RPL32 <- RPL32 %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

RPL32 <- RPL32[!duplicated(RPL32$Name), ]

b.RPL32<- RPL32 %>%
  mutate(Training = case_when(
    endsWith(Name, "B-L") ~ "trained",
    endsWith(Name, "P-L") ~ "untrained",
    endsWith(Name, "B-P") ~ "trained",
    endsWith(Name, "P-P") ~ "untrained"
  ))
b.RPL32 <- na.omit(b.RPL32)
b.RPL32<- b.RPL32 %>%
  mutate(exposure = case_when(
    endsWith(Name, "B-L") ~ "exposed",
    endsWith(Name, "P-L") ~ "exposed",
    endsWith(Name, "B-P") ~ "control",
    endsWith(Name, "P-P") ~ "control"
  ))
c.b.RPL32 <- subset(b.RPL32, ID == "Cavefish")
s.b.RPL32 <- subset(b.RPL32, ID == "Surface fish")

c.b.RPL32$ref <- paste(c.b.RPL32$Training, c.b.RPL32$exposure)
s.b.RPL32$ref <- paste(s.b.RPL32$Training, s.b.RPL32$exposure)

ref.c.b.RPL32 <- subset(c.b.RPL32, ref == "trained control")
ref.s.b.RPL32 <- subset(s.b.RPL32, ref == "trained control")

# Calculate the average of all values in the column
c.RPL32.average <- mean(ref.c.b.RPL32$Cp, na.rm = TRUE)
s.RPL32.average <- mean(ref.s.b.RPL32$Cp, na.rm = TRUE)

# Print the average
print(c.RPL32.average)
print(s.RPL32.average)

####RPL13a
RPL13a <- RPL13a  %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

RPL13a <- RPL13a[!duplicated(RPL13a$Name), ]

b.RPL13a <- RPL13a  %>%
  mutate(Training = case_when(
    endsWith(Name, "B-L") ~ "trained",
    endsWith(Name, "P-L") ~ "untrained",
    endsWith(Name, "B-P") ~ "trained",
    endsWith(Name, "P-P") ~ "untrained"
  ))
b.RPL13a  <- na.omit(b.RPL13a )
b.RPL13a <- b.RPL13a  %>%
  mutate(exposure = case_when(
    endsWith(Name, "B-L") ~ "exposed",
    endsWith(Name, "P-L") ~ "exposed",
    endsWith(Name, "B-P") ~ "control",
    endsWith(Name, "P-P") ~ "control"
  ))
c.b.RPL13a  <- subset(b.RPL13a, ID == "Cavefish")
s.b.RPL13a  <- subset(b.RPL13a, ID == "Surface fish")

c.b.RPL13a$ref <- paste(c.b.RPL13a$Training, c.b.RPL13a$exposure)
s.b.RPL13a$ref <- paste(s.b.RPL13a$Training, s.b.RPL13a$exposure)

ref.c.b.RPL13a <- subset(c.b.RPL13a, ref == "trained control")
ref.s.b.RPL13a <- subset(s.b.RPL13a, ref == "trained control")

# Calculate the average of all values in the column
c.RPL13a.average <- mean(ref.c.b.RPL13a$Cp, na.rm = TRUE)
s.RPL13a.average <- mean(ref.s.b.RPL13a$Cp, na.rm = TRUE)

# Print the average
print(c.RPL13a.average)
print(s.RPL13a.average)

###calculate delta ct values
c.b.IL1b$delta <- as.numeric(c.IL1b.average-c.b.IL1b$Cp)
c.b.IL6$delta <- as.numeric(c.IL6.average-c.b.IL6$Cp)
c.b.TNFa$delta <- as.numeric(c.TNFa.average-c.b.TNFa$Cp)
c.b.GCSF$delta <- as.numeric(c.GCSF.average-c.b.GCSF$Cp)
c.b.RPL32$delta <- as.numeric(c.RPL32.average-c.b.RPL32$Cp)
c.b.RPL13a$delta <- as.numeric(c.RPL13a.average-c.b.RPL13a$Cp)

s.b.IL1b$delta <- as.numeric(s.IL1b.average-s.b.IL1b$Cp)
s.b.IL6$delta <- as.numeric(s.IL6.average-s.b.IL6$Cp)
s.b.TNFa$delta <- as.numeric(s.TNFa.average-s.b.TNFa$Cp)
s.b.GCSF$delta <- as.numeric(s.GCSF.average-s.b.GCSF$Cp)
s.b.RPL32$delta <- as.numeric(s.RPL32.average-s.b.RPL32$Cp)
s.b.RPL13a$delta <- as.numeric(s.RPL13a.average-s.b.RPL13a$Cp)


###create relative quantity values
eff.IL1b <- 2.1531
eff.IL6 <- 1.9461
eff.TNFa <- 2.2307
eff.GCSF <- 2.1914
eff.RPL32 <- 1.8969
eff.RPL13a <- 2.214

###calculate relative quantity values
c.b.IL1b$RQ <- as.numeric(eff.IL1b^c.b.IL1b$delta)
c.b.IL6$RQ <- as.numeric(eff.IL6^c.b.IL6$delta)
c.b.TNFa$RQ <- as.numeric(eff.TNFa^c.b.TNFa$delta)
c.b.GCSF$RQ <- as.numeric(eff.GCSF^c.b.GCSF$delta)
c.b.RPL32$RQ <- as.numeric(eff.RPL32^c.b.RPL32$delta)
c.b.RPL13a$RQ<- as.numeric(eff.RPL13a^c.b.RPL13a$delta)

s.b.IL1b$RQ <- as.numeric(eff.IL1b^s.b.IL1b$delta)
s.b.IL6$RQ <- as.numeric(eff.IL6^s.b.IL6$delta)
s.b.TNFa$RQ <- as.numeric(eff.TNFa^s.b.TNFa$delta)
s.b.GCSF$RQ <- as.numeric(eff.GCSF^s.b.GCSF$delta)
s.b.RPL32$RQ <- as.numeric(eff.RPL32^s.b.RPL32$delta)
s.b.RPL13a$RQ<- as.numeric(eff.RPL13a^s.b.RPL13a$delta)

c.df <- cbind.data.frame(c.b.RPL32$RQ, c.b.RPL13a$RQ, c.b.RPL32$Training,c.b.RPL32$exposure)
names(c.df) <- c("RQ.RPL32", "RQ.RPL13a","training", "exposure") 

s.df <- cbind.data.frame(s.b.RPL32$RQ, s.b.RPL13a$RQ, s.b.RPL32$Training,s.b.RPL32$exposure)
names(s.df) <- c("RQ.RPL32", "RQ.RPL13a","training", "exposure") 

###calculate geometric mean
for(i in 1:nrow(c.df)) {c.df$gm[i]<-geometric.mean(c(c.df$RQ.RPL32[i],c.df$RQ.RPL13a[i]))}
for(i in 1:nrow(s.df)) {s.df$gm[i]<-geometric.mean(c(s.df$RQ.RPL32[i],s.df$RQ.RPL13a[i]))}

###calculate relative gene expression values
c.df$RGE.IL1b<-c.b.IL1b$RQ/c.df$gm
c.df$RGE.IL6<-c.b.IL6$RQ/c.df$gm
c.df$RGE.TNFa<-c.b.TNFa$RQ/c.df$gm
c.df$RGE.GCSF<-c.b.GCSF$RQ/c.df$gm

s.df$RGE.IL1b<-s.b.IL1b$RQ/s.df$gm
s.df$RGE.IL6<-s.b.IL6$RQ/s.df$gm
s.df$RGE.TNFa<-s.b.TNFa$RQ/s.df$gm
s.df$RGE.GCSF<-s.b.GCSF$RQ/s.df$gm

####renaming
c.df$training[c.df$training == "trained"] <- "trained"
c.df$training[c.df$training == "untrained"] <- "auntrained"
c.df$exposure[c.df$exposure == "exposed"] <- "exposed"
c.df$exposure[c.df$exposure == "control"] <- "control"

s.df$training[s.df$training == "trained"] <- "trained"
s.df$training[s.df$training == "untrained"] <- "auntrained"
s.df$exposure[s.df$exposure == "exposed"] <- "exposed"
s.df$exposure[s.df$exposure == "control"] <- "control"


####plot the data
p.c.IL1b <- ggplot(c.df, aes(x=exposure,RGE.IL1b,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(x = "Cavefish", y = "relative gene expression [log 10]", title = "Cavefish") +
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))
p.c.IL1b

p.c.IL6 <- ggplot(c.df, aes(x=exposure,RGE.IL6,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "Cavefish") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.c.TNFa <-ggplot(c.df, aes(x=exposure,RGE.TNFa,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "relative gene expression [log 10]", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.c.GCSF <-ggplot(c.df, aes(x=exposure,RGE.GCSF,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

####Surface fish

p.s.IL1b <- ggplot(s.df, aes(x=exposure,RGE.IL1b,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "Surface fish") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))


p.s.IL6 <- ggplot(s.df, aes(x=exposure,RGE.IL6,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "Surface fish") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.s.TNFa <-ggplot(s.df, aes(x=exposure,RGE.TNFa,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.s.GCSF <-ggplot(s.df, aes(x=exposure,RGE.GCSF,  fill = training)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

###combine plots
p.IL1b <- ggarrange(p.c.IL1b,
                    p.s.IL1b,
                    ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "IL1b", y="Flourescens intensity in Log10")+
  theme(plot.title = element_text(size=20, face='bold'))
p.IL1b

p.IL6 <- ggarrange(p.c.IL6,
                    p.s.IL6,
                    ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "IL6", y="")+
  theme(plot.title = element_text(size=20, face='bold'))
p.IL6

p.TNFa <- ggarrange(p.c.TNFa,
                   p.s.TNFa,
                   ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "TNFa", y="Flourescens intensity in Log10")+
  theme(plot.title = element_text(size=20, face='bold'))
p.TNFa

p.GCSF <- ggarrange(p.c.GCSF,
                    p.s.GCSF,
                    ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "GCSF", y="")+
  theme(plot.title = element_text(size=20, face='bold'))
p.GCSF

p.qPCR.master <- ggarrange(p.IL1b,
                           p.IL6,
                           p.TNFa,
                           p.GCSF,
                           ncol = 2, nrow = 2,common.legend = TRUE, legend = "right") +
  labs(title = "", y="")+
  theme(plot.title = element_text(size=20, face='bold'))
p.qPCR.master 

###glm
#visualise in histogram
ggplot(data.frame(x = s.df$RGE.IL6), aes(x = x)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Data Distribution", x = "Values", y = "Frequency")
  
# Q-Q plot for normality check
qqnorm(c.df$RGE.IL1b)
qqline(s.df$RGE.GCSF)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(s.df$RGE.GCSF)
print(shapiro_test)

# GLM with Gamma distribution if the data suggests it's appropriate
if(shapiro_test$p.value < 0.05) {
  # Data may not be normal; consider Gamma if right-skewed
  gamma_model <- glm(RGE.TNFa ~ training * exposure, family = Gamma(link = "log"), data = s.df)
  summary(gamma_model)
  
  # Model comparison or further diagnostics could be here
  # For instance, checking AIC, residual plots, etc.
} else {
  print("Data may be normally distributed, consider Gaussian GLM or validate Gamma distribution fit further.")
}

# Note: The decision to use a Gamma GLM should also be based on theoretical considerations
# and further diagnostics beyond the Shapiro-Wilk test. This includes evaluating model fit,
# residual plots, and whether the mean-variance relationship in your data suggests Gamma is appropriate.
# Kruskal-Wallis test for non-parametric comparison
kruskal_test <- kruskal.test(RGE.IL6 ~ interaction, data = c.df)
print(kruskal_test)

# If Kruskal-Wallis test is significant, perform pairwise comparisons
if(kruskal_test$p.value < 0.05) {
  print("Significant differences detected, proceeding with pairwise comparisons.")
  
  # Pairwise Wilcoxon tests with p-value adjustment
  pairwise_results <- pairwise.wilcox.test(qpcr_data$deltaCt, qpcr_data$interaction, 
                                           p.adjust.method = "holm")
  
  print(pairwise_results)
  
  # Note: The pairwise.wilcox.test function does not handle ties or zero-difference pairs by default as well as the Wilcoxon test does.
  # Consider consulting additional resources or a statistician if your data has many ties or zero differences.
} else {
  print("No significant differences detected by Kruskal-Wallis test.")
}

## Assume you have a vector of p-values from your models
p_values <- c(0.0178, 9.55e-08,0.0639)

# Apply Benjamini-Hochberg adjustment
p_adjusted <- p.adjust(p_values, method = "BH")

# Print adjusted p-values
print(p_adjusted)

# You might then decide on significance based on these adjusted p-values.

###LPS 

#load data from excel and transform
IL1b <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "IL1bLPS")
IL6 <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "IL6LPS")
TNFa <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "TNFaLPS")
GCSF <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "GCSFLPS")
RPL32 <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "RPL32LPS")
RPL13a <- read_excel("/Users/michelleborgers/Documents/Uni/Masterarbeit/qPCR.xlsx", sheet = "RPL13aLPS")


#IL1b
IL1b <- IL1b %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

IL1b <- IL1b[!duplicated(IL1b$Name), ]

b.IL1b<- IL1b %>%
  mutate(Time = case_when(
    endsWith(Name, "3h LPS") ~ "3 hours",
    endsWith(Name, "3h PBS") ~ "3 hours",
    endsWith(Name, "1h LPS") ~ "1 hour",
    endsWith(Name, "1h PBS") ~ "1 hour"
  ))

b.IL1b<- b.IL1b %>%
  mutate(exposure = case_when(
    endsWith(Name, "LPS") ~ "exposed",
    endsWith(Name, "PBS") ~ "control",
  ))
c.b.IL1b <- subset(b.IL1b, ID == "Cavefish")
s.b.IL1b <- subset(b.IL1b, ID == "Surface fish")

c.b.IL1b$ref <- paste(c.b.IL1b$Time, c.b.IL1b$exposure)
s.b.IL1b$ref <- paste(s.b.IL1b$Time, s.b.IL1b$exposure)

ref.c.b.IL1b <- subset(c.b.IL1b, ref == "3 hours control")
ref.s.b.IL1b <- subset(s.b.IL1b, ref == "3 hours control")

# Calculate the average of all values in the column
c.IL1b.average <- mean(ref.c.b.IL1b$Cp, na.rm = TRUE)
s.IL1b.average <- mean(ref.s.b.IL1b$Cp, na.rm = TRUE)

# Print the average
print(c.IL1b.average)
print(s.IL1b.average)

#IL6
IL6 <- IL6 %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

IL6 <- IL6[!duplicated(IL6$Name), ]

b.IL6<- IL6 %>%
  mutate(Time = case_when(
    endsWith(Name, "3h LPS") ~ "3 hours",
    endsWith(Name, "3h PBS") ~ "3 hours",
    endsWith(Name, "1h LPS") ~ "1 hour",
    endsWith(Name, "1h PBS") ~ "1 hour"
  ))

b.IL6<- b.IL6 %>%
  mutate(exposure = case_when(
    endsWith(Name, "LPS") ~ "exposed",
    endsWith(Name, "PBS") ~ "control",
  ))
c.b.IL6 <- subset(b.IL6, ID == "Cavefish")
s.b.IL6 <- subset(b.IL6, ID == "Surface fish")

c.b.IL6$ref <- paste(c.b.IL6$Time, c.b.IL6$exposure)
s.b.IL6$ref <- paste(s.b.IL6$Time, s.b.IL6$exposure)

ref.c.b.IL6 <- subset(c.b.IL6, ref == "3 hours control")
ref.s.b.IL6 <- subset(s.b.IL6, ref == "3 hours control")

# Calculate the average of all values in the column
c.IL6.average <- mean(ref.c.b.IL6$Cp, na.rm = TRUE)
s.IL6.average <- mean(ref.s.b.IL6$Cp, na.rm = TRUE)

# Print the average
print(c.IL6.average)
print(s.IL6.average)

#TNFa
TNFa <- TNFa %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

TNFa <- TNFa[!duplicated(TNFa$Name), ]

b.TNFa<- TNFa %>%
  mutate(Time = case_when(
    endsWith(Name, "3h LPS") ~ "3 hours",
    endsWith(Name, "3h PBS") ~ "3 hours",
    endsWith(Name, "1h LPS") ~ "1 hour",
    endsWith(Name, "1h PBS") ~ "1 hour"
  ))

b.TNFa<- b.TNFa %>%
  mutate(exposure = case_when(
    endsWith(Name, "LPS") ~ "exposed",
    endsWith(Name, "PBS") ~ "control",
  ))
c.b.TNFa <- subset(b.TNFa, ID == "Cavefish")
s.b.TNFa <- subset(b.TNFa, ID == "Surface fish")

c.b.TNFa$ref <- paste(c.b.TNFa$Time, c.b.TNFa$exposure)
s.b.TNFa$ref <- paste(s.b.TNFa$Time, s.b.TNFa$exposure)

ref.c.b.TNFa <- subset(c.b.TNFa, ref == "3 hours control")
ref.s.b.TNFa <- subset(s.b.TNFa, ref == "3 hours control")

# Calculate the average of all values in the column
c.TNFa.average <- mean(ref.c.b.TNFa$Cp, na.rm = TRUE)
s.TNFa.average <- mean(ref.s.b.TNFa$Cp, na.rm = TRUE)

# Print the average
print(c.TNFa.average)
print(s.TNFa.average)


#GCSF
GCSF <- GCSF %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

GCSF <- GCSF[!duplicated(GCSF$Name), ]

b.GCSF<- GCSF %>%
  mutate(Time = case_when(
    endsWith(Name, "3h LPS") ~ "3 hours",
    endsWith(Name, "3h PBS") ~ "3 hours",
    endsWith(Name, "1h LPS") ~ "1 hour",
    endsWith(Name, "1h PBS") ~ "1 hour"
  ))

b.GCSF<- b.GCSF %>%
  mutate(exposure = case_when(
    endsWith(Name, "LPS") ~ "exposed",
    endsWith(Name, "PBS") ~ "control",
  ))
c.b.GCSF <- subset(b.GCSF, ID == "Cavefish")
s.b.GCSF <- subset(b.GCSF, ID == "Surface fish")

c.b.GCSF$ref <- paste(c.b.GCSF$Time, c.b.GCSF$exposure)
s.b.GCSF$ref <- paste(s.b.GCSF$Time, s.b.GCSF$exposure)

ref.c.b.GCSF <- subset(c.b.GCSF, ref == "3 hours control")
ref.s.b.GCSF <- subset(s.b.GCSF, ref == "3 hours control")


# Calculate the average of all values in the column
c.GCSF.average <- mean(ref.c.b.GCSF$Cp, na.rm = TRUE)
s.GCSF.average <- mean(ref.s.b.GCSF$Cp, na.rm = TRUE)

# Print the average
print(c.GCSF.average)
print(s.GCSF.average)


####housekeeping genes
#RPL32
RPL32 <- RPL32 %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

RPL32 <- RPL32[!duplicated(RPL32$Name), ]

b.RPL32<- RPL32 %>%
  mutate(Time = case_when(
    endsWith(Name, "3h LPS") ~ "3 hours",
    endsWith(Name, "3h PBS") ~ "3 hours",
    endsWith(Name, "1h LPS") ~ "1 hour",
    endsWith(Name, "1h PBS") ~ "1 hour"
  ))

b.RPL32<- b.RPL32 %>%
  mutate(exposure = case_when(
    endsWith(Name, "LPS") ~ "exposed",
    endsWith(Name, "PBS") ~ "control",
  ))
c.b.RPL32 <- subset(b.RPL32, ID == "Cavefish")
s.b.RPL32 <- subset(b.RPL32, ID == "Surface fish")

c.b.RPL32$ref <- paste(c.b.RPL32$Time, c.b.RPL32$exposure)
s.b.RPL32$ref <- paste(s.b.RPL32$Time, s.b.RPL32$exposure)

ref.c.b.RPL32 <- subset(c.b.RPL32, ref == "3 hours control")
ref.s.b.RPL32 <- subset(s.b.RPL32, ref == "3 hours control")

# Calculate the average of all values in the column
c.RPL32.average <- mean(ref.c.b.RPL32$Cp, na.rm = TRUE)
s.RPL32.average <- mean(ref.s.b.RPL32$Cp, na.rm = TRUE)

# Print the average
print(c.RPL32.average)
print(s.RPL32.average)

####RPL13a
RPL13a <- RPL13a %>%
  mutate(ID = case_when(
    startsWith(Name, "C") ~ "Cavefish",
    startsWith(Name, "S") ~ "Surface fish"
  ))

RPL13a <- RPL13a[!duplicated(RPL13a$Name), ]

b.RPL13a<- RPL13a %>%
  mutate(Time = case_when(
    endsWith(Name, "3h LPS") ~ "3 hours",
    endsWith(Name, "3h PBS") ~ "3 hours",
    endsWith(Name, "1h LPS") ~ "1 hour",
    endsWith(Name, "1h PBS") ~ "1 hour"
  ))

b.RPL13a<- b.RPL13a %>%
  mutate(exposure = case_when(
    endsWith(Name, "LPS") ~ "exposed",
    endsWith(Name, "PBS") ~ "control",
  ))
c.b.RPL13a <- subset(b.RPL13a, ID == "Cavefish")
s.b.RPL13a <- subset(b.RPL13a, ID == "Surface fish")

c.b.RPL13a$ref <- paste(c.b.RPL13a$Time, c.b.RPL13a$exposure)
s.b.RPL13a$ref <- paste(s.b.RPL13a$Time, s.b.RPL13a$exposure)

ref.c.b.RPL13a <- subset(c.b.RPL13a, ref == "3 hours control")
ref.s.b.RPL13a <- subset(s.b.RPL13a, ref == "3 hours control")

# Calculate the average of all values in the column
c.RPL13a.average <- mean(ref.c.b.RPL13a$Cp, na.rm = TRUE)
s.RPL13a.average <- mean(ref.s.b.RPL13a$Cp, na.rm = TRUE)

# Print the average
print(c.RPL13a.average)
print(s.RPL13a.average)

###calculate delta ct values
c.b.IL1b$delta <- as.numeric(c.IL1b.average-c.b.IL1b$Cp)
c.b.IL6$delta <- as.numeric(c.IL6.average-c.b.IL6$Cp)
c.b.TNFa$delta <- as.numeric(c.TNFa.average-c.b.TNFa$Cp)
c.b.GCSF$delta <- as.numeric(c.GCSF.average-c.b.GCSF$Cp)
c.b.RPL32$delta <- as.numeric(c.RPL32.average-c.b.RPL32$Cp)
c.b.RPL13a$delta <- as.numeric(c.RPL13a.average-c.b.RPL13a$Cp)

s.b.IL1b$delta <- as.numeric(s.IL1b.average-s.b.IL1b$Cp)
s.b.IL6$delta <- as.numeric(s.IL6.average-s.b.IL6$Cp)
s.b.TNFa$delta <- as.numeric(s.TNFa.average-s.b.TNFa$Cp)
s.b.GCSF$delta <- as.numeric(s.GCSF.average-s.b.GCSF$Cp)
s.b.RPL32$delta <- as.numeric(s.RPL32.average-s.b.RPL32$Cp)
s.b.RPL13a$delta <- as.numeric(s.RPL13a.average-s.b.RPL13a$Cp)


###create relative quantity values
eff.IL1b <- 2.1531
eff.IL6 <- 1.9461
eff.TNFa <- 2.2307
eff.GCSF <- 2.1914
eff.RPL32 <- 1.8969
eff.RPL13a <- 2.214

###calculate relative quantity values
c.b.IL1b$RQ <- as.numeric(eff.IL1b^c.b.IL1b$delta)
c.b.IL6$RQ <- as.numeric(eff.IL6^c.b.IL6$delta)
c.b.TNFa$RQ <- as.numeric(eff.TNFa^c.b.TNFa$delta)
c.b.GCSF$RQ <- as.numeric(eff.GCSF^c.b.GCSF$delta)
c.b.RPL32$RQ <- as.numeric(eff.RPL32^c.b.RPL32$delta)
c.b.RPL13a$RQ<- as.numeric(eff.RPL13a^c.b.RPL13a$delta)

s.b.IL1b$RQ <- as.numeric(eff.IL1b^s.b.IL1b$delta)
s.b.IL6$RQ <- as.numeric(eff.IL6^s.b.IL6$delta)
s.b.TNFa$RQ <- as.numeric(eff.TNFa^s.b.TNFa$delta)
s.b.GCSF$RQ <- as.numeric(eff.GCSF^s.b.GCSF$delta)
s.b.RPL32$RQ <- as.numeric(eff.RPL32^s.b.RPL32$delta)
s.b.RPL13a$RQ<- as.numeric(eff.RPL13a^s.b.RPL13a$delta)

c.df <- cbind.data.frame(c.b.RPL32$RQ, c.b.RPL13a$RQ, c.b.RPL32$Time,c.b.RPL32$exposure)
names(c.df) <- c("RQ.RPL32", "RQ.RPL13a","time", "exposure") 

s.df <- cbind.data.frame(s.b.RPL32$RQ, s.b.RPL13a$RQ, s.b.RPL32$Time,s.b.RPL32$exposure)
names(s.df) <- c("RQ.RPL32", "RQ.RPL13a","time", "exposure") 

###calculate geometric mean

geometric.mean <- function(x) {
  exp(mean(log(x)))
}

for(i in 1:nrow(c.df)) {c.df$gm[i]<-geometric.mean(c(c.df$RQ.RPL32[i],c.df$RQ.RPL13a[i]))}
for(i in 1:nrow(s.df)) {s.df$gm[i]<-geometric.mean(c(s.df$RQ.RPL32[i],s.df$RQ.RPL13a[i]))}

###calculate relative gene expression values
c.df$RGE.IL1b<-c.b.IL1b$RQ/c.df$gm
c.df$RGE.IL6<-c.b.IL6$RQ/c.df$gm
c.df$RGE.TNFa<-c.b.TNFa$RQ/c.df$gm
c.df$RGE.GCSF<-c.b.GCSF$RQ/c.df$gm

s.df$RGE.IL1b<-s.b.IL1b$RQ/s.df$gm
s.df$RGE.IL6<-s.b.IL6$RQ/s.df$gm
s.df$RGE.TNFa<-s.b.TNFa$RQ/s.df$gm
s.df$RGE.GCSF<-s.b.GCSF$RQ/s.df$gm

###substets
c.1h <- subset(c.df, time == "1 hour")
c.3h <- subset(c.df, time == "3 hours")

c.e <- subset(c.df, exposure == "exposed")
c.c <- subset(c.df, exposure == "control")

s.e <- subset(s.df, exposure == "exposed")
s.c <- subset(s.df, exposure == "control")

s.1h <- subset(s.df, time == "1 hour")
s.3h <- subset(s.df, time == "3 hours")

c.1h.e <- subset(c.1h, exposure == "exposed")
c.1h.c <- subset(c.1h, exposure == "control")
c.3h.e <- subset(c.3h, exposure == "exposed")
c.3h.c <- subset(c.3h, exposure == "control")

s.1h.e <- subset(s.1h, exposure == "exposed")
s.1h.c <- subset(s.1h, exposure == "control")
s.3h.e <- subset(s.3h, exposure == "exposed")
s.3h.c <- subset(s.3h, exposure == "control")

####plot the data
p.c.IL1b <- ggplot(c.df, aes(x=exposure,RGE.IL1b,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  labs(x = "Cavefish", y = "relative gene expression [log 10]", title = "Cavefish") +
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))
p.c.IL1b

p.c.IL6 <- ggplot(c.df, aes(x=exposure,RGE.IL6,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "Cavefish") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.c.TNFa <-ggplot(c.df, aes(x=exposure,RGE.TNFa,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "relative gene expression [log 10]", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.c.GCSF <-ggplot(c.df, aes(x=exposure,RGE.GCSF,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "bottom",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

####Surface fish

p.s.IL1b <- ggplot(s.df, aes(x=exposure,RGE.IL1b,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "Surface fish") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))


p.s.IL6 <- ggplot(s.df, aes(x=exposure,RGE.IL6,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "Surface fish") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=0), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.s.TNFa <-ggplot(s.df, aes(x=exposure,RGE.TNFa,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

p.s.GCSF <-ggplot(s.df, aes(x=exposure,RGE.GCSF,  fill = time)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(width = 0.5), binwidth = 0.1) +
  scale_color_manual() +
  theme_light()+
  labs(x = "Cavefish", y = "", title = "") +
  scale_fill_manual(values = c("#53868B", "#FFB90F"))+
  theme(plot.title = element_text(size=20, face='bold'), axis.title.x = element_blank(), axis.title.y = element_text(size=18, vjust=1), axis.text.x = element_text(size=18))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size=18), axis.title.y = element_text(size=18))+
  theme(legend.position = "",legend.key.size = unit(1, 'cm'),legend.text = element_text(size = 18), plot.title = element_text(hjust=0.5))+
  scale_y_continuous(trans='log10',limits=c(0.05, 400))

###combine plots
p.IL1b <- ggarrange(p.c.IL1b,
                    p.s.IL1b,
                    ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "IL1b", y="Flourescens intensity in Log10")+
  theme(plot.title = element_text(size=20, face='bold'))
p.IL1b

p.IL6 <- ggarrange(p.c.IL6,
                   p.s.IL6,
                   ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "IL6", y="")+
  theme(plot.title = element_text(size=20, face='bold'))
p.IL6

p.TNFa <- ggarrange(p.c.TNFa,
                    p.s.TNFa,
                    ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "TNFa", y="Flourescens intensity in Log10")+
  theme(plot.title = element_text(size=20, face='bold'))
p.TNFa

p.GCSF <- ggarrange(p.c.GCSF,
                    p.s.GCSF,
                    ncol = 2, nrow = 1,common.legend = TRUE, legend = "none") +
  labs(title = "GCSF", y="")+
  theme(plot.title = element_text(size=20, face='bold'))
p.GCSF

p.qPCR.master <- ggarrange(p.IL1b,
                           p.IL6,
                           p.TNFa,
                           p.GCSF,
                           ncol = 2, nrow = 2,common.legend = TRUE, legend = "right") +
  labs(title = "", y="")+
  theme(plot.title = element_text(size=20, face='bold'))
p.qPCR.master 


###visualise in histogram
ggplot(data.frame(x = s.3h$RGE.TNFa), aes(x = x)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Data Distribution", x = "Values", y = "Frequency")



# Q-Q plot for normality check
qqnorm(c.df$RGE.IL1b)
qqline(s.df$RGE.GCSF)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(s.e$RGE.IL1b)
print(shapiro_test)



# GLM with Gamma distribution if the data suggests it's appropriate
if(shapiro_test$p.value < 0.05) {
  # Data may not be normal; consider Gamma if right-skewed
  gamma_model <- glm(RGE.GCSF ~ time + exposure, family = Gamma(link = "log"), data = s.df)
  summary(gamma_model)
  
  # Model comparison or further diagnostics could be here
  # For instance, checking AIC, residual plots, etc.
} else {
  print("Data may be normally distributed, consider Gaussian GLM or validate Gamma distribution fit further.")
}



## Assume you have a vector of p-values from your models
p_values <- c(0.327, 2.62e-08)

# Apply Benjamini-Hochberg adjustment
p_adjusted <- p.adjust(p_values, method = "BH")

# Print adjusted p-values
print(p_adjusted)

# You might then decide on significance based on these adjusted p-values.

