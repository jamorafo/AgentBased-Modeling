rm(list=ls(all=TRUE))
################################################################################
# #  Paths 
################################################################################
# #  Directory. Type your own source path
setwd("/Users/andresmorales/Documents/TELUQ/ENV6008/T2/src")

docPath      <- "../doc/informes/"
outPath      <- "../output/"
outPathGraph <- "../output/graph/"
inPath       <- "../input/"
srcPath      <- "../src/"

################################################################################
# #  Libraries 
################################################################################


library(jpeg)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(cowplot)

################################################################################
# #  Importing 
################################################################################

# # Question 2: Utilisez les paramètres par défaut du modèle, mais fixez le
# # nombre de recycleurs à zéro. Explorez ce scénario en produisant cinq simulations 
# # d’une durée d’environ 5000 itérations. 

read_pop <- function(i) {
  fileName <- paste("population (",i,").csv",sep = "")
  inFile   <- paste(inPath, fileName, sep = "")
  data     <- read.csv(inFile,header = T)
  data.frame(sim=i,data)
  }

read_land <- function(i) {
  fileName <- paste("land-use (",i,").csv",sep = "")
  inFile   <- paste(inPath, fileName, sep = "")
  data     <- read.csv(inFile,header = T)
  data.frame(sim=i,data)
}

pop  <- lapply(1:5,read_pop)
land <- lapply(1:5,read_land)


pop_2 <- do.call(rbind,pop)
land_2 <- do.call(rbind,land)


qu2 <- pop_2 %>%
  full_join(land_2)

m_qu2 <- qu2 %>%
  group_by(sim) %>%
  summarise(n_sim=n(),recyclers= mean(recyclers), wastefuls=mean(wastefuls), new=mean(new),
            recycled = mean(recycled), waste=mean(waste))


mt_qu2 <-  qu2 %>%
  summarise(n_sim=n(),recyclers= mean(recyclers), wastefuls=mean(wastefuls), new=mean(new),
            recycled = mean(recycled), waste=mean(waste))

m_qu2 <- rbind(m_qu2,data.frame(sim="Total",mt_qu2))

qu2 %>%
  summarise(n_sim=n(),recyclers= max(recyclers), wastefuls=max(wastefuls), new=max(new),
            recycled = max(recycled), waste=max(waste))


fileName <- "m_question2.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(m_qu2,outFile)




sd_qu2 <- qu2 %>%
  group_by(sim) %>%
  summarise(n_sim=n(),recyclers= sd(recyclers), wastefuls=sd(wastefuls), new=sd(new),
            recycled = sd(recycled), waste=sd(waste))

sdt_qu2 <- qu2 %>%
  summarise(n_sim=n(),recyclers= sd(recyclers), wastefuls=sd(wastefuls), new=sd(new),
            recycled = sd(recycled), waste=sd(waste))

sd_qu2 <- rbind(sd_qu2,data.frame(sim="Total",sdt_qu2))


fileName <- "sd_question2.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(sd_qu2,outFile)





# # Question 3: Conservez toujours le nombre de recycleurs à zéro et 
# # évaluez les deux scénarios suivants.

# # a. Augmentez le paramètre resource-regeneration à 50. Produisez cinq simulations 
# # de ce scénario pour une durée d’environ 5000 itérations et calculez le nombre 
# # moyen de gaspilleurs et son écart type. Expliquez l’effet de cette augmentation 
# # sur le nombre de gaspilleurs pouvant exister. 
# # Justifiez votre réponse en fonction du comportement des agents et du 
# # fonctionnement du modèle.



pop  <- lapply(6:10,read_pop)
land <- lapply(6:10,read_land)

pop_3a <- do.call(rbind,pop)
land_3a <- do.call(rbind,land)


qu3a <- pop_3a %>%
  full_join(land_3a)

m_qu3a <- qu3a %>%
  group_by(sim) %>%
  summarise(n_sim=n(),recyclers= mean(recyclers), wastefuls=mean(wastefuls), new=mean(new),
            recycled = mean(recycled), waste=mean(waste))


mt_qu3a <-  qu3a %>%
  summarise(n_sim=n(),recyclers= mean(recyclers), wastefuls=mean(wastefuls), new=mean(new),
            recycled = mean(recycled), waste=mean(waste))

m_qu3a <- rbind(m_qu3a,data.frame(sim="Total",mt_qu3a))

qu3a %>%
  summarise(n_sim=n(),recyclers= max(recyclers), wastefuls=max(wastefuls), new=max(new),
            recycled = max(recycled), waste=max(waste))

m_qu3a
fileName <- "m_question3a.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(m_qu3a,outFile)




sd_qu3a <- qu3a %>%
  group_by(sim) %>%
  summarise(n_sim=n(),recyclers= sd(recyclers), wastefuls=sd(wastefuls), new=sd(new),
            recycled = sd(recycled), waste=sd(waste))

sdt_qu3a <- qu3a %>%
  summarise(n_sim=n(),recyclers= sd(recyclers), wastefuls=sd(wastefuls), new=sd(new),
            recycled = sd(recycled), waste=sd(waste))

sd_qu3a <- rbind(sd_qu3a,data.frame(sim="Total",sdt_qu3a))


fileName <- "sd_question3a.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(sd_qu3a,outFile)





# # b. Diminuez le paramètre resource-regeneration à 10 et conservez les valeurs 
# # par défaut pour les autres paramètres.

pop  <- lapply(11:15,read_pop)
land <- lapply(11:15,read_land)

pop_3b <- do.call(rbind,pop)
land_3b <- do.call(rbind,land)



qu3b <- pop_3b %>%
  full_join(land_3b)

m_qu3b <- qu3b %>%
  group_by(sim) %>%
  summarise(n_sim=n(),recyclers= mean(recyclers), wastefuls=mean(wastefuls), new=mean(new),
            recycled = mean(recycled), waste=mean(waste))


mt_qu3b <-  qu3b %>%
  summarise(n_sim=n(),recyclers= mean(recyclers), wastefuls=mean(wastefuls), new=mean(new),
            recycled = mean(recycled), waste=mean(waste))

m_qu3b <- rbind(m_qu3b,data.frame(sim="Total",mt_qu3b))

qu3b %>%
  summarise(n_sim=n(),recyclers= max(recyclers), wastefuls=max(wastefuls), new=max(new),
            recycled = max(recycled), waste=max(waste))

m_qu3b
fileName <- "m_question3b.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(m_qu3b,outFile)




sd_qu3b <- qu3b %>%
  group_by(sim) %>%
  summarise(n_sim=n(),recyclers= sd(recyclers), wastefuls=sd(wastefuls), new=sd(new),
            recycled = sd(recycled), waste=sd(waste))

sdt_qu3b <- qu3b %>%
  summarise(n_sim=n(),recyclers= sd(recyclers), wastefuls=sd(wastefuls), new=sd(new),
            recycled = sd(recycled), waste=sd(waste))

sd_qu3b <- rbind(sd_qu3b,data.frame(sim="Total",sdt_qu3b))


fileName <- "sd_question3b.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(sd_qu3b,outFile)




# # Question 4: Conservez toujours le nombre de recycleurs à zéro, 
# # mais augmentez le nombre de gaspilleurs à 50. Simulez le modèle pendant 
# # environ 1500 itérations en utilisant la valeur par défaut pour le paramètre resource-regeneration (25).
library(gridExtra)

pop  <- read_pop(16)
land <- read_land(16)

qu4 <- pop %>%
  full_join(land)


# Time vs New, Recycled, and Waste
p1 <- ggplot(qu4, aes(x = ticks)) +
  geom_line(aes(y = new, colour = "New")) +
  geom_line(aes(y = recycled, colour = "Recycled")) +
  geom_line(aes(y = waste, colour = "Waste")) +
  labs(x = "Time", y = "percent", colour = "Legend", title = "Land Use") +
  theme_minimal()

p1

p2 <- ggplot(qu4, aes(x = ticks)) +
  geom_line(aes(y = recyclers, colour = "Recyclers")) +
  geom_line(aes(y = wastefuls, colour = "Wastefuls")) +
  labs(x = "Time", y = "# of developers" , colour = "Legend", title = "Population") +
  theme_minimal()

p2
p_combined <- grid.arrange(p2, p1, ncol = 2)

# Save the combined plot to a PNG file
fileName <- "Q4.png"
outFile   <- paste(outPath, fileName, sep = "")
ggsave(outFile, plot = p_combined, width = 10, height = 5)


# # Question 5:
# # a. Simulez le modèle pendant environ 10 000 itérations en utilisant les paramètres par défaut (25 recycleurs). 
# # Rapportez les figures Population et Land use produites avec ce scénario. Combien de recycleurs et de gaspilleurs
# # persistent à la fin de la simulation?

pop  <- read_pop(17)
land <- read_land(17)


qu5a <- pop %>%
  full_join(land)


# Time vs New, Recycled, and Waste
p1 <- ggplot(qu5a, aes(x = ticks)) +
  geom_line(aes(y = new, colour = "New")) +
  geom_line(aes(y = recycled, colour = "Recycled")) +
  geom_line(aes(y = waste, colour = "Waste")) +
  labs(x = "Time", y = "percent", colour = "Legend", title = "Land Use") +
  theme_minimal()

p1

p2 <- ggplot(qu5a, aes(x = ticks)) +
  geom_line(aes(y = recyclers, colour = "Recyclers")) +
  geom_line(aes(y = wastefuls, colour = "Wastefuls")) +
  labs(x = "Time", y = "# of developers" , colour = "Legend", title = "Population") +
  theme_minimal()

p2
p_combined <- grid.arrange(p2, p1, ncol = 2)

# Save the combined plot to a PNG file
fileName <- "Q5a.png"
outFile   <- paste(outPath, fileName, sep = "")
ggsave(outFile, plot = p_combined, width = 10, height = 5)


# # Simulez le modèle, mais cette fois avec 50 recycleurs. Rapportez les figures Population et Land use produites avec ce scénario. 
# # Combien de recycleurs et de gaspilleurs persistent à la fin de la simulation?
# #

pop  <- read_pop(18)
land <- read_land(18)


qu5b <- pop %>%
  full_join(land)


# Time vs New, Recycled, and Waste
p1 <- ggplot(qu5b, aes(x = ticks)) +
  geom_line(aes(y = new, colour = "New")) +
  geom_line(aes(y = recycled, colour = "Recycled")) +
  geom_line(aes(y = waste, colour = "Waste")) +
  labs(x = "Time", y = "percent", colour = "Legend", title = "Land Use") +
  theme_minimal()

p1

p2 <- ggplot(qu5b, aes(x = ticks)) +
  geom_line(aes(y = recyclers, colour = "Recyclers")) +
  geom_line(aes(y = wastefuls, colour = "Wastefuls")) +
  labs(x = "Time", y = "# of developers" , colour = "Legend", title = "Population") +
  theme_minimal()

p2
p_combined <- grid.arrange(p2, p1, ncol = 2)

# Save the combined plot to a PNG file
fileName <- "Q5b.png"
outFile   <- paste(outPath, fileName, sep = "")
ggsave(outFile, plot = p_combined, width = 10, height = 5)


########




pop  <- lapply(c(17,19,20,21,22,23),read_pop)

pop_5d <- do.call(rbind,pop)
pop_5d <- pop_5d %>%
  filter(ticks<5000)

pop_5d$rr[pop_5d$sim==17] <- 25
pop_5d$rr[pop_5d$sim==19] <- 40
pop_5d$rr[pop_5d$sim==20] <- 55
pop_5d$rr[pop_5d$sim==21] <- 70
pop_5d$rr[pop_5d$sim==22] <- 50
pop_5d$rr[pop_5d$sim==23] <- 10


p1 <- ggplot(pop_5d, aes(x = ticks)) +
  geom_line(aes(y = recyclers, group = rr, color = as.factor(rr))) +
  labs(x = "Time", y = "Count", color = "Res. regeneration", title = "Recyclers Population") +
  theme_minimal()+ 
  theme(legend.position="none") # remove legend


p2 <- ggplot(pop_5d, aes(x = ticks)) +
  geom_line(aes(y = wastefuls, group = rr, color = as.factor(rr))) +
  labs(x = "Time", y = "Count", color = "Simulation", title = "Wastefuls Population") +
  theme_minimal() +
  theme(legend.position="none") # remove legend


# Extract the legend from one of the plots
legend <- get_legend(p1 + theme(legend.position="right")+ guides(color = guide_legend(reverse = TRUE)))

#p_combined <- grid.arrange(p1, p2, ncol = 2)
# Arrange the plots and the legend
p_combined <- grid.arrange(p1, p2, legend, ncol=3,widths=c(5,5,1.5))




fileName <- "Q5d.png"
outFile   <- paste(outPath, fileName, sep = "")
ggsave(outFile, plot = p_combined, width = 10, height = 5)




################################################################################
# #  End
################################################################################

















