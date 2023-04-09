library(dplyr)
library(ggplot2)
library(mgcv)




####pre plot####
data <- read.csv2("data.csv", sep = ",")

data$vol_ph_1 <- as.double(gsub(",",".", data$vol_ph_1))

data$der_ph_1 <- c(0, diff(data$ph_1))
data$der_vol_1 <- c(0, diff(data$vol_ph_1))
data$der_phvol_1 <- data$der_ph_1/data$der_vol_1

data$der_ph_2 <- c(0, diff(data$ph_2))
data$der_vol_2 <- c(0, diff(data$vol_ph_2))
data$der_phvol_2 <- data$der_ph_2/data$der_vol_2


data$fator_cor_cond <- (25+80+data$vol_cond_1)/25
data$cor_cond_1 <- (data$cond_1 * data$fator_cor_cond)


data$fator_cor_cond2 <- (25+132+data$vol_cond_2)/25
data$cor_cond_2 <- (data$cond_2 * data$fator_cor_cond2)

####tabelas####

df_ph1 <- data[, c("vol_ph_1", "ph_1", "der_ph_1", "der_vol_1", "der_phvol_1")]
df_ph2 <- data[, c("vol_ph_2", "ph_2", "der_ph_2", "der_vol_2", "der_phvol_2")]

df_cond1 <- data[c(1:86), c("vol_cond_1", "cond_1","fator_cor_cond" ,"cor_cond_1")]
df_cond2 <- data[c(1:83), c("vol_cond_2", "cond_2", "fator_cor_cond2","cor_cond_2")]

write_xlsx(df_cond2, "cond2.xlsx")

####função####

plottitration <- function(x,y,xlab, ylab) {
  
  ggplot(data = data, aes(x=x, y=y))+
    geom_line()+
    geom_point(shape = 15, fill = "black")+
    theme_bw()+
    labs(x=xlab, y=ylab)
  
}

intercept_point <- function(slope1, inter1, slope2, inter2) {
  x <- (inter2 - inter1) / (slope1 - slope2)
  y <- slope1 * x + inter1
  return(c(x, y))
}



####potenciometria 1####


ph1_plot <- plottitration(data$vol_ph_1, data$ph_1, "Volume de NaOH (mL)", "pH")
ph1_der_plot <- plottitration(data$vol_ph_1, data$der_phvol_1, "Volume de NaOH (mL)", "dpH/dV")


#ph1_der <- 
  ggplot(data, aes(x = vol_ph_1)) +
  geom_point(aes(y=ph_1, shape = "pH"))+
  geom_point(aes(y=der_phvol_1*5, shape = "dpH/dV"), size = 1.5)+
  geom_line(aes(y = ph_1)) +
  geom_line(aes(y = der_phvol_1*5))+
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "dpH/dV"))+
  theme_bw()+
  labs(x = "Volume de NaOH (mL)", y = "pH"
       , title = "Titulação 1 - Potenciometria para determinação do teor de Ácido Ascórbico"
       )+
  scale_shape_manual(name = "",values = c("pH" = 15, "dpH/dV" = 17))+
  theme(legend.position = c(0.11,0.95),
        legend.background = element_rect(fill = "transparent"))+
  geom_hline(yintercept = 4.34, color = "red", linetype = "dashed")+
  annotate("text", x=8, y=4.7, label = "pKa Ácido Ascórbico = 4,34", color = "red")+
  geom_point(aes(x=23.4, y=2.79), color = "red", shape = 15, size = 2.5)+
    geom_point(aes(x=46.6, y=9.76), color = "red", shape = 15, size = 2.5)+
    annotate("text", x=23.4, y=3.2, label="PE1", color = "red", size = 2.7)+
    annotate("text", x=48.5, y=9.76, label="PE2", color = "red", size = 2.7)
  
####potenciometria 2####


ph2_plot <- plottitration(data$vol_ph_2, data$ph_2, "Volume de NaOH (mL)", "pH")
ph2_der_plot <- plottitration(data$vol_ph_2, data$der_phvol_2, "Volume de NaOH (mL)", "dpH")

ph2_der <- 
  ggplot(data, aes(x = vol_ph_2)) +
  geom_point(aes(y=ph_2, shape = "pH"))+
  geom_point(aes(y=der_phvol_2*5, shape = "dpH/dV"), size = 1.5)+
  geom_line(aes(y = ph_2)) +
  geom_line(aes(y = der_phvol_2*5))+
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "dpH/dV"))+
  theme_bw()+
  labs(x = "Volume de NaOH (mL)", y = "pH"
       #, title = "Titulação 2 - Potenciometria para determinação do teor de Ácido Ascórbico"
       )+
  scale_shape_manual(name = "",values = c("pH" = 15, "dpH/dV" = 17))+
  theme(legend.position = c(0.11,0.95),
        legend.background = element_rect(fill = "transparent"))+
  geom_hline(yintercept = 4.45, color = "red", linetype = "dashed")+
  annotate("text", x=10.5, y=5, label = "pKa ?cido Asc?rbico = 4,45", color = "red")+
  geom_point(aes(x=24, y=2.94), color = "red", shape = 15, size = 2.5)+
  geom_point(aes(x=46.4, y=8.70), color = "red", shape = 15, size = 2.5)+
  annotate("text", x=24, y=3.5, label="PE1", color = "red", size = 2.7)+
  annotate("text", x=48.5, y=8.75, label="PE2", color = "red", size = 2.7)



##### Condutometria 1 ####

diffcond1 <- c(0,na.exclude(diff(data$cor_cond_1)))
diffvol1 <- c(0,na.exclude(diff(data$vol_cond_1)))
diffcondvol1 <- diffcond1/diffvol1

linha1 <- data[c(2:23),c("vol_cond_1","cor_cond_1")]
linha2 <- data[c(41:69), c("vol_cond_1","cor_cond_1")]
linha3 <- data[c(71:86), c("vol_cond_1","cor_cond_1")]

lm(linha1$cor_cond_1 ~ linha1$vol_cond_1)
lm(linha2$cor_cond_1 ~ linha2$vol_cond_1)
lm(linha3$cor_cond_1 ~ linha3$vol_cond_1)

intercept_point(-2.082, 80.331, 0.565, 19.318)
intercept_point(0.565, 19.318, 1.209, -10.479)  

cond1_plot <-  
  ggplot(data = data, aes(x=vol_cond_1, y=cor_cond_1))+
  geom_point(shape = 15, fill = "black")+
  geom_abline(slope = -2.082 , intercept = 80.331)+
  geom_abline(slope = 0.565 , intercept = 19.318)+
  geom_abline(slope = 1.209, intercept = -10.479)+
  annotate("text",x=22, y=53, label= "Região I\ny = -2.082x + 80.331", size = 3.5)+
  annotate("text",x=32, y=44, label= "Região II\ny = 0.565x + 19.318", size = 3.5)+
  annotate("text",x=47, y=60, label= "Região III\ny = 1.209x - 10.479", size = 3.5)+
  annotate("text",x=28, y=32, label= "23,05mL\n32,34mS/cm", color="red", size=3)+
  annotate("text",x=50, y=44, label= "46,27mL\n45,46mS/cm", color="red", size=3)+
  theme_bw()+
  labs(x= "Volume de NaOH (mL)", y= "Condutância corrigida (mS/cm)"
       #, title = "Figura X - Titulação condutométrica: segundo ensaio"
  )+
  geom_point(aes(x=23.04987, y=32.34118), color = "red", shape = 15)+
  geom_point(aes(x=46.26863, y=45.45978), color = "red", shape = 15)
  
  
##### Condutometria 2 ####

diffcond2 <- c(0,na.exclude(diff(data$cor_cond_2)))
diffvol2 <- c(0,na.exclude(diff(data$vol_cond_2)))
diffcondvol2 <- diffcond2/diffvol2

linha12 <- data[c(1:17),c("vol_cond_2","cor_cond_2")]
linha22 <- data[c(28:66), c("vol_cond_2","cor_cond_2")]
linha32 <- data[c(67:83), c("vol_cond_2","cor_cond_2")]

lm(linha12$cor_cond_2 ~ linha12$vol_cond_2)
lm(linha22$cor_cond_2 ~ linha22$vol_cond_2)
lm(linha32$cor_cond_2 ~ linha32$vol_cond_2)


intercept_point(-2.064, 82.384, 0.6143, 19.1967)  
intercept_point(0.6143,19.1967, 1.230, -9.748)


cond2_plot <-  
  ggplot(data = data, aes(x=vol_cond_2, y=cor_cond_2))+
    geom_point(shape = 15, fill = "black")+
  geom_abline(slope = -2.064 , intercept = 82.384)+
  geom_abline(slope = 0.6143 , intercept = 19.1967)+
  geom_abline(slope = 1.230 , intercept = -9.748)+
  annotate("text",x=20, y=60, label= "Região I\ny = -2.064x + 82.384", size = 3.5)+
  annotate("text",x=35, y=48, label= "Região II\ny = 0.6143x + 19.1967", size = 3.5)+
  annotate("text",x=45, y=60, label= "Região III\ny = 1.230x - 9.748", size = 3.5)+
  annotate("text",x=28, y=33.5, label= "23,59mL\n33,69mS/cm", color="red", size=3)+
  annotate("text",x=50, y=47, label= "47,01mL\n48,08mS/cm", color="red", size=3)+
  theme_bw()+
  labs(x= "Volume de NaOH (mL)", y= "Condutância corrigida (mS/cm)"
       #, title = "Figura X - Titulação condutométrica: segundo ensaio"
       )+
  geom_point(aes(x=23.59232, y=33.68946), color = "red", shape = 15)+
  geom_point(aes(x=47.01104, y=48.07558), color = "red", shape = 15)




