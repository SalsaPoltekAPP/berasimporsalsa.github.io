setwd("D:/METOPEL UAS/SALSA METOPEL")
library(readxl)
library(kableExtra)
library(tidyverse)

#Data
indi<-c(            
  "Populasi"="SP.POP.TOTL", 
  "Nilai Impor"="NE.IMP.GNFS.CD",
  "PDB"="NY.GDP.MKTP.CD"
)

dat<-WDI(           # Menarik data World Bank
  country="IDN", # Ganti nama negaranya sesuai kelompok
  indicator=indi,
  start=2003,end=2022,
)
view(dat)

read_excel("beras_.xlsx")
dat <- read_excel("beras_.xlsx")
kbl(dat) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Plot 
plot(dat$imp,dat$beras,xlab="Populasi",ylab="Impor Beras")

# regresi
reg1<-lm(imp~beras+pdb,data=dat)
summary(reg1)

#Y = Nilai Total Impor, X = Impor Beras, S = PDB
resid(reg1)
dat$u<-resid(reg1)
# plot eror

dat$u<-resid(reg1)
plot(dat$imp,dat$u,xlab="Total Impor (USD)",ylab="error")
abline(h=0) # membuat garis horizontal di y=0


plot(dat$beras,dat$u,xlab="Nilai Impor Beras (USD)",ylab="error")
abline(h=0)


plot(dat$pdb,dat$u,xlab="Nilai PDB (USD)",ylab="error")
abline(h=0)
