library(xlsx)
library(plspm)
library(psych)
library(haven)
library(Amelia)
library(DMwR)
library(mice)
library(Hmisc)


rm(list = ls(all = TRUE)) # Clean the environment

#-----------------------------------------------------------------------------------------------------
# File names
#-----------------------------------------------------------------------------------------------------
data_file = "Revision 2017_EM.sav"
mmodel_file = "measurement model.txt"

#-----------------------------------------------------------------------------------------------------
# Model setup
#-----------------------------------------------------------------------------------------------------
# Structural model
image = c(0, 0, 0, 0, 0, 0, 0)
expect = c(1, 0, 0, 0, 0, 0, 0)
prodq = c(1, 1, 0, 0, 0, 0, 0)
servq = c(1, 1, 1, 0, 0, 0, 0)
value = c(0, 0, 1, 1, 0, 0, 0)
epsi = c(1, 0, 1, 1, 1, 0, 0)
loyal = c(0, 0, 0, 0, 0, 1, 0)

rbind <- base:::rbind
sat_path = rbind(image, expect, prodq, servq, value, epsi, loyal)
latent_names = rownames(sat_path)

# Measurement model
m_mod = read.csv(mmodel_file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
sat_blocks = list(which(m_mod$Image==-1), which(m_mod$Expect==-1), which(m_mod$ProdQ==-1), which(m_mod$ServQ==-1), which(m_mod$Value==-1), which(m_mod$EPSI==-1), which(m_mod$Loyal==-1))
sat_mod = rep("A", 7) # Set the mode of the meausuremant model
vars = m_mod$Manifest
n_manifests = length(vars)
vars[length(vars) + 1] = "Q1" #Add Q1
vars[length(vars) +1] = "CODERESP" #Add respondent id
#-----------------------------------------------------------------------------------------------------
# Model setup done
#-----------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------
# Data setup
#-----------------------------------------------------------------------------------------------------
df_org <- read_spss(data_file)
# Subset selection here
df = df_org[df_org$Q1==1,]
selection = attr(df_org$Q1, "labels")[1]
# Iteration setup here



levels(df$Q1)
plsdata <- df[, vars[1:n_manifests]]
# Impute data
for(i in 1:n_manifests){
  plsdata[[i]] = impute(plsdata[[i]], mean) 
}
anyNA(plsdata)

#-----------------------------------------------------------------------------------------------------
# Step for select subset data to run analysis on
#-----------------------------------------------------------------------------------------------------


plsdata = cbind(plsdata,as.data.frame(df[, vars[(n_manifests+1):(n_manifests+2)]]))
satpls = plspm(plsdata, as.matrix(sat_path), sat_blocks, modes = sat_mod,boot.val = TRUE)

describe((rescale(satpls)-1)*100/9)
df_list <- split(plsdata, as.factor(plsdata$Q1)) #Data per Q1 in a list.
list_names = attr(plsdata$Q1, "labels")
Q1Names[1]


print(levels(plsdata$Q1)[as.numeric(1)])