install.packages("readxl")
install.packages('Benchmarking')
install.packages("corrgram")
install.packages("mnormt")
install.packages("sandwich")
install.packages("corrplot")
install.packages("GGally")
install.packages("carData")
install.packages("AER")
install.packages("ggplot2movies")
install.packages("dplyr")
install.packages("stargazer")
install.packages("car")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("lmtest")
install.packages("ggplot2")
install.packages("erer")
install.packages("gridExtra")
install.packages("usethis")
install.packages("caret")
install.packages("pscl")
install.packages("MASS")
install.packages("readr")
install.packages("forcats")
install.packages("RColorBrewer")
install.packages("wesanderson")
install.packages("plm")
install.packages("nnet")
install.packages("tidyr")
install.packages("viridisLite")
install.packages("flexclust")
install.packages("VGAM")

library(AER)
library(corrplot)
library(ggplot2)
library(readxl)
library(dplyr)
library(stargazer)
library(car)
library(mnormt)
library(FactoMineR)
library(factoextra)
library(lmtest)
library(ggplot2)
library(GGally)
library(corrgram)
library(gridExtra)
library(sandwich)
library(usethis)
library(caret)
library(pscl)
library(MASS)
library(erer)
library(readr)
library(forcats)
library(RColorBrewer)
library(wesanderson)
library(plm)
library(car)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(mnormt)
library(sandwich)
library(corrplot)
library(GGally)
library(corrgram)
library(Benchmarking)
library(nnet)
library(readxl)
library(tidyr)
library(viridis)
library(flexclust)
library(VGAM)

###############################################
#DEA
##############################################Data1 <- read_excel("DIPLOMA_Data_1_Short.xlsx")
Data1 <- read_excel("Data1_Main.xlsx")
View(Data1)
str(Data1)
summary(Data1)
Data1.1 <- as.data.frame(Data1)
stargazer(Data1.1,type = "text", out = "Data1.1.html" )
#���������� ������
X <- read_excel("X_Main.xlsx")
View(X)
Y <- read_excel("Y_Main.xlsx")
View(Y)
X_new <-as.data.frame(X)
Y_new <-as.data.frame(Y)
e_vrs <- dea(X_new,Y_new, RTS="vrs", ORIENTATION="in")
eff(e_vrs)
EFF<-as.matrix(eff(e_vrs),ncol = 20)
#vrs - ���������� ������ �� ��������
#crs - ���������� ������ �� ��������
e_crs <- dea(X_new,Y_new, RTS="crs", ORIENTATION="in")
eff(e_crs)
#e_vrs <- dea(X1,Y1, RTS="vrs", ORIENTATION="in", NAMES=TRUE)
peers(e_vrs)
lambda(e_vrs)
str(Y)
e_vrs <- dea(X_new,Y_new,RTS='vrs')
e_drs <- dea(X_new,Y_new,RTS='drs')
e_crs <- dea(X_new,Y_new,RTS='crs')
se <- eff(e_crs)/eff(e_vrs) 
round(se*1000)
abs(eff(e_vrs)- eff(e_drs)) <= 0 #test if DRS eff = VRS eff : ������� ������� �����

#############################
#����� ������� ���������
DataShort <- read_excel("Data2_Main_Ad.xlsx", 
                        col_types = c("text", "numeric", "text", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
View(DataShort)
#������ ��������
DataShort1 <- na.omit(DataShort)
#��������� ���������� ����� ����������� � ���� ������������� � ���������
DataShort2 <-mutate(DataShort1,  Place=as.factor(Place), Ownership=as.factor(Ownership))
DataShort3 <- as.data.frame(DataShort2)
str(DataShort3)
View(DataShort3)
#������� ������ ���������� ���� numeric
DataPCA<-DataShort3[,-c(1,3,4,5,8,9,10,12,13,18,20,22,23)]
#������ �� ������� ����������, ������� ����� ����������� ������ �������
View(DataPCA)
mod <- PCA(DataPCA)
#����������� ����� � ����������� �������
mod_eigen <- eigen(var(DataPCA))
mod_eigen$values
mod$eig
#����������� �������
mod$svd$V
mod_eigen$vectors
#������� ��������
View(mod$var$coord)
corrplot(mod$var$coord, is.corr = FALSE,tl.cex = 0.9,tl.col = "black")
#������ ���������� �����
fviz_eig(mod, addlabels = TRUE, barfill="lightgreen")
#��������� ���������� � ����� ������
fviz_pca_ind(mod)
#���������� ����������
fviz_pca_ind(mod, pointsize = "cos2", pointshape = 21, fill = "lightblue", repel = TRUE)
#��� ������ �����, ��� ����� ����� ������� ��������� ��������� ������ ����������.

#�������������
mod_clust <- HCPC(mod)
mod_clust$data.clust
fviz_cluster(mod_clust, pointshape = 6, pointsize = 2, geom = "point") 
Clust <- mod_clust$desc.var
#������������
fviz_dend(mod_clust, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8)
##########################################

#��������� �� ��������� ������
#�������� ������ ��� ���������
Data2 <- read_excel("Data2_Main.xlsx", 
                    col_types = c("text", "numeric", "numeric", 
                                  "text", "text", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))
View(Data2)
#������ ��������
data2 <- na.omit(Data2)
View(data2)
#��������� ���������� �������, ����� ����������� � ����� ������������� � ���������
data3 <-mutate(data2, t=as.factor(t), Place=as.factor(Place), Ownership=as.factor(Ownership))
names(data3)
str(data3)
#������������ ����������
summary(data3)
Data3 <- as.data.frame(data3)
stargazer(Data3,type = "text", out = "Data3.html" )
data_new <-dplyr::select(data3, -t, -Place, -Name, -Ownership)
str(data_new)
corr <- corrplot(cor(data_new), type = "upper",  tl.cex = 0.7,tl.col = "black")
#�������
ggplot(data = Data3, aes(x = Place , y = DEA_result)) + geom_boxplot(aes(fill = Data3$Place), color="black")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("������") + ylab("�������������")+ theme(legend.position = "none")
ggplot(data = Data3, aes(x = Place))+ geom_bar(width = 1, fill = 'lightblue', color="black")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  xlab("������") + ylab("���������� ����������")
ggplot(data = Data3, aes(x = Ownership, y = DEA_result ))+ geom_violin(aes(fill = Ownership)) + scale_fill_brewer(palette = "Spectral")+ xlab("����� �������������") + ylab("�������������")+labs(fill = "����� �������������")
ggplot(data = Data3, aes(x = Ownership, y = DEA_result ))+ geom_boxplot(aes(fill = Ownership)) + scale_fill_brewer(palette = "Spectral")+ xlab("����� �������������") + ylab("�������������")+labs(fill = "����� �������������")
Data3New2 <- Data3 %>% group_by(Ownership, t) %>% transmute(median_LoansToOrganizations = median(LoansToOrganizations),median_LoansToIndividuals=median(LoansToIndividuals),median_LoansToOtherBanks=median(LoansToOtherBanks))
Data3New2
Data3New2 <- Data3New2 %>% pivot_longer(cols = c("median_LoansToIndividuals","median_LoansToOrganizations", "median_LoansToOtherBanks"), names_to = "Loans", values_to = "Value")
Data3New2 <- Data3New2 %>% group_by(Loans)
ggplot(Data3New2, aes(x=t, y=Value, group=Loans)) +
  geom_line(aes(colour=Loans))+
  geom_point(aes(colour=Loans,size=2))+facet_wrap(~Ownership)+
  scale_color_manual(values = c("#F25905", "#02988B","#5A2C87"),labels = c("�������, �������� ���������� �����","�������, �������� ����������� �����","�������, �������� ������ ������"))+
  xlab("���") + ylab("��������� �������� �����������") +
  theme_bw()+theme(legend.position = "bottom",strip.background = element_rect(fill = "#E2DFFD"),strip.text.x = element_text(size = 14)) +guides(size = FALSE)+
  guides(color = guide_legend(nrow=3), shape=FALSE)
Data3New <- Data3 %>% pivot_longer(DepositsOfOrganizations:DepositsOfIndividuals, names_to = "type_of_Deposits", values_to = "valuation")
ggplot(Data3New,aes(x=t,y=valuation,fill=type_of_Deposits)) +   geom_bar(stat="identity",position="stack", width=0.4) + theme(legend.position = "right") + xlab("���") + ylab("����� �������� �������� ���������") + labs(fill = "��� ��������")
Data3New3 <- Data3 %>% group_by(Ownership, t) %>% transmute(median_ClientFunds = median(ClientFunds))
ggplot(data = Data3New3, aes(x = t, y = median_ClientFunds, group = Ownership, color = Ownership)) + geom_line(size=1) + geom_point(size=5) +
  scale_color_manual(values = c("#D46E77", "#3D3D4D","#9898A9","#6496CE", "#E39179","#5A8547","#C78DDD"))+
  xlab("���") + ylab("�������� ��������") +guides(size = FALSE)+ guides(color = guide_legend(nrow=7), shape=FALSE)
ggplot(data = Data3, aes(x = H1, y = DEA_result )) + geom_point(aes(col=Age, size=DEA_result,lpha=3)) +geom_smooth(method="loess",colour = I("black"), se=F) + geom_smooth(method="lm",colour = I("red"), se=F)+ xlab("�������� �1") + ylab("�������������")


#������ pooled
mod1 <- plm(data = Data3 , DEA_result ~ GDP + Inflation + Age + Size+ AccountsBankOfRussia +LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+ DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE +Place +Ownership, model = "pooling",index = c("Name", "t"))
summary(mod1)
#������ � �������������� ���������
mod2 <- plm(data = data3 , DEA_result ~ GDP + Inflation + Age + Size+ AccountsBankOfRussia +LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+  DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE + Place + Ownership, model = "within",index = c("Name", "t"))
summary(mod2)
#������ �� ���������� ���������
mod3 <- plm(data = data3 , DEA_result ~ GDP + Age + Size+ AccountsBankOfRussia +LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+  DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE + Place + Ownership, model = "random",index = c("Name", "t"))
summary(mod3)
#T��� �������� (F� ��� R�).
phtest(mod2, mod3)
#p-value = 0, ������, H0 �����������, � �� �������� ������ � �������������� ���������.
#���� ������-������ (R� ��� pooled)
plmtest(mod3, type = "bp")
#p-value = 0, ������, H0 �����������, � �� �������� ������ �� ���������� ���������.
#���� FE ��� pooled.
pFtest(mod2, mod1)
#p-value = 0, ������, H0 �����������, � �� �������� ������ � �������������� ���������.
stargazer(list(mod1, mod2, mod3), column.labels = c("Pooling", "FE", "RE"), type = "text",out = "mods.html")
#�� ����������� ������ �������� ������ � �������������� ���������. 
str(Data3)
model0 <- lm(data = Data3, DEA_result ~  AccountsBankOfRussia+ Size + LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+ DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE +Name +t)
summary(model0)
stargazer(model0, type = "text",out = "model.html")
vif(model0)
stargazer(vif(model0), type = "text",out = "vif(model0).html")
#���������� ������� Size
model <- update(model0, .~ . -Size)
vif(model)
AIC(model0,model)
stargazer(vif(model), type = "text",out = "vif(model).html")
#� ��������������������� �� � �������

#������������ ��������
plot(model,which = 2)
#��������� ����������� �� ����� �������
boxCox(model)
#����� 0 � 1, ��������� ������ log
model1 <- lm(data=Data3, log(DEA_result) ~ AccountsBankOfRussia +LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+ DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE+ Name +t)
summary(model1)
plot(model1, which=2)
#�� ������� �����, ��� ����� �����.

#������ ���������� ����������
model2 <- stepAIC(model1)
summary(model2)
AIC(model2,model1)
stargazer(list(model1,model2), column.labels = c("������ �1", "������ �2"), type = "text",out = "models.html")

#������ ���������� ������������
plot(model2, which = 1)
#�����, ��� ������ ���������������� ����� ���������
crPlots(model2)
#�������, �������� � ROE � Capital
#���� ������
resettest(model2, power=2)
#������� �������� �� ����������� �� 0,1% ������
#��, ��������, ����� �����, ��������� ����� ROE^2
model3.1 <- update(model2, . ~ .+ I(Capital^2))
model3.2 <- update(model2, . ~ .+ I(Capital^2)+ I(ROE^2))
model3 <- update(model2, . ~ .+ I(ROE^2))
summary(model3.1)
stargazer(model3.1, type = "text",out = "model3.1.html")
#(Capital^2) �� ������
summary(model3.2)
stargazer(model3.2, type = "text",out = "model3.2.html")
summary(model3)
stargazer(model3, type = "text",out = "model3.html")
plot(model3, which = 1)
crPlots(model3)
resettest(model3, power=2)
#����� ������� �����

#��������������������
plot(model3,which = 3)
#�� ������� ��������� ��������� ��������������������
#���� ������-������
bptest(model3)
#����� ������������ ��������� ������
V_new <- vcovHC(model3, type = "HC0")
coeftest(model3,V_new)

#��������� ����������� ����������
plot(model3, which = 4)
#��������� ���������� ����
Cook_mod<-cooks.distance(model3)
L <- which(Cook_mod>3/100)
L
Data4 <- Data3[-L,]
model4 <- update(model3, . ~ .,data = Data4)
summary(model4)

#��������� ��������
e <-resid(model4)
g <-e/sd(e)
barplot(abs(g))
influencePlot(model4)
#������ �������
D <- which(abs(g)>4)
D
Data5 <- Data4[-D,]
View(Data5)
model5 <- update(model4, . ~ .,data = Data5)
summary(model5)
V_new_N <- vcovHC(model5, type = "HC0")
coeftest(model5,V_new_N)
stargazer(coeftest(model5,V_new_N), type = "text",out = "model2.html")
AIC(model5)
#R^2 �����
model6 <-stepAIC(model5)
summary(model6)
V_new <- vcovHC(model6, type = "HC0")
coeftest(model6,V_new)
stargazer(coeftest(model6,V_new), type = "text",out = "model3.html")
AIC(model6)

########################################################################              
#������ �� ���������������� ������
names(DataShort2)
str(DataShort2)

#������������ ����������
summary(DataShort2)
DataShort3 <- as.data.frame(DataShort2)
stargazer(DataShort3,type = "text", out = "DataShort3.html" )
ggplot(data = DataShort3, aes(x = ROA, y = log(DEA_result) )) + geom_point(colour = 'darkgreen', aes(alpha=3), size = 2) +geom_smooth(colour = I("black"), size = 1) 
modelS1 <- lm(data = DataShort3 , DEA_result ~ Size + �ge + AccountsBankOfRussia +LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+ DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE+ Place + Ownership)
summary(modelS1)
stargazer(modelS1, type = "text",out = "modelS1.html")
vif(modelS1)
#� ��������������������� �� � �������

#������������ ��������
plot(modelS1,which = 2)
#��������� ����������� �� ����� �������
boxCox(modelS1)
#����� 0 � 1, ��������� ������ log
modelS2 <- lm(data=DataShort3, log(DEA_result) ~ Size + �ge + AccountsBankOfRussia +LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+ DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE+ Place + Ownership)
summary(modelS2)
plot(modelS2, which=2)
#�� ������� �����, ��� ����� �����.

#������ ���������� ������������
plot(modelS2, which = 1)
#�����, ��� ������ ���������������� ����� ���������
crPlots(modelS2)
#�������, �������� � ROA
#���� ������
resettest(modelS2, power=2)
#������� �������� �� ����������� �� 5% ������

#��������������������
plot(modelS2,which = 3)
#�� ������� ��������� ��������������������
#���� ������-������
bptest(modelS2)
#������, �� ����� ����� ������������ ��������� ������
V_new <- vcovHC(modelS2, type = "HC0")
coeftest(modelS2,V_new)

#������ ���������� ����������
modelS3 <- update(modelS2, .~. -�ge -AccountsBankOfRussia -DepositsOfOrganizations - ProvisionsForPossibleLosses - Capital - LoanPortfolioCoverageRatio - LoanPortfolioCollateralRatio - OverdueDebtRatio - ROA -ROE - Place)
summary(modelS3)
AIC(modelS2,modelS3)
V_new1 <- vcovHC(modelS3, type = "HC0")
coeftest(modelS3,V_new1)
stargazer(modelS3, type = "text",out = "modelS3.html")

#��������� ����������� ����������
plot(modelS3, which = 4)
#��������� ���������� ����
Cook_mod<-cooks.distance(modelS3)
K <- which(Cook_mod>5/100)
K
DataShort4 <- DataShort3[-K,]
modelS4 <- update(modelS3, . ~ .,data = DataShort4)
summary(modelS4)

#��������� ��������
e <-resid(modelS5)
g <-e/sd(e)
barplot(abs(g))
influencePlot(modelS5)
#������ �������
N <- which(abs(g)>3)
N
DataShort5 <- DataShort4[-N,]
modelS5<- update(modelS4, . ~ .,data = DataShort5)
summary(modelS5)
#R^2 �����
V_new2 <- vcovHC(modelS5, type = "HC0")
coeftest(modelS5,V_new2)
stargazer(coeftest(modelS5,V_new2), type = "text",out = "modelS5.html")
#�������
nw <- data.frame(Size=c(10,12.5,10),LoansToOrganizations=c(0.25,0.25,0.25), LoansToIndividuals=c(0.13,0.13,0.13), LoansToOtherBanks=c(0.09,0.09,0.09), ClientFunds=c(0.6,0.6,0.6),
                 BankSecurities=c(0.12,0.5,0.12), DepositsOfIndividuals=c(0.3,0.3,0.3), H1=c(0.33,0.33,0.33), H3=c(2,2,2), Ownership = c("������� �������������","������� �������������","����������� �������������"))
exp(predict(modelS5, newdata=nw))

##############################################
##������ ��� �������� �� ������������ ����������� (������������)
#############################################
Data3_Main <- read_excel("Data3_Main.xlsx", 
                         col_types = c("text", "numeric", "numeric", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))
View(Data3_Main)
modalLagged <- lm(data=Data3_Main, log(DEA_result) ~ AccountsBankOfRussia +LoansToOrganizations + LoansToOtherBanks + BankSecurities + DepositsOfOrganizations + DepositsOfIndividuals + H3 + LoanPortfolioCoverageRatio + OverdueDebtRatio + ROE+ I(ROE^2) + Name +t)
summary(modalLagged)
stargazer(list(model6,modalLagged), column.labels = c("������ �1", "������ �2"), type = "text",out = "modelLag.html")


V_new3 <- vcovHC(modalLagged, type = "HC0")
coeftest(modalLagged,V_new3)
stargazer(coeftest(modalLagged,V_new3), type = "text",out = "modalLagged.html")

##############################################
###������ � ������������������� �����������
str(Data3)
M0 <- lm(data = Data3, DEA_result ~  Place+ Ownership +Age + Size + GDP +Inflation +AccountsBankOfRussia+ LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+ DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE)
summary(M0)
stargazer(M0, type = "text",out = "M0.html")
vif(M0)
#� ��������������������� �� � �������

#������������ ��������
plot(M0,which = 2)
#��������� ����������� �� ����� �������
boxCox(M0)
#����� 0 � 1, ��������� ������ log
M1 <- lm(data=Data3, log(DEA_result) ~ Place+ Ownership +Age + Size + GDP +Inflation +AccountsBankOfRussia+ LoansToOrganizations+  LoansToIndividuals + LoansToOtherBanks + ClientFunds + BankSecurities+ DepositsOfOrganizations + DepositsOfIndividuals+ ProvisionsForPossibleLosses + Capital + H1 + H3 + LoanPortfolioCoverageRatio + LoanPortfolioCollateralRatio + OverdueDebtRatio + ROA + ROE)
summary(M1)
plot(M1, which=2)
#�� ������� �����, ��� ����� �����.

#������ ���������� ����������
M2 <- update(M1, .~. -LoanPortfolioCoverageRatio - LoanPortfolioCollateralRatio - OverdueDebtRatio - ROA - ROE - ClientFunds - Age - ProvisionsForPossibleLosses - AccountsBankOfRussia)
summary(M2)
AIC(M1,M2)
stargazer(list(M1,M2), column.labels = c("������ �1", "������ �2"), type = "text",out = "modelsM.html")

#������ ���������� ������������
plot(M2, which = 1)
#�����, ��� ������ ���������������� ����� ���������
#���� ������
resettest(M2, power=2)
#������� �������� �� ����������� �� 5% ������

#��������������������
plot(M2,which = 3)
#�� ������� ��������� ��������� ��������������������
#���� ������-������
bptest(M2)
#����� ������������ ��������� ������
V_new <- vcovHC(M2, type = "HC0")
coeftest(M2,V_new)

#��������� ����������� ����������
plot(M2, which = 4)
#��������� ���������� ����
Cook_mod<-cooks.distance(M2)
B <- which(Cook_mod>1/100)
B
Data4 <- Data3[-B,]
M3 <- update(M2, . ~ .,data = Data4)
summary(M3)
V_new3 <- vcovHC(M3, type = "HC0")
coeftest(M3,V_new3)

#��������� ��������
e <-resid(M3)
g <-e/sd(e)
barplot(abs(g))
influencePlot(M3)
#������ �������
Q <- which(abs(g)>4)
Q
Data5 <- Data4[-Q,]
M4 <- update(M3, . ~ .,data = Data5)
summary(M4)
V_new5 <- vcovHC(M4, type = "HC0")
coeftest(M4,V_new5)
stargazer(coeftest(M4,V_new5), type = "text",out = "M4.html")






