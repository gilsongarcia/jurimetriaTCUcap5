library(readxl)
library(tidyverse)

d<-tcu_total_denuncia
dp <- d %>% filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))

dp <- d %>% filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))
dp <- d %>% filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))
p <- d %>% filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))
d1<-d
d <- d %>% filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))
d<-d %>% mutate(Ano_julgado = str_sub(Data,7,10))
d<-d %>% mutate(Ano_processo=str_sub(Processo,9,12))
d<-d %>% mutate(Ano_julgado=as.integer((Ano_julgado)))
d<-d%>% mutate(Ano_processo=as.integer(Ano_processo))
d<-d %>% mutate(Duracao=Ano_julgado-Ano_processo)
d<-d %>% mutate(Duracao=as.integer(Duracao))
d<-d %>% mutate(mp=case_when(str_detect
                             (d$`Representante do Ministério Público`,
                               "não atuou|Não atuou|não atuou.|Não atuou.|não há")~
                               "nao atuou",TRUE~"atuou"))
p<-d
p <- p %>% mutate(decisao=as.factor(decisao))

p <- p %>% mutate(decisao=as.factor(decisao))
p <- p %>% mutate(decisao=as.factor(decisao))
p <- p %>% mutate(decisao=case_when(str_detect(Sumário,
                                               "(?i)(IMPROCEDÊNCIA|improcedente|não procedente|não procedência)")~
                                      "improcedente",TRUE~"procedente"))
p <- p %>% mutate(decisao=as.factor(decisao))

p <- p %>% mutate(decisao=as.factor(decisao))
p <- p %>% mutate(decisao=case_when(str_detect(Sumário,
                                               "(?i)(IMPROCEDÊNCIA|improcedente|não procedente|não procedência)")~
                                      "improcedente",TRUE~"procedente"))
p <- p %>% mutate(decisao=as.factor(decisao))
p <- p %>% mutate(decisao=as.factor(decisao))
p1<-p
p$reexame<-str_detect(p$Sumário,"(?i)(reexame|embargos|agravo)")
p$reexame<-str_detect(p$Sumário,"(?i)(reexame|embargos|agravo)")
p$reexame<-str_detect(p$Sumário,"(?i)(reexame|embargos|agravo)")
table(p$reexame)
p <- p %>%
  filter(reexame == "FALSE")
p <- p %>% mutate(decisao1=case_when(str_detect(decisao,
                                                "(?i)(improcedente)")~"0",TRUE~"1"))
p <- p %>% mutate(decisao1=case_when(str_detect(decisao,
                                                "(?i)(improcedente)")~"0",TRUE~"1"))
p <- p %>% mutate(decisao1=as.factor(decisao1))
p <- p %>% mutate(mp=as.factor(mp))
p <- p %>% mutate(Relator=as.factor(Relator))
p$cautelar <- str_detect(p$Sumário, "(?i)(cautelar)")
p$licitacao <- str_detect(p$Sumário, "(?i)(licitação)")
glm.fit=glm(decisao1~p$Duracao+p$Ano_julgado+p$Relator+
              mp+p$cautelar+p$licitacao,
            data = p , family = binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3873)
glm.pred[glm.probs >.5]="1"
table(glm.pred,p$decisao1)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",591)
glm.pred[glm.probs >.5]="1"
table(glm.pred,p$decisao1)
mean(glm.pred==p$decisao1)
glm.fit=glm(decisao1~p$Relator+p$Duracao
            p$cautelar+p$licitacao+ mp,
            data = p , family = binomial)
glm.fit=glm(decisao1~p$Relator+p$Duracao
            p$cautelar+p$licitacao+ mp,
            data = p, family = binomial)
glm.fit=glm(decisao1~p$Relator+p$Duracao+
              p$cautelar+p$licitacao+ mp,data = p , family = binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",591)
glm.pred[glm.probs >.5]="1"
table(glm.pred,p$decisao1)
mean(glm.pred==p$decisao1)
summary(glm.fit)
table(p$decisao1)
coef(glm.fit)
ggplot(p,aes(x=Ano_julgado, fill=decisao))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("decisão", values = c("procedente" = "black", "improcedente" = "white"))+
  labs(x="ano do julgado", y="frequência")+theme(legend.position="top")
ggplot(p,aes(x=decisao))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("cautelar", values = c("1" = "black", "0" = "white"))+
  labs(x="decisão", y="frequência")+theme(legend.position="right")
ggplot(p,aes(x=Ano_processo, fill=decisao))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("decisão", values = c("procedente" = "black", "improcedente" = "white"))+
  labs(x="ano do processo", y="frequência")+theme(legend.position="top")
ggplot(p,aes(x=mp, fill=decisao))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("decisão", values = c("procedente" = "black", "improcedente" = "white"))+
  labs(x="ministério público", y="frequência")+theme(legend.position="top")
ggplot(p,aes(x=Duracao, fill=decisao))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("decisão", values = c("procedente" = "black", "improcedente" = "white"))+
  labs(x="Duração (anos)", y="frequência")+theme(legend.position="top")
coef(glm.fit)
exp(coef(glm.fit))
mean(glm.pred==p$decisao1)
table(glm.pred,p$decisao1)
mean(glm.pred==p$decisao1)
plot(sort(glm.probs), xlab="decisões", ylab="probabilidade")
d1<-d1 %>% mutate(conhecimento=case_when(str_detect
                                         (d$`Representante do Ministério Público`,
                                           "não conheci")~
                                           "nao conhecimento",TRUE~"conhecimento"))

d1<-d1 %>% mutate(conhecimento=case_when(str_detect
                                         (d$`Representante do Ministério Público`,
                                           "não conheci")~
                                           "nao conhecimento",TRUE~"conhecimento"))
d1<-d1 %>% mutate(conhecimento=case_when(str_detect
                                         (d1$Sumário,
                                           "não conheci")~
                                           "nao conhecimento",TRUE~"conhecimento"))
View(d1)
table(d1$conhecimento)
d1<-d1 %>% mutate(conhecimento=case_when(str_detect
                                         (d1$Sumário,
                                           "(?i)(não conheci")~
                                           "nao conhecimento",TRUE~"conhecimento"))
d1<-d1 %>% mutate(conhecimento=case_when(str_detect
                                         (d1$Sumário,
                                           "(?i)(não conheci")~
                                           "nao conhecimento",TRUE~"conhecimento"))


table(d1$conhecimento)
table(p$reexame)
table(p$decisao)
table(p$decisao1)
pr <- p %>% filter(str_detect(decisao1,"(?i)(1)"))
View(pr)
pr<-pr %>% mutate(procedencia=case_when(str_detect
                                        (d1$Sumário,
                                          "(?i)(parcia)")~
                                          "procedencia parcial",TRUE~"procedencia"))
pr<-pr %>% mutate(procedencia=case_when(str_detect
                                        (d1$Sumário,
                                          "(?i)(parcia)")~
                                          "procedencia parcial",TRUE~"procedencia"))
pr<-pr %>% mutate(procedencia=case_when(str_detect
                                        (d1$Sumário,
                                          "(?i)(parcial)")~
                                          "procedencia parcial",TRUE~"procedencia"))
pr<-pr %>% mutate(procedencia=case_when(str_detect
                                        (pr$Sumário,
                                          "(?i)(parcial)")~
                                          "procedencia parcial",TRUE~"procedencia"))
View(pr)
table(pr$procedencia)
table(d1$conhecimento)
table(p$decisao)
table(pr$procedencia)
t_ano_julgado<-as.data.frame(table(d1$Ano_julgado))
t_ano_processo<-as.data.frame(table(d1$Ano_processo))
View(d1)
d1<-d1 %>% mutate(Ano_julgado = str_sub(Data,7,10))

d1<-d1 %>% mutate(Ano_julgado = str_sub(Data,7,10))
d1<-d1 %>% mutate(Ano_processo=str_sub(Processo,9,12))
d1<-d1 %>% mutate(Ano_julgado=as.integer((Ano_julgado)))
d1<-d1%>% mutate(Ano_processo=as.integer(Ano_processo))
d1<-d1 %>% mutate(Duracao=Ano_julgado-Ano_processo)
View(d1)
t_ano_julgado<-as.data.frame(table(d1$Ano_julgado))
t_ano_processo<-as.data.frame(table(d1$Ano_processo))
View(t_ano_processo)
library(writexl)
write_xlsx(t_ano_processo,"t_ano_processo1.xlsx")
View(d1)
View(d1)
table(d1$Ano_processo)
t_ano_processo<-as.data.frame(table(d1$Ano_processo))
View(t_ano_processo)
