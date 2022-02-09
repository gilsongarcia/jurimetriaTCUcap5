library(tidyverse)
e<-tcu_emergencia_OU_emergencial_OU_calamidade_OU_calamitosa_E_dispensa_de_licitacao

d<-e %>% filter(str_detect(e$`Tipo de processo`,"(?i)(representação|denúncia)"))
p<-d %>%filter(str_detect(Sumário,"(?i)(PROCEDENTE|PROCEDÊNCIA|
IMPROCEDENTE|IMPROCEDÊNCIA)"))

r<-p %>% filter(str_detect(p$Sumário,"(?i)(reexame)"))

p$reexame <- str_detect(p$Sumário, "(?i)(reexame)")
table(p$reexame)

d1<-d
p1<-p

p<-p %>% filter(p$reexame=="FALSE")

glm.fit=glm(decisao1~p$Duracao+p$`Tipo de processo`+p$Relator+
              mp+p$ant,
            data = p , family = binomial)
coef(glm.fit)
summary(glm.fit)

glm.probs=predict(glm.fit,type="response")

glm.pred=rep("0",234)
glm.pred[glm.probs >.5]="1"
table(glm.pred,p$decisao1)
mean((glm.pred==p$decisao1))

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


prescricao <- cjsg_prescricao %>% 
  mutate(sumula_435 = ifelse(data_julgamento < as.Date("2010-04-13"),"anterior_sumula435","posterior_sumula435"),
         repetitivo_444 = ifelse(data_julgamento < as.Date("2019-05-08"),"anterior_repetitivo444","posterior_repetitivo444"))

install.packages("lubridate")

p$data<-lubridate::dmy(p$Data)

p<-p %>% mutate(ant = ifelse(data < as.Date("2007-09-12"),"anterior","posterior"))
table(p$ant)
table(p$Relator)

p <- p %>% mutate(decisao=case_when(str_detect(Sumário,
                                               "(?i)(IMPROCEDÊNCIA|improcedente|não procedente|não procedência)")~
                                      "improcedente",TRUE~"procedente"))
p <- p %>% mutate(decisao1=case_when(str_detect(decisao,
                                                "(?i)(improcedente)")~"0",TRUE~"1"))
p <- p %>% mutate(decisao1=as.factor(decisao1))

library(writexl)
write_xlsx(p,"p.xlsx")

rel<-as.data.frame(table(p$Relator))
MUNp<-as.data.frame(table(MUNp$Município))

write_xlsx(rel,"rel.xlsx")

p$R1<- str_detect(p$Relator, "(?i)(AUGUSTO SHERMAN)")
table(p$R1)

glm.fit=glm(decisao1~p$Duracao+p$`Tipo de processo`+p$R1+
              mp+p$ant,
            data = p , family = binomial)
coef(glm.fit)
summary(glm.fit)
p$R1<- str_detect(p$Relator, "(?i)(AUGUSTO SHERMAN)")

glm.fit=glm(p$decisao1~p$Duracao+p$`Tipo de processo`+p$Relator+
              p$mp+p$ant,data = p , family = binomial)

glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",234)
glm.pred[glm.probs >.5]="1"
table(glm.pred,p$decisao1)
mean(glm.pred==p$decisao1)

pred<-data.frame(Duracao=0,
                `Tipo de processo`=factor("DENÚNCIA (DEN)"),
                Relator=factor("ADYLSON MOTTA"),
                mp=factor("atuou"),
                ant=factor("anterior"))

pred$prob<-predict(glm.fit,newdata=pred,type="response")
pred$prob<-predict(glm.fit,newdata=pred)
#
#
predict(glm.fit,newdata=pred)

table(p$decisao1)

glm.probs=predict(glm.fit,type="response")

glm.fit=glm(p$decisao1~p$Duracao+p$`Tipo de processo`+p$Relator+
             p$mp+p$ant,data = p , family = binomial)

pred$prob<-predict(glm.fit,newdata=pred,type="response")

predict(glm.fit,newdata=pred,type="response")

glimpse(p)

#julio trecenti

glm.fit=glm(decisao1~Duracao+`Tipo de processo`+Relator1+
              mp+ant,data = p , family = binomial)

pred<-data.frame(Duracao=0,
                 `Tipo de processo`=factor("DENÚNCIA (DEN)"),
                 Relator=factor("ADYLSON MOTTA"),
                 mp=factor("atuou"),
                 ant=factor("anterior"))
glimpse(p)

# olha como fica a coluna Tipo de processo

pred
#>   Duracao Tipo.de.processo       Relator    mp      ant
#> 1       0   DENÚNCIA (DEN) ADYLSON MOTTA atuou anterior

pred<-tibble::tibble(Duracao=0,
                     `Tipo de processo`=factor("DENÚNCIA (DEN)"),
                     Relator=factor("ADYLSON MOTTA"),
                     mp=factor("atuou"),
                     ant=factor("anterior"))

# agora sim! pred

pred$prob<-predict(glm.fit,newdata=pred,type="response")
pred$prob
#>         1 
#> 0.8081983

#retorno

coef(glm.fit)
table(p$Relator)

p2<-p
p <- p %>% mutate(decisao1=as.integer(decisao1))
glimpse(p)

summary(glm.fit)

glm.fit=glm(decisao1~Duracao+tipo.de.processo+relator+
              mp+ant,data = p , family = binomial)

coef(glm.fit)

pred<-data.frame(Duracao=0,
                 tipo.de.processo=factor("DEN"),
                 relator=factor("R00"),
                 mp=factor("atuou"),
                 ant=factor("anterior"))

library(writexl)
write_xlsx(coef,"coef.xlsx")

pred$prob<-predict(glm.fit,newdata=pred,type="response")
pred$prob

write_xlsx(pred,"pred.xlsx")

pred1<-data.frame(Duracao=1,
                 tipo.de.processo=factor("REPR"),
                 relator=factor("R06"),
                 mp=factor("nao atuou"),
                 ant=factor("posterior"))

pred1$prob<-predict(glm.fit,newdata=pred1,type="response")
pred1$prob

improbidade <-     bind_rows(improbidade_advertencia,improbidade_inidoneidade,
                             improbidade_multa,improbidade_pena,
                             improbidade_sancao, improbidade_suspensao)
?distinct()
improbidade1 <- distinct(improbidade)
View(improbidade1)

predt<-bind_rows(pred,pred1)

glm.fit=glm(decisao1~tipo.de.processo+relator+
              +               mp+ant+Duracao,data = p , family = binomial)
glimpse(p)
p <- p  %>% mutate(Ano_processo=as.integer(Ano_processo))
p <- p  %>% mutate(Ano_julgado=as.integer(Ano_julgado))

summary(glm.fit)

glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",234)
glm.pred[glm.probs >.5]="1"
table(glm.pred,p$decisao1)
mean(glm.pred==p$decisao1)

table(p$Duracao)

table(p[c(20,25)])
durxdec<-as.data.frame(table(p[c(20,25)]))      

write_xlsx(durxdec,"durxdec.xlsx")

pro<-p %>% filter(str_detect(p$decisao1,"(?i)(1)"))
imp<-p %>% filter(str_detect(p$decisao1,"(?i)(0)"))

mean(p$Duracao)
mean(pro$Duracao)
mean(imp$Duracao)
