


# carga e preparação dos dados -----------------------------------------------------------------------------------------------------------

# carga do arquivo
ds <- read.csv(file=file.path("C:/Mestrado/NovaVersao/Experiment1/data/novo_log_sara.csv"))

# pre-processamento das colunas de data
date_columns = c(created = "sys_created_at", opened = "opened_at", updated = "sys_updated_at", resolved = "resolved_at", closed = "closed_at")
dateFormat <- "%d/%m/%Y %H:%M"
for(i in 1:length(date_columns)) {
   ds[,date_columns[i]] <- as.POSIXct(strptime(ds[,date_columns[i]], dateFormat))
}

#  cria nova coluna para deixar generico no codigo
ds$updated_at <- ds$sys_updated_at

# cria campos com valor inteiro para a contagem do tempo
ds$created_at_stc <- as.integer(ds[, date_columns["created"]])
ds$updated_at_stc <- as.integer(ds[, date_columns["updated"]])
ds$opened_at_stc <- as.integer(ds[, date_columns["opened"]])
ds$resolved_at_stc <- as.integer(ds[, date_columns["resolved"]])
ds$closed_at_stc <- as.integer(ds[, date_columns["closed"]])

# gera contadores para modelo MTA
# elapsed: opcao 1 = updated - opened; opcao 2 = updated - created
ds$elapsed_stc <- ds$updated_at_stc - ds$opened_at_stc
#incidentevtlog$elapsed_stc <- incidentevtlog$updated_at_stc - incidentevtlog$created_at_stc

# remaining: opcao 1 = closed - updated; opcao 2 = resolved - updated
ds$remaining_stc <- ds$closed_at_stc - ds$updated_at_stc
# dúvida, pelo jeito há alguns casos de mais de um evento de fechamento,
# pois ficaram 24985 registros, enquanto que temos 24918 cases



# calculo da correlação com eventos de closed apenas --------------------------------------------------------------

# manter somente os eventos finais de cada incidente
ds_closed <- ds[which(ds$incident_state=="Closed"),]

# removendo colunas de data e o id do incidente (neste caso do closed também tem que tirar o incident_state)
rds <- ds_closed[,c(-1,-2,-10,-12,-14,-42:-35)]

# eta-squared
#install.packages("lsr")
library(lsr)
#et <- etaSquared(rds)
#etaSquared()
# precisa do modelo com o ANOVA primeiro

str(rds)

colunas <- colnames(rds)

# calculando para cada atributo exceto elapsed e remaining
etas <- NULL
for(i in 1:length(colunas)-2) {
   print(paste("Calculando para ",i,": ", colunas[i]))
   aov.ex = aov(elapsed_stc~colunas[i],data=rds)  #do the analysis of variance
   etas[i] <- etaSquared(aov.ex)
} # not working...

# indo um por um...
aov.ex = aov(elapsed_stc~caller_id,data=rds)
e <- etaSquared(aov.ex)

aov.ex = aov(elapsed_stc~reassignment_count,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~active,data=rds)
e <- rbind(e,etaSquared(aov.ex))
# erro

aov.ex = aov(elapsed_stc~assigned_to,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~assignment_group,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~category,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~contact_type,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~impact,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~knowledge,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~location,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~made_sla,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~notify,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~opened_by,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~priority,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~problem_id,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~reopen_count,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~resolved_by,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~subcategory,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~sys_mod_count,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~u_priority_confirmation,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~u_symptom,data=rds)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~urgency,data=rds)
e <- rbind(e,etaSquared(aov.ex))





write.csv2(e, file="correlacao-somente-closed.csv", row.names=TRUE)

# TODOS OS EVENTOS -------------------


# fazer agora para todos os eventos


# removendo colunas de data e o id do incidente
rds2 <- ds[,c(-1,-10,-12,-14,-42:-35)]

eta_closed <- e

e <- NULL

aov.ex = aov(elapsed_stc~active,data=rds2)
e <- rbind(e,etaSquared(aov.ex))
# erro

aov.ex = aov(elapsed_stc~assigned_to,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~assignment_group,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~category,data=rds2)
e <- rbind(e,etaSquared(aov.ex))


aov.ex = aov(elapsed_stc~contact_type,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~impact,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~knowledge,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~location,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~made_sla,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~notify,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~opened_by,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~priority,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~problem_id,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~reopen_count,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~resolved_by,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~subcategory,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~sys_mod_count,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~u_priority_confirmation,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~u_symptom,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~urgency,data=rds2)
e <- rbind(e,etaSquared(aov.ex))



aov.ex = aov(elapsed_stc~caller_id,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~reassignment_count,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

aov.ex = aov(elapsed_stc~incident_state,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

write.csv2(e, file="correlacao-todos.csv", row.names=TRUE)


# problema com o tamanho do caller_id
rds2$caller_id_num <- as.numeric(rds2$caller_id)
str(rds2$caller_id_num)
aov.ex = aov(elapsed_stc~caller_id_num,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

write.csv2(e, file="correlacao-todos.csv", row.names=TRUE)

# teste com opened_by
rds2$opened_by_num <- as.numeric(rds2$opened_by)
str(rds2$opened_by_num)
aov.ex = aov(elapsed_stc~opened_by_num,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

# dá um resultado muito diferente, quando executado sobre o campo com os valores originais, factor

write.csv2(e, file="correlacao-todos.csv", row.names=TRUE)


for(i in 1:nrow(rds2)) {
   rds2[i,"caller_id_reduc"] <- substring(rds2[i,"caller_id"],8)
}
aov.ex = aov(elapsed_stc~caller_id_reduc,data=rds2)
e <- rbind(e,etaSquared(aov.ex))

# ainda não deu...


# Limpeza ---------------------------------------------------------

rm(f1,f2,f3,f4,f5)
rm(one_perc,one_perc_cases,one_perc_set_states)
rm(etas)
rm(aov.ex)



# Mais tentativas anteriores ---------------------------------------------------------

aov.ex1 = aov(elapsed_stc~caller_id,data=rds)  #do the analysis of variance
e <- etaSquared(aov.ex1)


# tentativa com matrix de fatores numéricos (usando a representação interna)

#criando a matrix de fatores numéricos
fds <- data.frame(data.matrix(rds))

aov.ex2 = aov(elapsed_stc~caller_id,data=fds)  #do the analysis of variance
etaSquared(aov.ex2)


aov.ex2 = aov(elapsed_stc~as.numeric(caller_id),data=rds)  #do the analysis of variance
etaSquared(aov.ex2)

rfds <- cbind(elapsed_real=rds$elapsed_stc,fds)
aov.ex2 = aov(elapsed_real~caller_id,data=rfds)  #do the analysis of variance
etaSquared(aov.ex2)


# re-scaling the matrix of indexes
fds$incident_state <- as.integer(as.character(fds$incident_state))
str(fds)

sds <- data.frame(scale(fds, center = TRUE, scale = TRUE))

rfds <- cbind(elapsed_real=sds$elapsed_stc,sds)
aov.ex2 = aov(elapsed_real~caller_id,data=rfds)  #do the analysis of variance
etaSquared(aov.ex2)





# -------- nada mais funcionou então tem que usar como categórico mesmo...
aov.ex1 = aov(elapsed_stc~subcategory,data=rds)  #do the analysis of variance
etaSquared(aov.ex1)

#eta.sq eta.sq.part
#assigned_to 0.2641364   0.2641364

#eta.sq eta.sq.part
#assignment_group 0.3191299   0.3191299

#eta.sq eta.sq.part
#u_symptom 0.1912505   0.1912505

#eta.sq eta.sq.part
#subcategory 0.1987251   0.1987251




# outras tentativas ------------------------------------------------------------------------------------------------


# campo active (dois valores possíveis)
aov.ex1 = aov(elapsed_stc~active,data=rds)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(remaining_stc~active,data=rds)        #graphical summary


# converting to minutes instead of seconds
rds$remaining_stc_min <- rds$remaining_stc / 60

aov.ex1 = aov(remaining_stc_min~active,data=rds)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(remaining_stc_min~active,data=rds)        #graphical summary




# correlation - tentativas anteriores -----------------------------------------------------------------------------------------------------------

cor(rds)


# c("pearson", "kendall", "spearman")
cor.test(fds$remaining_stc,fds$caller_id, method = "pearson")
cor.test(fds$remaining_stc,fds$caller_id, method = "kendall") #demora bastante...
cor.test(fds$remaining_stc,fds$caller_id, method = "spearman")


chisq.test(x = fds$remaining_stc, y = fds$caller_id)
#Error: cannot allocate vector of size 1.0 Gb

fisher.test(x = fds$remaining_stc, y = fds$caller_id)
#FEXACT error 40. Out of workspace.



install.packages("GGally")
library(GGally)
ggcorr(fds)


library(corrplot)
library(RColorBrewer)

# use = "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".

correlation_matrix <- cor(fds, method = "spearman", use = "everything")

correlation_matrix[is.na(correlation_matrix)] <- 0

corrplot(correlation_matrix, method = "square", type = "upper",
         tl.col = "black", order = "hclust", col = brewer.pal(n = 5, name = "RdYlBu"))


# re-scaling the matrix of indexes
sds <- data.frame(scale(fds, center = TRUE, scale = TRUE))

correlation_matrix <- cor(sds, method = "spearman", use = "everything")

correlation_matrix[is.na(correlation_matrix)] <- 0

corrplot(correlation_matrix, method = "square", type = "upper",
         tl.col = "black", order = "hclust", col = brewer.pal(n = 5, name = "RdYlBu"))

cor.test(sds$remaining_stc,sds$caller_id, method = "spearman")
chisq.test(x = sds$remaining_stc, y = sds$caller_id)



