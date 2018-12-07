# carga do arquivo
ds <- read.csv(file=file.path("data/novo_log_sara.csv"))

# pre-processamento das colunas de data
date_columns = c(created = "sys_created_at", opened = "opened_at", updated = "sys_updated_at", resolved = "resolved_at", closed = "closed_at")
dateFormat <- "%d/%m/%Y %H:%M"
for(i in 1:length(date_columns)) {
   ds[,date_columns[i]] <- as.POSIXct(strptime(ds[,date_columns[i]], dateFormat))
}


# dates -----------------------------------------------------------------------------------------------------------

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

# removendo colunas de data agora
rds <- ds[,c(-14:-11,-42:-35)]

#criando a matrix de fatores numéricos
fds <- data.frame(data.matrix(rds))

str(fds)

# correlation -----------------------------------------------------------------------------------------------------------

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



# eta-squared
install.packages("lsr")
library(lsr)

et <- etaSquared(rds)

etaSquared()



