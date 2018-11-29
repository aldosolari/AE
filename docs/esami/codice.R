# Analisi Esplorativa
# Codice messo a disposizione per la prova scritta e di Laboratorio

n <- nrow(X)
p <- ncol(X)
t(X)
summary(X)
var(X)
cor(X)
plot(X)
plot(x2 ~ x1)
boxplot(x)
boxplot.stats(x)
bagplot(X)
bagplot(X)$pxy.outlier
cloud(x3 ~ x1 + x2, group=x4, data=dati)
faces(dati, scale=TRUE)
stars(dati, scale=TRUE)
coplot(x3 ~ x2 | x1, data = dati)

# .. to be continued