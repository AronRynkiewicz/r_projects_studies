library("factoextra")

df <- read.csv2("http://www.cs.put.poznan.pl/kgutowska/PSwBB/dane/przykladoweDane-Projekt.csv", sep = ";")
df$HCT[which(df$HCT==0.0423)] = 0.423
df$MON[which(df$MON==7.0)] = 0.70
df$ERY[which(df$ERY==33.0)] = 3.30

df[rowSums(is.na(df)) > 0,]
aggregate( HGB ~ grupa, df, mean )
aggregate(MON ~ grupa, df, mean )

which(is.na(df$HGB))
df$HGB[13] = 12.41141
df$HGB[68] = 11.26357
which(is.na(df$MON))
df$MON[5] = 0.8579167

df[df$grupa == 'CHOR1', 1] = 1
df[df$grupa == 'CHOR2', 1] = 2
df[df$grupa == 'KONTROLA', 1] = 0

df[df$plec == 'm', 2] = 0
df[df$plec == 'k', 2] = 1

df <- transform(df, grupa = as.numeric(grupa))
df <- transform(df, plec = as.numeric(plec))

result.prcomp <- prcomp(df, scale = TRUE, center = TRUE)

result.prcomp
get_eigenvalue(result.prcomp)

get_pca_ind(result.prcomp)
get_pca_var(result.prcomp)$coord
get_pca_var(result.prcomp)$cos2
get_pca_var(result.prcomp)$contrib


get_pca_var(result.prcomp)


fviz_eig(result.prcomp, addlabels = TRUE)


fviz_pca_ind(result.prcomp,
             col.ind = "cos2", 
             repel = TRUE
)


fviz_pca_var(result.prcomp,
             col.var = "contrib",
             repel = TRUE
)

fviz_pca_var(result.prcomp,
             col.var = "cos2",
             repel = TRUE
)


fviz_cos2(result.prcomp, choice = "var", axes = 1:2)
fviz_contrib(result.prcomp, choice = "var", axes = 1:2)
fviz_pca_biplot(result.prcomp, repel = TRUE)

