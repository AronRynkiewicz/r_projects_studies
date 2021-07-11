library("Hmisc")
library("dplyr")
library("ggpubr")
library("car")
library("dunn.test")
library("FSA")

df <- read.csv2("http://www.cs.put.poznan.pl/kgutowska/PSwBB/dane/przykladoweDane-Projekt.csv", sep = ";")

summary(df)
table(df$plec)
table(df$grupa)

sum(df$hsCRP > 10.0)
sum(df$HCT < 0.18)
sum(na.omit(df$MON) > 2.0)
sum(na.omit(df$ERY) > 6.0)

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

boxplot(df$HGB, main="Wykres dla kolumny HGB", ylab="Rozkład wartości w kolumnie HGB [g/dl]", xlab="HGB")
boxplot(df$ERY, main="Wykres dla kolumny ERY", ylab="Rozkład wartości w kolumnie ERY [mln/mm3]", xlab="ERY")
boxplot(df$HCT, main="Wykres dla kolumny HCT", ylab="Rozkład wartości w kolumnie HCT [ml/ml]", xlab="HCT")
boxplot(df$MON, main="Wykres dla kolumny MON", ylab="Rozkład wartości w kolumnie MON [10^9/l]", xlab="MON")


for(i in colnames(df)){print(var(df[[i]]))}
for(i in colnames(df)){print(sd(df[[i]]))}


for (i in c("KONTROLA", "CHOR1", "CHOR2")){
    for(j in c("hsCRP", "ERY", "PLT", "HGB", "HCT", "MCHC", "MON", "LEU")){
        print(i)
        print(j);
        print(shapiro.test(with(df, df[grupa == i, ])[[j]])$p.value);
    }
}


ggdensity(df, x="hsCRP",
          color = "grupa",
          fill = "grupa",
          palette = c("#99cc00", "#660099", "#0047b3"),
          ylab = "Gęstość",
          xlab = "hsCRP [mg/l]"
          ) + facet_wrap(~ grupa, scales="free")

ggdensity(df, x="ERY",
          color = "grupa",
          fill = "grupa",
          palette = c("#99cc00", "#660099", "#0047b3"),
          ylab = "Gęstość",
          xlab = "ERY [mln/mm3]"
          ) + facet_wrap(~ grupa, scales="free")

ggdensity(df, x="PLT",
          color = "grupa",
          fill = "grupa",
          palette = c("#99cc00", "#660099", "#0047b3"),
          ylab = "Gęstość",
          xlab = "PLT [tys/mm3]"
) + facet_wrap(~ grupa, scales="free")

ggdensity(df, x="HGB",
          color = "grupa",
          fill = "grupa",
          palette = c("#99cc00", "#660099", "#0047b3"),
          ylab = "Gęstość",
          xlab = "HGB [g/dl]"
) + facet_wrap(~ grupa, scales="free")

ggdensity(df, x="MON",
          color = "grupa",
          fill = "grupa",
          palette = c("#99cc00", "#660099", "#0047b3"),
          ylab = "Gęstość",
          xlab = "MON [10^9/l]"
) + facet_wrap(~ grupa, scales="free")


leveneTest(HCT ~ grupa, data = df)$"Pr(>F)"[1]
leveneTest(MCHC ~ grupa, data = df)$"Pr(>F)"[1]
leveneTest(LEU ~ grupa, data = df)$"Pr(>F)"[1]

summary(aov(HCT ~ grupa, data = df))
summary(aov(MCHC ~ grupa, data = df))
summary(aov(LEU ~ grupa, data = df))

TukeyHSD(aov(HCT ~ grupa , data = df))
TukeyHSD(aov(MCHC ~ grupa , data = df))
plot(TukeyHSD(aov(HCT ~ grupa , data = df)))
plot(TukeyHSD(aov(MCHC ~ grupa , data = df)))


kruskal.test(hsCRP ~ grupa , data = df)$p.value
kruskal.test(ERY ~ grupa , data = df)$p.value
kruskal.test(PLT ~ grupa , data = df)$p.value
kruskal.test(HGB ~ grupa , data = df)$p.value
kruskal.test(MON ~ grupa , data = df)$p.value
dunnTest(df$HGB, df$grupa)

for (i in c("CHOR1", "CHOR2", "KONTROLA")){
    for(j in c("HCT", "MCHC", "LEU")){
        for(k in c("HCT", "MCHC", "LEU")){
            if(j==k){
                next;
            }
            tmp <- df[grupa==i, ];
            test <- cor.test(tmp[[j]], tmp[[k]], method="pearson");
            print(c(i, j, k));
            print(test$estimate);
            print(test$p.value);
        }
    }
}


for (i in c("CHOR1", "CHOR2", "KONTROLA")){
    for(j in c("hsCRP", "ERY", "PLT", "HGB", "MON")){
        for(k in c("hsCRP", "ERY", "PLT", "HGB", "MON")){
            if(j==k){
                next;
            }
            tmp <- df[grupa==i, ];
            test <- cor.test(tmp[[j]], tmp[[k]], method="spearman");
            print(c(i, j, k));
            print(test$estimate);
            print(test$p.value);
        }
    }
}

ggscatter(df[grupa=="CHOR1", ], x = "HCT", y = "MCHC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          color = "grupa", fill = "grupa",
          ylab = "[g/dl]", xlab = "[ml/ml]")


ggscatter(df[grupa=="CHOR2", ], x = "ERY", y = "HGB",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          color = "grupa", fill = "grupa",
          ylab = "[g/dl]", xlab = "[mln/mm3]")


ggscatter(df[grupa=="KONTROLA", ], x = "ERY", y = "HGB",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          color = "grupa", fill = "grupa",
          ylab = "[g/dl]", xlab = "[mln/mm3]")


ggscatter(df[grupa=="KONTROLA", ], x = "ERY", y = "PLT",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          color = "grupa", fill = "grupa",
          ylab = "[tys/mm3]", xlab = "[mln/mm3]")
