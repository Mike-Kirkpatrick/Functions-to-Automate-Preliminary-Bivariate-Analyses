


# R Functions to Automate Preliminary Bivariate Analyses

#~~~~~~~~~~~~~~~~~~~~~#
#     IMPORT DATA     #
#~~~~~~~~~~~~~~~~~~~~~#
data(mtcars)
head(mtcars)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#     CREATE ANALYSIS DATASET - MAKE A COUPLE CATEGORICAL VARIABLES     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
df <- mtcars

df$cyl <- as.factor(df$cyl)

df$am <- as.factor(
  sapply(df$am, function(i)
  if (i == 0) {"automatic"}
  else if (i==1) {"manual"}
))

df$vs <- as.factor(
  sapply(df$vs, function(i)
    if (i == 0) {"V-engine"}
    else if (i==1) {"Straigt engine"}
  ))

head(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#     SET DEFAULT PARAMETERS     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
color <- c("coral","aquamarine3")      # Plot colors
yrange <- function(y) c(0,max(y)*1.25) # Function to make Y axis 25% bigger
alpha <- 0.05                          # alpha level for statistical significance
library(car)                           # Need car package to get ANOVA p-values with categorical DVs


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#     CONTINUOUS DV - STATISTICAL TESTS TABLE     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
DV <- "mpg"
IVs <- colnames(df)[colnames(df)!= DV]

f.ConDV <- function(IV) {
  m <- lm (df[,DV] ~ df[,IV])
  a <- anova(m)
  s <- summary(m)
  r <- data.frame(cbind(df1=s$fstatistic[2], df2=s$fstatistic[3], F.stat=round(s$fstatistic[1],2), p.value=round(a$`Pr(>F)`[1],3), r.sqr = round(s$r.squared,2)))
  r <- cbind(DV,IV,r)
  r$Sig <- ifelse(r$p.value < alpha,'Yes','No')
  rownames(r) <- NULL
  return(r)
}

tbl.ConDV <- do.call(rbind,lapply(IVs, f.ConDV))
tbl.ConDV <- tbl.ConDV[order(tbl.ConDV$p.value,-tbl.ConDV$F.stat),]
rownames(tbl.ConDV) <- NULL
print(tbl.ConDV)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#     CONTINUOUS DV - PLOTS     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
DV <- "mpg"
IVs <- as.matrix(subset(tbl.ConDV,Sig=="Yes",select = IV))  # Only statistically significant IVs


f.ConDvPlots <- function(IV) {
  if(class(df[,IV])=="numeric") {
    plot(df[,IV],df[,DV], main=paste("Scatterplot of",DV,"by",IV), xlab=IV, ylab=DV, pch=20)
    abline(lm(df[,DV]~df[,IV]), col="red")
    lines(lowess(df[,IV],df[,DV]), col="blue")
  }
  else if (class(df[,IV])=="factor") {
    boxplot(df[,DV] ~ df[,IV], col = color, main=paste("Boxplot of",DV,"by",IV), ylab=DV, xlab=IV)  # FIX COLORS IF MORE THAN 2 LEVEL
    }
}


lapply(IVs, f.ConDvPlots)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#     CATEGORICAL DV - STATISTICAL TESTS TABLE     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
DV <- "vs"
IVs <- colnames(df)[colnames(df)!= DV]

f.CatDV <- function(IV) {
  m <- glm(df[,DV] ~ df[,IV], family = binomial)
  a <- Anova(m, type="III")
  r <- data.frame(cbind(df=a$Df, ChiSq=round(a$`LR Chisq`,2), p.value=round(a$`Pr(>Chisq)`,3)))
  r <- cbind(DV,IV,r)
  r$Sig <- ifelse(r$p.value < alpha,'Yes','No')
  rownames(r) <- NULL
  return(r)
}

tbl.CatDV <- do.call(rbind,lapply(IVs, f.CatDV))
tbl.CatDV <- tbl.CatDV[order(tbl.CatDV$p.value,-tbl.CatDV$ChiSq),]
rownames(tbl.CatDV) <- NULL
print(tbl.CatDV)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#     CATEGORICAL DV - PLOTS     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
DV <- "vs"
IVs <- as.matrix(subset(tbl.CatDV,Sig=="Yes",select = IV))  # Only stat significant IVs



f.CatDvPlots <- function(IV) {
  if(class(df[,IV])=="numeric") {
    boxplot(df[,IV] ~ df[,DV], col = color, main=paste("Boxplot of",DV,"by",IV), ylab=IV, xlab=DV)
  }
  else if (class(df[,IV])=="factor") {
    t <- table(df[,DV],df[,IV])
    p <- prop.table(t,2) #if want a propotion table
    par(mfrow=c(1,2)) 
    barplot(t, beside=T, col=color, ylim = yrange(t), main=paste("Barplots for",DV,"by",IV), ylab="Frequency", xlab=IV)
    legend("topleft", levels(df[,DV]), pch=15, col=color, bty="n")
    barplot(p, col=color, ylab = "Percent of Total", xlab=IV)
    par(mfrow=c(1,1))
  }
  
}


lapply(IVs, f.CatDvPlots)



