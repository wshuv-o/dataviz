library("dplyr")

originalDataFrame<-read.csv("C://Users//Shuvo//Downloads//hepatitis//hepatitis_data.csv")

originalDataFrame
print(str(originalDataFrame))
colSums(is.na(originalDataFrame))


updated1DataFrame<-originalDataFrame
updated1DataFrame

missing_counts<-sapply(updated1DataFrame, function(Age) sum(is.na(Age)))
barplot(missing_counts, names.arg=colnames(updated1DataFrame), main= "Missing Value Counts", ylab="Number of Missing Values")


cols_categorical<-c("Sex", "Steroid", "Antivirals", "Fatigue", "Malaise", "Anorexia", "Liver.Big", "Liver.Firm", "Spleen.Palpable", "Spiders", "Ascites", "Varices")

for (col in cols_categorical) {
  mode_val<-as.numeric(names(which.max(table(originalDataFrame[[col]]))))
  originalDataFrame[[col]][is.na(originalDataFrame[[col]])]<-mode_val
}

colSums(is.na(originalDataFrame))



mean_bilirubin<-mean(originalDataFrame$Bilirubin, na.rm=TRUE)
originalDataFrame$Bilirubin[is.na(originalDataFrame$Bilirubin)]<-mean_bilirubin
colSums(is.na(originalDataFrame))


cols_to_impute<-c("Alk.Phosphate", "Sgot", "Albumin", "Protime")

for (col in cols_to_impute) {
  mean_val<-mean(originalDataFrame[[col]], na.rm=TRUE)
  originalDataFrame[[col]][is.na(originalDataFrame[[col]])]<-mean_val
}

colSums(is.na(originalDataFrame))



anova_result<-aov(originalDataFrame$Age ~ originalDataFrame$Class, data= originalDataFrame)
print(summary(anova_result))

kendall_result<-cor(originalDataFrame$Class, originalDataFrame$Age, method= "kendall")
print(kendall_result)




chi_square_results<-lapply(cols_categorical, function(col) {
  contingency_table<-table(originalDataFrame[[col]], originalDataFrame$Class)
  chi_square_test<-chisq.test(contingency_table)
  
  chi_square_statistic<-chi_square_test$statistic
  p_value<-chi_square_test$p.value
  
  result<-list(Attribute=col,
                 ChiSquare=chi_square_statistic,
                 P_Value=p_value)
  return(result)
})

chi_square_results_df<-do.call(rbind, chi_square_results)
print(chi_square_results_df)


anova_result_Bilirubin<-aov(originalDataFrame$Bilirubin ~originalDataFrame$Class, data =originalDataFrame)
print(summary(anova_result_Bilirubin))
kendall_result_Bilirubin<-cor(originalDataFrame$Class, originalDataFrame$Bilirubin, method= "kendall")
print(kendall_result_Bilirubin)


anova_result_AlkPhosphate<-aov(originalDataFrame$Alk.Phosphate ~ originalDataFrame$Class, data=originalDataFrame)
print(summary(anova_result_AlkPhosphate))
kendall_result_AlkPhosphate<-cor(originalDataFrame$Class, originalDataFrame$Alk.Phosphate, method= "kendall")
print(kendall_result_AlkPhosphate)

anova_result_Sgot<-aov(originalDataFrame$Sgot ~ originalDataFrame$Class, data= originalDataFrame)
print(summary(anova_result_Sgot))
kendall_result_Sgot<-cor(originalDataFrame$Class, originalDataFrame$Sgot, method="kendall")
print(kendall_result_Sgot)

anova_result_Albumin<-aov(originalDataFrame$Albumin ~ originalDataFrame$Class, data=originalDataFrame)
print(summary(anova_result_Albumin))
kendall_result_Albumin<-cor(originalDataFrame$Class, originalDataFrame$Albumin, method= "kendall")
print(kendall_result_Albumin)

anova_result_Protime<-aov(originalDataFrame$Protime ~ originalDataFrame$Class, data= originalDataFrame)
print(summary(anova_result_Protime))
kendall_result_Protime<-cor(originalDataFrame$Class, originalDataFrame$Protime, method="kendall")
print(kendall_result_Protime)


pearson_result=cor(originalDataFrame,method="pearson")
pearson_result

spearman_result=cor(originalDataFrame,method="spearman")
spearman_result
