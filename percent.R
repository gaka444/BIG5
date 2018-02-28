#Positive Percentage

#Renaming
extraSc=table_final$Extraversion
agreeSc=table_final$Agreeableness
openSc=table_final$Openness
consSc=table_final$Conscientiousness
neuroSc=table_final$Neuroticism


#Adding column
table_final$ExtPercent = extraSc/ (extraSc+agreeSc+openSc+consSc+neuroSc) *100

#Replacing Nan with zero
ee= table_final$ExtPercent
ee[is.nan(ee)] <- 0
table_final$ExtPercent = ee



#Adding column
table_final$AgrPercent = agreeSc/ (extraSc+agreeSc+openSc+consSc+neuroSc) *100

#Replacing Nan with zero
aa = table_final$AgrPercent
aa[is.nan(aa)] <- 0
table_final$AgrPercent = aa

#Adding column
table_final$OpnPercent = openSc/ (extraSc+agreeSc+openSc+consSc+neuroSc) *100

#Replacing Nan with zero
oo = table_final$OpnPercent
oo[is.nan(oo)] <- 0
table_final$OpnPercent = oo

#Adding column
table_final$ConPercent = consSc/ (agreeSc+extraSc+consSc+openSc+neuroSc) *100

#Replacing Nan with zero
cc = table_final$ConPercent
cc[is.nan(cc)] <- 0
table_final$ConPercent = cc

#Adding column
table_final$NuroPercent = neuroSc/ (agreeSc+extraSc+openSc+consSc+neuroSc) *100

#Replacing Nan with zero
nn = table_final$NuroPercent
nn[is.nan(nn)] <- 0
table_final$NuroPercent = nn