library(readr)

blood <- read_csv(file = "blood_transfusion.csv")

df <- blood

df[100, 'Monetary']

tail(df, 10)

above_avg <- df[['Monetary']] > mean(df[['Monetary']])
df[above_avg, 'Monetary']


cincy <- read_csv(file = "PDI__Police_Data_Initiative__Crime_Incidents.csv")

df <- cincy

colSums(is.na(df))

range(df[['DATE_REPORTED']])

table(df[['SUSPECT_AGE']])

table(df[['DAYOFWEEK']])
