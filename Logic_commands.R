#### Delete row conditionally ####

df <- df[!df$Colx == "is not" | df$Coly == "is also not",]
# The _ ! _ basically specifies "if is not... then..."
# -> If in Colx it says 'is not' OR in Coly it says 'is also not' then omit the row, 
# or in other words, dataframe df is defined as dataframe df without the specified arguments.
# [row,column]
#
df <- df[!df$Colx == "is not" & df$Coly == "is also not",]

data()

which(df$Colx != "is not")


a <- c(0,1,2,1,2,1,2,3,2,3,0,1)
which(a != 0 & a == 2)

if( )
