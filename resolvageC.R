
library(readr)
path<-"C:/Users/Masspeclab/Desktop/Suivi_EIs_Tenax_GCxGC"
path<-"C:/Users/Masspeclab/Desktop/Seq_plasma_RECORDS-Interv/"
files <- list.files(path, pattern = ".csv", full.names = TRUE)


f<-files[1]


df <- read_csv2(f, show_col_types = FALSE) #lecture avec nimporte quel separateur sauf ","
name<-names(df)
# changer le nom des mollecule avec vurgule
df<-apply(df,1,function(x) stringi::stri_replace_last_fixed(str=x,pattern = "Decanoic acid, methyl ester C10",
                                                   replacement = "Decanoic acid methyl ester C10"))

#.....

#separation des colonnes avec separateur ","
separation<-sapply(df,function(x) strsplit(x = x,split = ",",fixed = TRUE)[[1]])

#test si il y a bien le meme nombre de colonne dans chaque ligne
df<-as.data.frame(Reduce(rbind,separation))
#ajouter nom de colonne