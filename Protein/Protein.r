#Задание 3. Средствами пакета  tidyverse(преимущественно) отобразите зависимость относительного уровня белка в опухоли(файл с пометкой proteome) от относительного уровня транскрипта (файл с пометкой transcriptome) для генов AKT1, PTEN, TP53, на одном рисунке (geom_point). Каждому гену должен соответствовать свой цвет и форма символа. Обязательно использование одной из функций для 2 таблиц(left_join, right_join, inner_join, full_join)


df_prot <- read.csv("luad_fc_proteome_500g.csv")
df_trans <- read.csv("luad_fc_transcriptome_500g.csv")

library(dplyr)
library(patchwork) 

df_corr <- read.delim("hgnc_complete_set_17_for_students.txt",sep = '\t', )
df_corr_ <- df_corr[c('symbol','ensembl_gene_id')]
colnames(df_corr_)[1] = 'gene'
#df_corr_

dim(df_prot)

df_merged = df_prot %>% inner_join(df_corr_,by = 'gene') %>% inner_join(df_trans, by = join_by(ensembl_gene_id == gene))
#df_merged

subset(df_merged,df_merged$gene %in% c('TP53','AKT1','PTEN'))

library(tidyverse)
prot_long <- df_prot %>%
  pivot_longer(cols = -c(gene,X),
               names_to = "patient",
               values_to = "protein_level") %>%
  filter(gene %in% c("AKT1", "PTEN", "TP53"))

trans_long <- df_trans %>%
  pivot_longer(cols = -c(gene,X),
               names_to = "patient",
               values_to = "transcript_level")


combined_data <- prot_long %>%
  left_join(df_corr_, by = "gene") %>%
  left_join(trans_long,
            by = c("ensembl_gene_id" = "gene", "patient"),
            suffix = c("_protein", "_transcript"))

p <- ggplot(combined_data,aes(x = transcript_level, y = protein_level, color = gene, shape = gene))+
  geom_point(alpha=.5)+
  labs(x = "Уровень транскриптома",y = 'Уровень белка')+
  theme_classic()

combined_plot <- p + plot_layout(nrow = 1)

ggsave("Protein.png", combined_plot, width = 10, height = 10, dpi = 150,limitsize = FALSE)
system("xdg-open Protein.png")
