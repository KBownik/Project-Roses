library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(RColorBrewer)
library(stringi)


roses <- read.csv("roses_1.csv")
roses <- roses[, c(24:98, 222)]

roses <- roses %>% 
  filter(!is.na(zawody))

zawody <- unlist(stri_split_fixed(roses$zawody, ", "))
ile <- stri_count_fixed(roses$zawody, ",") + 1
Q5_1 <- rep(roses$Q5_1, ile)
tmp <- data.frame(Q5_1)

for(i in 2:(ncol(roses) - 1)) {
  name <- colnames(roses)[i]
  a <- roses[, i]
  a <- rep(a, ile)
  tmp <- data.frame(tmp, a)
  colnames(tmp)[i] <- name
}

roses <- data.frame(tmp, zawody)


roses_mys <- roses[, c("Q7_3", "Q7_4", "Q7_5", "Q7_6", "Q7_9", "Q9_31", "zawody")]

roses_mys$mean <- rowMeans(roses_mys[, 1:6], na.rm = TRUE)

roses_mys <- roses_mys %>% 
  filter(!is.nan(mean) & zawody != "nie wiem") %>% 
  dplyr::select(mean, zawody) %>% 
  filter(mean >= 3.3) %>% 
  count(zawody) %>% 
  arrange(-n) %>% 
  rename(n_mys = n)

roses_env <- roses[, c("Q21_1", "Q7_10", "Q7_11", "Q9_2", "Q9_3",
                       "Q9_4", "Q9_11", "Q9_14", "Q9_15", "Q9_16", "zawody")]

roses_env$mean <- rowMeans(roses_env[, 1:10], na.rm = TRUE)

roses_env <- roses_env %>% 
  filter(!is.nan(mean) & zawody != "nie wiem") %>% 
  dplyr::select(mean, zawody) %>% 
  filter(mean >= 3.3) %>% 
  count(zawody) %>% 
  arrange(-n) %>% 
  rename(n_env = n)

roses_cos <- roses[, c("Q5_12", "Q5_13", "Q21_8", "Q21_16",
                       "Q7_2", "Q7_7", "Q9_22", "zawody")]

roses_cos$mean <- rowMeans(roses_cos[, 1:7], na.rm = TRUE)

roses_cos <- roses_cos %>% 
  filter(!is.nan(mean) & zawody != "nie wiem") %>% 
  dplyr::select(mean, zawody) %>% 
  filter(mean >= 3.3) %>% 
  count(zawody) %>% 
  arrange(-n) %>% 
  rename(n_cos = n)

roses_chem <- roses[, c("Q5_1", "Q5_8", "Q21_5", "Q21_6",
                        "Q21_17", "Q21_18", "zawody")]

roses_chem$mean <- rowMeans(roses_chem[, 1:6], na.rm = TRUE)

roses_chem <- roses_chem %>% 
  filter(!is.nan(mean) & zawody != "nie wiem") %>% 
  dplyr::select(mean, zawody) %>% 
  filter(mean >= 3.3) %>% 
  count(zawody) %>% 
  arrange(-n) %>% 
  rename(n_chem = n)


roses_ani <- roses[, c("Q5_9", "Q21_3", "Q21_4", "Q9_11",
                       "Q9_13", "Q9_19", "Q9_20", "zawody")]

roses_ani$mean <- rowMeans(roses_ani[, 1:7], na.rm = TRUE)

roses_ani <- roses_ani %>% 
  filter(!is.nan(mean) & zawody != "nie wiem") %>% 
  dplyr::select(mean, zawody) %>% 
  filter(mean >= 3.3) %>% 
  count(zawody) %>% 
  arrange(-n) %>% 
  rename(n_ani = n)

top_zawody <- unique(c(roses_cos$zawody[1:5], roses_env$zawody[1:5], roses_mys$zawody[1:5], roses_ani$zawody[1:5], roses_chem$zawody[1:5]))

tabelka <- full_join(roses_cos, roses_mys)

tabelka <- tabelka %>% 
  full_join(roses_env) %>% 
  full_join(roses_ani) %>% 
  full_join(roses_chem) %>% 
  filter(zawody %in% top_zawody) %>% 
  pivot_longer(!zawody, names_to = "zainteresowania", values_to = "iloœæ")


ggplot(tabelka, aes(y = zawody, x = zainteresowania, fill = iloœæ)) +
  geom_tile() +
  scale_fill_gradient(high = "#22044a", low = "#e5ddf0")
