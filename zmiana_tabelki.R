library(haven)
library(data.table)

load_savfile <- read_spss("roses.sav")
saveascsv <- write.csv(load_savfile, file = "roses.csv", row.names = FALSE)
load_csvfile <-read.csv("roses.csv")
roses <- as.data.frame((load_csvfile))

library(dplyr)
library(stringi)

roses1 <- roses %>%
  select(Q18_1)

roses1 <- as.data.frame(lapply(roses1, tolower))


zawody1 <- c("informatyk", "lekarz", "psycholog", "weterynarz", "architek", "mechanik", "programist", "pi³karz",
             "naukow", "prawnik", "fotograf", "kuchar", "grafik", "t³umacz", "elektryk", "nauczyciel", "kosmety", "projektant",
             "stra¿ak", "stewardess", "aktor", "dietety", "polic", "fryzjer", "kierowc", "¿o³nie", "rolnik",
             "sportow", "piosenka", "cukiernik", "chirurg", "stolar", "muzyk", "sprzedawca", "dziennika", "pisar", "biolog", 
             "makija¿yst", "tance", "archeolog", "fizjoterapeut", "astronaut", "prezydent", "trener", "opiekun", "wiza¿", "szef",
             "farmaceut", "przewodnik", "ratownik", "chemik", "pilot", "kosmetolog", "in¿ynier", "artyst", "astrofizyk",
             "detektyw", "okulista", "bokser", "szachis", "polityk", "kardiolog", "logistyk", "tapicer", "mechatronik",
             "przedszkola", "stomatolog", "dentyst", "ogrodni", "murar", "œlusar", "bank")

zawody2 <- c("informatyk", "lekarz", "psycholog", "weterynarz", "architekt", "mechanik", "programista", "pi³karz",
             "naukowiec", "prawnik", "fotograf", "kucharz", "grafik", "t³umacz", "elektryk", "nauczyciel", "makija¿ysta", "projektant",
             "stra¿ak", "stewardessa", "aktor", "dietetyk", "policjant", "fryzjer", "kierowca", "¿o³nierz", "rolnik",
             "sportowiec", "piosenkarz", "cukiernik", "chirurg", "stolarz", "muzyk", "sprzedawca", "dziennikarz", "pisarz", "biolog", 
             "makija¿ysta", "tancerz", "archeolog", "fizjoterapeuta", "astronauta", "prezydent", "trener", "opiekun", "wiza¿ysta", "szef",
             "farmaceuta", "przewodnik", "ratownik", "chemik", "pilot", "kosmetolog", "in¿ynier", "artysta", "astrofizyk", 
             "detektyw", "okulista", "bokser", "szachista", "polityk", "kardiolog", "logistyk", "tapicer", "mechatronik",
             "przedszkolanka", "dentysta", "dentysta", "ogrodnik", "murarz", "œlusarz", "bankier")

zawody <- rep(NA, length(roses1$Q18_1))

for (i in 1:length(zawody1)){
  for (j in which(!is.na(zawody) & stri_detect_fixed(roses1$Q18_1, zawody1[i]))){
    zawody[j] <- paste(zawody[j], zawody2[i], sep = ", ")
  }
  
  zawody[stri_detect_fixed(roses1$Q18_1, zawody1[i]) & is.na(zawody)] <- zawody2[i]
}

roses$zawody <- zawody

#uzupe³niam rêcznie
roses$zawody[roses$Q18_1 == " Fryzierka"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "Copywriterem"] <- "copywriter"
roses$zawody[roses$Q18_1 == "zatrudniona w zawodzie, gdzie potrzebna jest umiejêtnoœæ rysowania na ró¿nych urz¹dzeniach i normalnie na p³ótnie, kartkach"] <- "artysta"
roses$zawody[roses$Q18_1 == "coœ zwi¹zane z koñmi"] <- "koniara"
roses$zawody[roses$Q18_1 == "chcia³abym otworzyæ w³asny sklep, bzines"] <- "szef, sprzedawca"
roses$zawody[roses$Q18_1 == "chcialbym za³orzyc firme paleciarsk¹ albo ogulnobudowlan¹"] <- "szef, budowlaniec"
roses$zawody[roses$Q18_1 == "Budowlanka"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "Konstruktor"] <- "budowlaniec, in¿ynier"
roses$zawody[roses$Q18_1 == "Pracowaæ w biurze"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "pawel jumper"] <- "pawe³ jumper"
roses$zawody[roses$Q18_1 == "Infortmatykiem"] <- "informatyk"
roses$zawody[roses$Q18_1 == "Badacz kosmosu"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "Chcia³bym byæ niani¹"] <- "opiekun"
roses$zawody[roses$Q18_1 == "Technik Pojazdów Samochodowych"] <- "mechanik"
roses$zawody[roses$Q18_1 == "osob¹ zajmuj¹c¹ siê komputerami"] <- "informatyk"
roses$zawody[roses$Q18_1 == "Krawcow¹"] <- "rzemieœlnik"
roses$zawody[roses$Q18_1 == "biznesmen"] <- "szef"
roses$zawody[roses$Q18_1 == "Ja chcia³bym byæ zawodnikiem zjazdowym."] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Patomorfolog/plastyk"] <- "lekarz, artysta"
roses$zawody[roses$Q18_1 == "Robotnika"] <- "robotnik"
roses$zawody[roses$Q18_1 == "Laborantem"] <- "chemik"
roses$zawody[roses$Q18_1 == "Chcia³bym Mieæ firmê Budowlan¹"] <- "szef, budowlaniec"
roses$zawody[roses$Q18_1 == "je¿dziæ zawodowo na eowerze"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "operatorem koparko-³adowarki"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "trendwatcher"] <- "socjolog"
roses$zawody[roses$Q18_1 == "Menagerem lub kierownikem jakiejœ firmy"] <- "szef"
roses$zawody[roses$Q18_1 == "coœ zwi¹zane z biurem"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "odkrywca technologii"] <- "in¿ynier"
roses$zawody[roses$Q18_1 == "sprzedawc¹ ubrañ"] <- "sprzedawca"
roses$zawody[roses$Q18_1 == "ginekolog"] <- "lekarz"
roses$zawody[roses$Q18_1 == "podologiem"] <- "lekarz"
roses$zawody[roses$Q18_1 == "technikiem"] <- "in¿ynier"
roses$zawody[roses$Q18_1 == "kelnerem/sprzedawc¹"] <- "kelner, sprzedawca"
roses$zawody[roses$Q18_1 == "spedytorem"] <- "logistyk"
roses$zawody[roses$Q18_1 == "dekoratorem wnêtrz"] <- "artysta"
roses$zawody[roses$Q18_1 == "florystka"] <- "biolog"
roses$zawody[roses$Q18_1 == "Badaczem bananów"] <- "biolog"
roses$zawody[roses$Q18_1 == "Chcia³abym mieæ pracê zwi¹zan¹ z jêzykami obcymi."] <- "t³umacz"
roses$zawody[roses$Q18_1 == "Poicjantka"] <- "policjant"
roses$zawody[roses$Q18_1 == "popularnym radc¹ prawnym"] <- "prawnik"
roses$zawody[roses$Q18_1 == "Weterynaria"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "Rolnictwo"] <- "rolnik"
roses$zawody[roses$Q18_1 == "Mój zawód musia³by byæ powi¹zany z matematyk¹ i rysowaniem."] <- "architekt"
roses$zawody[roses$Q18_1 == "Maryna¿em"] <- "marynarz"
roses$zawody[roses$Q18_1 == "prezesem firmy"] <- "szef"
roses$zawody[roses$Q18_1 == "pracowaæ na maszynach numerycznych"] <- "informatyk"
roses$zawody[roses$Q18_1 == "okulist¹"] <- "lekarz"
roses$zawody[roses$Q18_1 == "bizneswoman"] <- "szef"
roses$zawody[roses$Q18_1 == "pracowaæ w jakiœ wiêkszych korporacjach"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "osob¹ która jest w stycznoœci z natur¹"] <- "biolog"
roses$zawody[roses$Q18_1 == "weteryniarzem"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "strazak"] <- "stra¿ak"
roses$zawody[roses$Q18_1 == "poliglot¹"] <- "t³umacz"
roses$zawody[roses$Q18_1 == "jeszcze do koñca nie wiem kim ale coœ zwi¹zanego ze sportem"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "animatorem"] <- "animator"
roses$zawody[roses$Q18_1 == "Instruktor jazdy konnej"] <- "koniara"
roses$zawody[roses$Q18_1 == "Groomer"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "tlumacz"] <- "t³umacz"
roses$zawody[roses$Q18_1 == "Sterownikiem robotów"] <- "in¿ynier"
roses$zawody[roses$Q18_1 == "Zawodniczk¹ siatkówki i zajmowaæ siê dzietetyk¹ lub pracowaæ na wydziale kryminologicznym"] <- "spotowiec, dietetyk, detektyw"
roses$zawody[roses$Q18_1 == "w³aœciciel firmy g³ównie z samochodami"] <- "szef"
roses$zawody[roses$Q18_1 == "œledczym"] <- "detektyw"
roses$zawody[roses$Q18_1 == "Fryzierem"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "Rzemieœlnik"] <- "rzemieœlnik"
roses$zawody[roses$Q18_1 == "Technik logostyk"] <- "logistyk"
roses$zawody[roses$Q18_1 == "kafelkarz"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "Perkusista metalowy"] <- "muzyk"
roses$zawody[roses$Q18_1 == "marynarzem"] <- "marynarz"
roses$zawody[roses$Q18_1 == "E-sport"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "od dzieciñstwa marzê o staniu na scenie"] <- "aktor"
roses$zawody[roses$Q18_1 == "chcia³abym wykonywaæ zawód przeczkolankê"] <- "przedszkolanka"
roses$zawody[roses$Q18_1 == "byæ austrona³t¹"] <- "astronauta"
roses$zawody[roses$Q18_1 == "Sêdzia pi³karski"] <- "sêdzia"
roses$zawody[roses$Q18_1 == "psychiatra/patolog s¹dowy/prokurator"] <- "psycholog, prawnik"
roses$zawody[roses$Q18_1 == "Tokarz"] <- "rzemieœlnik"
roses$zawody[roses$Q18_1 == "genetykiem"] <- "biolog"
roses$zawody[roses$Q18_1 == "fryzjrk¹"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "architrkt"] <- "architekt"
roses$zawody[roses$Q18_1 == "Pilkarz"] <- "pi³karz"
roses$zawody[roses$Q18_1 == "asystentk¹ w biurze"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "Weterzynarzem"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "Pracownikiem w biurze"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "Leœniczym"] <- "leœniczy"
roses$zawody[roses$Q18_1 == "Wychowawc¹ grupy przedszkolnej"] <- "przedszkolanka"
roses$zawody[roses$Q18_1 == "Raperem"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "cz³onkiem astrofizycznej grupy badawczej lub w jakiœ inny sposób byæ zwi¹zan¹ z badaniem wrzechœwiata"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "miechanik samochodowy"] <- "mechanik"
roses$zawody[roses$Q18_1 == "Pracowaæ w hotelu"] <- "hotelarz"
roses$zawody[roses$Q18_1 == "Wuefista"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "Ochroniarzem SOP"] <- "policjant"
roses$zawody[roses$Q18_1 == "Instruktork¹ tañca lub po prostu tañcerk¹"] <- "tancerz"
roses$zawody[roses$Q18_1 == "Rolnictwo/Górnik"] <- "rolnik, górnik"
roses$zawody[roses$Q18_1 == "Budowlaniec"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "Raper i tyke"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "zoolog"] <- "biolog"
roses$zawody[roses$Q18_1 == "Malarka"] <- "malarz"
roses$zawody[roses$Q18_1 == "zwi¹zany z technologi¹ lub nauk¹"] <- "in¿ynier"
roses$zawody[roses$Q18_1 == "Automatyk"] <- "in¿ynier"
roses$zawody[roses$Q18_1 == "Chcia³abym mieæ swoj¹ firmê lub byæ jakimœ managerem dobrej firmy/hotelu"] <- "szef, hotelarz"
roses$zawody[roses$Q18_1 == "Deweloperem"] <- "deweloper"
roses$zawody[roses$Q18_1 == "ekonomista"] <- "ekonomista"
roses$zawody[roses$Q18_1 == "Cyberbezpieczeñstwo"] <- "programista"
roses$zawody[roses$Q18_1 == "leka¿em"] <- "lekarz"
roses$zawody[roses$Q18_1 == "Zostaæ w³aœcicielem firmy wykorzystuj¹cej nowe technologie."] <- "szef"
roses$zawody[roses$Q18_1 == "fizyk j¹drowy/subatomowy"] <- "fizyk"
roses$zawody[roses$Q18_1 == "Instruktorka jazdy konnej"] <- "koniara"
roses$zawody[roses$Q18_1 == "W³aœciciel du¿ej firmy zajmuj¹cej siê e-commerce"] <- "szef"
roses$zawody[roses$Q18_1 == "Ekonomik/ksiêgowa"] <- "ekonomista, ksiêgowy"
roses$zawody[roses$Q18_1 == "byæ Rybakiem"] <- "rybak"
roses$zawody[roses$Q18_1 == "Fryzier"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "geodetom"] <- "geodeta"
roses$zawody[roses$Q18_1 == "Hydraulika"] <- "hydraulik"
roses$zawody[roses$Q18_1 == "Grabiarzem"] <- "grabarz"
roses$zawody[roses$Q18_1 == "W przysz³oœci chcia³abym zostaæ dermatologiem."] <- "lekarz"
roses$zawody[roses$Q18_1 == "Lektorka"] <- "lektor"
roses$zawody[roses$Q18_1 == "Masa¿"] <- "masa¿ysta"
roses$zawody[roses$Q18_1 == "wojskowy"] <- "¿o³nierz"
roses$zawody[roses$Q18_1 == "Psi Behawiorysta"] <- "treser"
roses$zawody[roses$Q18_1 == "weteryna¿em"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "rysowaæ komiksy"] <- "grafik"
roses$zawody[roses$Q18_1 == "malarz"] <- "malarz"
roses$zawody[roses$Q18_1 == "medycyna estetyczna"] <- "lekarz, makija¿ysta"
roses$zawody[roses$Q18_1 == "Nie wiem kim dok³adnie, ale kimœ zwi¹zanym z histori¹"] <- "historyk"
roses$zawody[roses$Q18_1 == "Pracowaæ w hodowli zwierz¹t np.egzotycznych"] <- "biolog"
roses$zawody[roses$Q18_1 == "dropshipping"] <- "logistyk"
roses$zawody[roses$Q18_1 == "prowadziæ agroturystykê"] <- "przewodnik, hotelarz"
roses$zawody[roses$Q18_1 == "Antyterroryst¹"] <- "policjant"
roses$zawody[roses$Q18_1 == "kierownik budowy"] <- "szef, budowlaniec"
roses$zawody[roses$Q18_1 == "prawnkiem"] <- "prawnik"
roses$zawody[roses$Q18_1 == "graæ w najlepszym klubie w polsce jakim jest stal mielec"] <- "pi³karz"
roses$zawody[roses$Q18_1 == "coœ zwi¹zanego z chemi¹"] <- "chemik"
roses$zawody[roses$Q18_1 == "Animatorka"] <- "animator"
roses$zawody[roses$Q18_1 == "coœ z gastronomi¹"] <- "kucharz"
roses$zawody[roses$Q18_1 == "mlotocyklist¹"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Kimœ kto jest zwi¹zany z jêzykiem angielskim"] <- "t³umacz"
roses$zawody[roses$Q18_1 == "Anglistk¹"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "Raper i producent muzyczny"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "Ortodontk¹"] <- "dentysta"
roses$zawody[roses$Q18_1 == "Stra¿ po¿arna"] <- "stra¿ak"
roses$zawody[roses$Q18_1 == "stra¿nik leœny"] <- "leœniczy"
roses$zawody[roses$Q18_1 == "wynalasca"] <- "in¿ynier"
roses$zawody[roses$Q18_1 == "Okulist¹"] <- "lekarz"
roses$zawody[roses$Q18_1 == "Pracownikiem firmy"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "komornik"] <- "komornik"
roses$zawody[roses$Q18_1 == "pracownikiem salonu Audi lub innej marki samochodów która mi siê podoba"] <- "sprzedawca"
roses$zawody[roses$Q18_1 == "Kryminologiem i kryminalistykiem"] <- "detektyw"
roses$zawody[roses$Q18_1 == "Koszykarzem"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Komik"] <- "komik"
roses$zawody[roses$Q18_1 == "Kimœ kto ma coœ wspólnego ze sportem"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Recepcjonistk¹"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "ekspedientka"] <- "sprzedawca"
roses$zawody[roses$Q18_1 == "Gastronomem"] <- "kucharz"
roses$zawody[roses$Q18_1 == "Kontrolerem ruchu lotniczego"] <- "logistyk"
roses$zawody[roses$Q18_1 == "Medykiem"] <- "lekarz"
roses$zawody[roses$Q18_1 == "Chemiczk¹"] <- "chemik"
roses$zawody[roses$Q18_1 == "Œledczym lub kryminologiem"] <- "detektyw"
roses$zawody[roses$Q18_1 == "diagnost¹ samochodowy,"] <- "mechanik"
roses$zawody[roses$Q18_1 == "elektro monter"] <- "elektryk"
roses$zawody[roses$Q18_1 == "Scenarzystk¹ lub psycholoszk¹"] <- "pisarz, psycholog"
roses$zawody[roses$Q18_1 == "chcia³abym byæ deweloperem"] <- "deweloper"
roses$zawody[roses$Q18_1 == "hydraulik"] <- "hydraulik"
roses$zawody[roses$Q18_1 == "Pi³kark¹"] <- "pi³karz"
roses$zawody[roses$Q18_1 == "saperem lub snajperem"] <- "¿o³nierz"
roses$zawody[roses$Q18_1 == "Adwokat"] <- "prawnik"
roses$zawody[roses$Q18_1 == "dzia³acz na rzecz ochrony  œrodowiska"] <- "ekolog"
roses$zawody[roses$Q18_1 == "Sekretar¹"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "geolog"] <- "geolog"
roses$zawody[roses$Q18_1 == "ksiêgow¹"] <- "ksiêgowy"
roses$zawody[roses$Q18_1 == "testerem gier komputerowych"] <- "tester"
roses$zawody[roses$Q18_1 == "niania"] <- "opiekun"
roses$zawody[roses$Q18_1 == "Pedagaog"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "solista/dyrygent"] <- "muzyk"
roses$zawody[roses$Q18_1 == "badaczem kosmosu"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "strzelcem wyborowym si³ specjalnych"] <- "¿o³nierz"
roses$zawody[roses$Q18_1 == "Oficerem"] <- "¿o³nierz"
roses$zawody[roses$Q18_1 == "Ksiêgow¹"] <- "ksiêgowy"
roses$zawody[roses$Q18_1 == "floryst¹"] <- "biolog"
roses$zawody[roses$Q18_1 == "Polonista/ Humanista"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "hotelark¹"] <- "hotelarz"
roses$zawody[roses$Q18_1 == "Astronem"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "Raperem"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "menad¿er lub w rejestracji"] <- "szef"
roses$zawody[roses$Q18_1 == "Nie wiem jeszcze, ale zastanawiam siê nad prawem."] <- "prawnik"
roses$zawody[roses$Q18_1 == "Specjalista do spraw transportu"] <- "logistyk"
roses$zawody[roses$Q18_1 == "g³ownie militaria konstruktor pojazdów"] <- "in¿ynier"
roses$zawody[roses$Q18_1 == "Mangaka, Animator"] <- "grafik, animator"
roses$zawody[roses$Q18_1 == "kierownikiem apteki"] <- "szef, farmaceuta"
roses$zawody[roses$Q18_1 == "twórc¹ gier komputerowych"] <- "programista"
roses$zawody[roses$Q18_1 == "Tatua¿ysta"] <- "makija¿ysta"
roses$zawody[roses$Q18_1 == "Kosmona³ta"] <- "astronauta"
roses$zawody[roses$Q18_1 == "Chcialabym pracowac w wojsku"] <- "¿o³nierz"
roses$zawody[roses$Q18_1 == "Rysownik/Malarz"] <- "malarz"
roses$zawody[roses$Q18_1 == "wojskowym."] <- "¿o³nierz"
roses$zawody[roses$Q18_1 == "Spawacz"] <- "spawacz"
roses$zawody[roses$Q18_1 == "Najlepiej kimœ po technikum ekonomistycznym"] <- "ekonomista"
roses$zawody[roses$Q18_1 == "Psychiatra"] <- "psycholog"
roses$zawody[roses$Q18_1 == "Biznes woman"] <- "szef"
roses$zawody[roses$Q18_1 == "Infomatykiem"] <- "informatyk"
roses$zawody[roses$Q18_1 == "stra¿nikiem granicznym"] <- "policjant"
roses$zawody[roses$Q18_1 == "Projektowaæ reklamy"] <- "grafik"
roses$zawody[roses$Q18_1 == "Spawaæ"] <- "spawacz"
roses$zawody[roses$Q18_1 == "ksiêgowym"] <- "ksiêgowy"
roses$zawody[roses$Q18_1 == "W korporacji"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "Fizykiem"] <- "fizyk"
roses$zawody[roses$Q18_1 == "astronomem."] <- "astrofizyk"

roses$Q18_1 <- tolower(roses$Q18_1)
roses$zawody[stri_detect_fixed(roses$Q18_1, "nie") & is.na(roses$zawody) & roses$Q18_1 != "uczniem" & roses$Q18_1 != "jebanie disa"] <- "nie wiem"

rameczka <- roses %>% 
  select(Q18_1 | zawody) %>% 
  filter(is.na(zawody) == TRUE) %>% 
  filter(Q18_1 != "") 

roses$zawody[(stri_detect_fixed(roses$Q18_1, "zastanawiam") |  roses$Q18_1 == "nwm" | roses$Q18_1 == "jeszcze nw")& is.na(roses$zawody)] <- "nie wiem"

write.csv(roses, file = "roses_1.csv")

