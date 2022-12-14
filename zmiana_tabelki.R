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


zawody1 <- c("informatyk", "lekarz", "psycholog", "weterynarz", "architek", "mechanik", "programist", "pi?karz",
             "naukow", "prawnik", "fotograf", "kuchar", "grafik", "t?umacz", "elektryk", "nauczyciel", "kosmety", "projektant",
             "stra?ak", "stewardess", "aktor", "dietety", "polic", "fryzjer", "kierowc", "?o?nie", "rolnik",
             "sportow", "piosenka", "cukiernik", "chirurg", "stolar", "muzyk", "sprzedawca", "dziennika", "pisar", "biolog", 
             "makija?yst", "tance", "archeolog", "fizjoterapeut", "astronaut", "prezydent", "trener", "opiekun", "wiza?", "szef",
             "farmaceut", "przewodnik", "ratownik", "chemik", "pilot", "kosmetolog", "in?ynier", "artyst", "astrofizyk",
             "detektyw", "okulista", "bokser", "szachis", "polityk", "kardiolog", "logistyk", "tapicer", "mechatronik",
             "przedszkola", "stomatolog", "dentyst", "ogrodni", "murar", "?lusar", "bank")

zawody2 <- c("informatyk", "lekarz", "psycholog", "weterynarz", "architekt", "mechanik", "programista", "pi?karz",
             "naukowiec", "prawnik", "fotograf", "kucharz", "grafik", "t?umacz", "elektryk", "nauczyciel", "makija?ysta", "projektant",
             "stra?ak", "stewardessa", "aktor", "dietetyk", "policjant", "fryzjer", "kierowca", "?o?nierz", "rolnik",
             "sportowiec", "piosenkarz", "cukiernik", "chirurg", "stolarz", "muzyk", "sprzedawca", "dziennikarz", "pisarz", "biolog", 
             "makija?ysta", "tancerz", "archeolog", "fizjoterapeuta", "astronauta", "prezydent", "trener", "opiekun", "wiza?ysta", "szef",
             "farmaceuta", "przewodnik", "ratownik", "chemik", "pilot", "kosmetolog", "in?ynier", "artysta", "astrofizyk", 
             "detektyw", "okulista", "bokser", "szachista", "polityk", "kardiolog", "logistyk", "tapicer", "mechatronik",
             "przedszkolanka", "dentysta", "dentysta", "ogrodnik", "murarz", "?lusarz", "bankier")

zawody <- rep(NA, length(roses1$Q18_1))

for (i in 1:length(zawody1)){
  for (j in which(!is.na(zawody) & stri_detect_fixed(roses1$Q18_1, zawody1[i]))){
    zawody[j] <- paste(zawody[j], zawody2[i], sep = ", ")
  }
  
  zawody[stri_detect_fixed(roses1$Q18_1, zawody1[i]) & is.na(zawody)] <- zawody2[i]
}

roses$zawody <- zawody

#uzupe?niam r?cznie
roses$zawody[roses$Q18_1 == " Fryzierka"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "Copywriterem"] <- "copywriter"
roses$zawody[roses$Q18_1 == "zatrudniona w zawodzie, gdzie potrzebna jest umiej?tno?? rysowania na r??nych urz?dzeniach i normalnie na p??tnie, kartkach"] <- "artysta"
roses$zawody[roses$Q18_1 == "co? zwi?zane z ko?mi"] <- "koniara"
roses$zawody[roses$Q18_1 == "chcia?abym otworzy? w?asny sklep, bzines"] <- "szef, sprzedawca"
roses$zawody[roses$Q18_1 == "chcialbym za?orzyc firme paleciarsk? albo ogulnobudowlan?"] <- "szef, budowlaniec"
roses$zawody[roses$Q18_1 == "Budowlanka"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "Konstruktor"] <- "budowlaniec, in?ynier"
roses$zawody[roses$Q18_1 == "Pracowa? w biurze"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "pawel jumper"] <- "pawe? jumper"
roses$zawody[roses$Q18_1 == "Infortmatykiem"] <- "informatyk"
roses$zawody[roses$Q18_1 == "Badacz kosmosu"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "Chcia?bym by? niani?"] <- "opiekun"
roses$zawody[roses$Q18_1 == "Technik Pojazd?w Samochodowych"] <- "mechanik"
roses$zawody[roses$Q18_1 == "osob? zajmuj?c? si? komputerami"] <- "informatyk"
roses$zawody[roses$Q18_1 == "Krawcow?"] <- "rzemie?lnik"
roses$zawody[roses$Q18_1 == "biznesmen"] <- "szef"
roses$zawody[roses$Q18_1 == "Ja chcia?bym by? zawodnikiem zjazdowym."] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Patomorfolog/plastyk"] <- "lekarz, artysta"
roses$zawody[roses$Q18_1 == "Robotnika"] <- "robotnik"
roses$zawody[roses$Q18_1 == "Laborantem"] <- "chemik"
roses$zawody[roses$Q18_1 == "Chcia?bym Mie? firm? Budowlan?"] <- "szef, budowlaniec"
roses$zawody[roses$Q18_1 == "je?dzi? zawodowo na eowerze"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "operatorem koparko-?adowarki"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "trendwatcher"] <- "socjolog"
roses$zawody[roses$Q18_1 == "Menagerem lub kierownikem jakiej? firmy"] <- "szef"
roses$zawody[roses$Q18_1 == "co? zwi?zane z biurem"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "odkrywca technologii"] <- "in?ynier"
roses$zawody[roses$Q18_1 == "sprzedawc? ubra?"] <- "sprzedawca"
roses$zawody[roses$Q18_1 == "ginekolog"] <- "lekarz"
roses$zawody[roses$Q18_1 == "podologiem"] <- "lekarz"
roses$zawody[roses$Q18_1 == "technikiem"] <- "in?ynier"
roses$zawody[roses$Q18_1 == "kelnerem/sprzedawc?"] <- "kelner, sprzedawca"
roses$zawody[roses$Q18_1 == "spedytorem"] <- "logistyk"
roses$zawody[roses$Q18_1 == "dekoratorem wn?trz"] <- "artysta"
roses$zawody[roses$Q18_1 == "florystka"] <- "biolog"
roses$zawody[roses$Q18_1 == "Badaczem banan?w"] <- "biolog"
roses$zawody[roses$Q18_1 == "Chcia?abym mie? prac? zwi?zan? z j?zykami obcymi."] <- "t?umacz"
roses$zawody[roses$Q18_1 == "Poicjantka"] <- "policjant"
roses$zawody[roses$Q18_1 == "popularnym radc? prawnym"] <- "prawnik"
roses$zawody[roses$Q18_1 == "Weterynaria"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "Rolnictwo"] <- "rolnik"
roses$zawody[roses$Q18_1 == "M?j zaw?d musia?by by? powi?zany z matematyk? i rysowaniem."] <- "architekt"
roses$zawody[roses$Q18_1 == "Maryna?em"] <- "marynarz"
roses$zawody[roses$Q18_1 == "prezesem firmy"] <- "szef"
roses$zawody[roses$Q18_1 == "pracowa? na maszynach numerycznych"] <- "informatyk"
roses$zawody[roses$Q18_1 == "okulist?"] <- "lekarz"
roses$zawody[roses$Q18_1 == "bizneswoman"] <- "szef"
roses$zawody[roses$Q18_1 == "pracowa? w jaki? wi?kszych korporacjach"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "osob? kt?ra jest w styczno?ci z natur?"] <- "biolog"
roses$zawody[roses$Q18_1 == "weteryniarzem"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "strazak"] <- "stra?ak"
roses$zawody[roses$Q18_1 == "poliglot?"] <- "t?umacz"
roses$zawody[roses$Q18_1 == "jeszcze do ko?ca nie wiem kim ale co? zwi?zanego ze sportem"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "animatorem"] <- "animator"
roses$zawody[roses$Q18_1 == "Instruktor jazdy konnej"] <- "koniara"
roses$zawody[roses$Q18_1 == "Groomer"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "tlumacz"] <- "t?umacz"
roses$zawody[roses$Q18_1 == "Sterownikiem robot?w"] <- "in?ynier"
roses$zawody[roses$Q18_1 == "Zawodniczk? siatk?wki i zajmowa? si? dzietetyk? lub pracowa? na wydziale kryminologicznym"] <- "spotowiec, dietetyk, detektyw"
roses$zawody[roses$Q18_1 == "w?a?ciciel firmy g??wnie z samochodami"] <- "szef"
roses$zawody[roses$Q18_1 == "?ledczym"] <- "detektyw"
roses$zawody[roses$Q18_1 == "Fryzierem"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "Rzemie?lnik"] <- "rzemie?lnik"
roses$zawody[roses$Q18_1 == "Technik logostyk"] <- "logistyk"
roses$zawody[roses$Q18_1 == "kafelkarz"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "Perkusista metalowy"] <- "muzyk"
roses$zawody[roses$Q18_1 == "marynarzem"] <- "marynarz"
roses$zawody[roses$Q18_1 == "E-sport"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "od dzieci?stwa marz? o staniu na scenie"] <- "aktor"
roses$zawody[roses$Q18_1 == "chcia?abym wykonywa? zaw?d przeczkolank?"] <- "przedszkolanka"
roses$zawody[roses$Q18_1 == "by? austrona?t?"] <- "astronauta"
roses$zawody[roses$Q18_1 == "S?dzia pi?karski"] <- "s?dzia"
roses$zawody[roses$Q18_1 == "psychiatra/patolog s?dowy/prokurator"] <- "psycholog, prawnik"
roses$zawody[roses$Q18_1 == "Tokarz"] <- "rzemie?lnik"
roses$zawody[roses$Q18_1 == "genetykiem"] <- "biolog"
roses$zawody[roses$Q18_1 == "fryzjrk?"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "architrkt"] <- "architekt"
roses$zawody[roses$Q18_1 == "Pilkarz"] <- "pi?karz"
roses$zawody[roses$Q18_1 == "asystentk? w biurze"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "Weterzynarzem"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "Pracownikiem w biurze"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "Le?niczym"] <- "le?niczy"
roses$zawody[roses$Q18_1 == "Wychowawc? grupy przedszkolnej"] <- "przedszkolanka"
roses$zawody[roses$Q18_1 == "Raperem"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "cz?onkiem astrofizycznej grupy badawczej lub w jaki? inny spos?b by? zwi?zan? z badaniem wrzech?wiata"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "miechanik samochodowy"] <- "mechanik"
roses$zawody[roses$Q18_1 == "Pracowa? w hotelu"] <- "hotelarz"
roses$zawody[roses$Q18_1 == "Wuefista"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "Ochroniarzem SOP"] <- "policjant"
roses$zawody[roses$Q18_1 == "Instruktork? ta?ca lub po prostu ta?cerk?"] <- "tancerz"
roses$zawody[roses$Q18_1 == "Rolnictwo/G?rnik"] <- "rolnik, g?rnik"
roses$zawody[roses$Q18_1 == "Budowlaniec"] <- "budowlaniec"
roses$zawody[roses$Q18_1 == "Raper i tyke"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "zoolog"] <- "biolog"
roses$zawody[roses$Q18_1 == "Malarka"] <- "malarz"
roses$zawody[roses$Q18_1 == "zwi?zany z technologi? lub nauk?"] <- "in?ynier"
roses$zawody[roses$Q18_1 == "Automatyk"] <- "in?ynier"
roses$zawody[roses$Q18_1 == "Chcia?abym mie? swoj? firm? lub by? jakim? managerem dobrej firmy/hotelu"] <- "szef, hotelarz"
roses$zawody[roses$Q18_1 == "Deweloperem"] <- "deweloper"
roses$zawody[roses$Q18_1 == "ekonomista"] <- "ekonomista"
roses$zawody[roses$Q18_1 == "Cyberbezpiecze?stwo"] <- "programista"
roses$zawody[roses$Q18_1 == "leka?em"] <- "lekarz"
roses$zawody[roses$Q18_1 == "Zosta? w?a?cicielem firmy wykorzystuj?cej nowe technologie."] <- "szef"
roses$zawody[roses$Q18_1 == "fizyk j?drowy/subatomowy"] <- "fizyk"
roses$zawody[roses$Q18_1 == "Instruktorka jazdy konnej"] <- "koniara"
roses$zawody[roses$Q18_1 == "W?a?ciciel du?ej firmy zajmuj?cej si? e-commerce"] <- "szef"
roses$zawody[roses$Q18_1 == "Ekonomik/ksi?gowa"] <- "ekonomista, ksi?gowy"
roses$zawody[roses$Q18_1 == "by? Rybakiem"] <- "rybak"
roses$zawody[roses$Q18_1 == "Fryzier"] <- "fryzjer"
roses$zawody[roses$Q18_1 == "geodetom"] <- "geodeta"
roses$zawody[roses$Q18_1 == "Hydraulika"] <- "hydraulik"
roses$zawody[roses$Q18_1 == "Grabiarzem"] <- "grabarz"
roses$zawody[roses$Q18_1 == "W przysz?o?ci chcia?abym zosta? dermatologiem."] <- "lekarz"
roses$zawody[roses$Q18_1 == "Lektorka"] <- "lektor"
roses$zawody[roses$Q18_1 == "Masa?"] <- "masa?ysta"
roses$zawody[roses$Q18_1 == "wojskowy"] <- "?o?nierz"
roses$zawody[roses$Q18_1 == "Psi Behawiorysta"] <- "treser"
roses$zawody[roses$Q18_1 == "weteryna?em"] <- "weterynarz"
roses$zawody[roses$Q18_1 == "rysowa? komiksy"] <- "grafik"
roses$zawody[roses$Q18_1 == "malarz"] <- "malarz"
roses$zawody[roses$Q18_1 == "medycyna estetyczna"] <- "lekarz, makija?ysta"
roses$zawody[roses$Q18_1 == "Nie wiem kim dok?adnie, ale kim? zwi?zanym z histori?"] <- "historyk"
roses$zawody[roses$Q18_1 == "Pracowa? w hodowli zwierz?t np.egzotycznych"] <- "biolog"
roses$zawody[roses$Q18_1 == "dropshipping"] <- "logistyk"
roses$zawody[roses$Q18_1 == "prowadzi? agroturystyk?"] <- "przewodnik, hotelarz"
roses$zawody[roses$Q18_1 == "Antyterroryst?"] <- "policjant"
roses$zawody[roses$Q18_1 == "kierownik budowy"] <- "szef, budowlaniec"
roses$zawody[roses$Q18_1 == "prawnkiem"] <- "prawnik"
roses$zawody[roses$Q18_1 == "gra? w najlepszym klubie w polsce jakim jest stal mielec"] <- "pi?karz"
roses$zawody[roses$Q18_1 == "co? zwi?zanego z chemi?"] <- "chemik"
roses$zawody[roses$Q18_1 == "Animatorka"] <- "animator"
roses$zawody[roses$Q18_1 == "co? z gastronomi?"] <- "kucharz"
roses$zawody[roses$Q18_1 == "mlotocyklist?"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Kim? kto jest zwi?zany z j?zykiem angielskim"] <- "t?umacz"
roses$zawody[roses$Q18_1 == "Anglistk?"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "Raper i producent muzyczny"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "Ortodontk?"] <- "dentysta"
roses$zawody[roses$Q18_1 == "Stra? po?arna"] <- "stra?ak"
roses$zawody[roses$Q18_1 == "stra?nik le?ny"] <- "le?niczy"
roses$zawody[roses$Q18_1 == "wynalasca"] <- "in?ynier"
roses$zawody[roses$Q18_1 == "Okulist?"] <- "lekarz"
roses$zawody[roses$Q18_1 == "Pracownikiem firmy"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "komornik"] <- "komornik"
roses$zawody[roses$Q18_1 == "pracownikiem salonu Audi lub innej marki samochod?w kt?ra mi si? podoba"] <- "sprzedawca"
roses$zawody[roses$Q18_1 == "Kryminologiem i kryminalistykiem"] <- "detektyw"
roses$zawody[roses$Q18_1 == "Koszykarzem"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Komik"] <- "komik"
roses$zawody[roses$Q18_1 == "Kim? kto ma co? wsp?lnego ze sportem"] <- "sportowiec"
roses$zawody[roses$Q18_1 == "Recepcjonistk?"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "ekspedientka"] <- "sprzedawca"
roses$zawody[roses$Q18_1 == "Gastronomem"] <- "kucharz"
roses$zawody[roses$Q18_1 == "Kontrolerem ruchu lotniczego"] <- "logistyk"
roses$zawody[roses$Q18_1 == "Medykiem"] <- "lekarz"
roses$zawody[roses$Q18_1 == "Chemiczk?"] <- "chemik"
roses$zawody[roses$Q18_1 == "?ledczym lub kryminologiem"] <- "detektyw"
roses$zawody[roses$Q18_1 == "diagnost? samochodowy,"] <- "mechanik"
roses$zawody[roses$Q18_1 == "elektro monter"] <- "elektryk"
roses$zawody[roses$Q18_1 == "Scenarzystk? lub psycholoszk?"] <- "pisarz, psycholog"
roses$zawody[roses$Q18_1 == "chcia?abym by? deweloperem"] <- "deweloper"
roses$zawody[roses$Q18_1 == "hydraulik"] <- "hydraulik"
roses$zawody[roses$Q18_1 == "Pi?kark?"] <- "pi?karz"
roses$zawody[roses$Q18_1 == "saperem lub snajperem"] <- "?o?nierz"
roses$zawody[roses$Q18_1 == "Adwokat"] <- "prawnik"
roses$zawody[roses$Q18_1 == "dzia?acz na rzecz ochrony  ?rodowiska"] <- "ekolog"
roses$zawody[roses$Q18_1 == "Sekretar?"] <- "pracownik biurowy"
roses$zawody[roses$Q18_1 == "geolog"] <- "geolog"
roses$zawody[roses$Q18_1 == "ksi?gow?"] <- "ksi?gowy"
roses$zawody[roses$Q18_1 == "testerem gier komputerowych"] <- "tester"
roses$zawody[roses$Q18_1 == "niania"] <- "opiekun"
roses$zawody[roses$Q18_1 == "Pedagaog"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "solista/dyrygent"] <- "muzyk"
roses$zawody[roses$Q18_1 == "badaczem kosmosu"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "strzelcem wyborowym si? specjalnych"] <- "?o?nierz"
roses$zawody[roses$Q18_1 == "Oficerem"] <- "?o?nierz"
roses$zawody[roses$Q18_1 == "Ksi?gow?"] <- "ksi?gowy"
roses$zawody[roses$Q18_1 == "floryst?"] <- "biolog"
roses$zawody[roses$Q18_1 == "Polonista/ Humanista"] <- "nauczyciel"
roses$zawody[roses$Q18_1 == "hotelark?"] <- "hotelarz"
roses$zawody[roses$Q18_1 == "Astronem"] <- "astrofizyk"
roses$zawody[roses$Q18_1 == "Raperem"] <- "piosenkarz"
roses$zawody[roses$Q18_1 == "menad?er lub w rejestracji"] <- "szef"
roses$zawody[roses$Q18_1 == "Nie wiem jeszcze, ale zastanawiam si? nad prawem."] <- "prawnik"
roses$zawody[roses$Q18_1 == "Specjalista do spraw transportu"] <- "logistyk"
roses$zawody[roses$Q18_1 == "g?ownie militaria konstruktor pojazd?w"] <- "in?ynier"
roses$zawody[roses$Q18_1 == "Mangaka, Animator"] <- "grafik, animator"
roses$zawody[roses$Q18_1 == "kierownikiem apteki"] <- "szef, farmaceuta"
roses$zawody[roses$Q18_1 == "tw?rc? gier komputerowych"] <- "programista"
roses$zawody[roses$Q18_1 == "Tatua?ysta"] <- "makija?ysta"
roses$zawody[roses$Q18_1 == "Kosmona?ta"] <- "astronauta"
roses$zawody[roses$Q18_1 == "Chcialabym pracowac w wojsku"] <- "?o?nierz"
roses$zawody[roses$Q18_1 == "Rysownik/Malarz"] <- "malarz"
roses$zawody[roses$Q18_1 == "wojskowym."] <- "?o?nierz"
roses$zawody[roses$Q18_1 == "Spawacz"] <- "spawacz"
roses$zawody[roses$Q18_1 == "Najlepiej kim? po technikum ekonomistycznym"] <- "ekonomista"
roses$zawody[roses$Q18_1 == "Psychiatra"] <- "psycholog"
roses$zawody[roses$Q18_1 == "Biznes woman"] <- "szef"
roses$zawody[roses$Q18_1 == "Infomatykiem"] <- "informatyk"
roses$zawody[roses$Q18_1 == "stra?nikiem granicznym"] <- "policjant"
roses$zawody[roses$Q18_1 == "Projektowa? reklamy"] <- "grafik"
roses$zawody[roses$Q18_1 == "Spawa?"] <- "spawacz"
roses$zawody[roses$Q18_1 == "ksi?gowym"] <- "ksi?gowy"
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

