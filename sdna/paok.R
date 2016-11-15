# PAOK
pacman::p_load("rvest", "dplyr", "highcharter", "magrittr", "lubridate", "tidyr",
               "viridisLite", "pbapply")

trURL <- c("http://www.transfermarkt.gr/paok-thessaloniki/kader/verein/1091/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/olympiacos-piraeus/kader/verein/683/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/aek-athens/kader/verein/2441/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/panathinaikos-athens/kader/verein/265/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/asteras-tripolis/kader/verein/6676/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/atromitos-athen/kader/verein/3060/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/iraklis-thessaloniki/kader/verein/47/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/aok-kerkyra/kader/verein/5219/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/ae-larisa/kader/verein/4603/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/apo-levadiakos/kader/verein/2672/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/panionios-athens/kader/verein/169/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/panetolikos-agrinio/kader/verein/6418/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/pas-giannina/kader/verein/2671/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/platanias-chania/kader/verein/21957/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/pae-veria/kader/verein/2079/saison_id/2016/plus/1",
"http://www.transfermarkt.gr/ao-xanthi/kader/verein/128/saison_id/2016/plus/1")
u <- 4
all <- pblapply(1:length(trURL), function(u){
cat(u)
tr <- read_html(trURL[u])

NationalPlayers <- tr %>% html_nodes(".list:nth-child(1) tr:nth-child(5) a") %>% html_text
stadiumseats <- tr %>% html_nodes(".wsn .tabellenplatz") %>% html_text %>% 
  gsub(" Seats|\\.", "", .) %>% as.numeric()
ttlMarkValue <- tr %>% html_nodes(".marktwert a") %>% html_text
clubflag <- tr %>% html_nodes(".headerfoto img") %>% xml_attr("src") %>% sub("^//", "", .)

nUrl <- trURL[u] %>% sub("/kader/verein/", "/transfers/verein/", .) %>% sub("/plus/1$", "", .)
tr2 <- read_html(nUrl)

tr3 <- tr2 %>% html_nodes(".container-inhalt a") %>% .[1] %>% xml_attr("href") %>% paste0("http://www.transfermarkt.gr", .)
tr3 %<>% read_html


tb <- tr3 %>% html_nodes(".spielerdaten")
tb <- tb %>% html_nodes("table") %>% html_table %>% .[[1]]

YearsAsCouch <- tb[grepl("term as coach", tb[, 1]), 2] %>% sub(" Years", "", .)
pointsPerMatch <- tb[grepl("Points per match as manager", tb[, 1]), 2] 
Wins <- tb[grepl("Success rate as coach", tb[, 1]), 2]  %>% sub(" Wins.*", "", .)
Losses <- tb[grepl("Success rate as coach", tb[, 1]), 2]  %>% sub(".*Draw(.*) Losses", "\\1", .)
coachPhoto <- tr3 %>% html_nodes(".bilderrahmen-fixed") %>% xml_attr("src") %>% 
  sub("^//", "", .) %>% sub("(.*jpg).*", "\\1", .)
  
esodoMetagr <- tr2 %>% html_nodes(".box > table .rechts")%>% .[2] %>% html_text %>% gsub("\\r|\\n|\\t", "", .)
agoresMetagr <- tr2 %>% html_nodes(".box > table .rechts")%>% .[3] %>% html_text %>% gsub("\\r|\\n|\\t", "", .)
net <- tr2 %>% html_nodes(".box > table .rechts") %>% .[4] %>% html_text %>% gsub("\\r|\\n|\\t", "", .)
coach <- tr2 %>% html_nodes("#0") %>% .[1] %>% html_text
CoachNational <- tr2 %>% html_nodes(".container-zusatzinfo img") %>% .[1] %>% xml_attr("title")
coachAge <- tr2 %>% html_nodes(".container-zusatzinfo") %>% .[1] %>% html_text %>% gsub("\\r|\\n|\\t", "", .) %>% 
  sub(".*Age: ([0-9]+) Year.*", "\\1", .) %>% as.numeric()
coachContrUntil <- tr2 %>% html_nodes(".container-zusatzinfo") %>% .[1] %>% html_text %>% gsub("\\r|\\n|\\t", "", .) %>% 
  sub(".*until: (.*)", "\\1", .) %>% dmy

Names <- tr %>% html_nodes(".spielprofil_tooltip") %>% html_text
Thesi <- tr %>% html_nodes(".inline-table tr+ tr td") %>% html_text
Age <- tr %>% html_nodes("#yw1 td:nth-child(3)") %>% html_text
Age %<>% sub(".*\\(([0-9]+)\\)$", "\\1", .) %>% as.numeric
National <- tr %>% html_nodes("td:nth-child(4)")
National <- lapply(National, function(x){
  x %>% html_nodes("img") %>% xml_attr("title") %>% paste(collapse=" | ")
}) %>% unlist
Height <- tr %>% html_nodes("td:nth-child(5)") %>% html_text %>% sub(" m", "", .) %>% 
  sub(",", "\\.", .) %>% as.numeric %>% {. * 100}
Foot <- tr %>% html_nodes("td:nth-child(6)") %>% html_text
TeamSince <- tr %>% html_nodes("td:nth-child(7)") %>% html_text
PrevTeam <- lapply(tr %>% html_nodes("td:nth-child(8)"), function(z) {t <- z %>% html_nodes("img") %>% xml_attr("alt")
  if(!length(t)) t <- ""
  t
}) %>% unlist

ContractUntil <- tr %>% html_nodes("td:nth-child(9)") %>% html_text
Value <- tr %>% html_nodes(".rechts.hauptlink") %>% html_text
team <- tr %>% html_nodes("h1") %>% html_text
team <- rep(team, length(Value))
photo <- tr %>% html_nodes(".bilderrahmen-fixed") %>% xml_attr("src") %>% 
  sub("^//", "", .) %>% sub("(.*jpg).*", "\\1", .)

df <- data.frame(Team=team, Name=Names, Thesi=Thesi, Age=Age, Nationality=National, Height=Height, Foot=Foot,
                 TeamSince=TeamSince, previousTeam=PrevTeam, Until=ContractUntil,
                 Value=Value, PhotoUrl=photo)

a <- gsub(".*(^[0-9]+.*[0-9]+).*", "\\1", df$Value) %>% 
  sub(",", "\\.", .) %>% as.numeric()
b <- sub(".*Th\\..*", "1000", df$Value) %>% 
  sub(".*Mill\\..*", "1000000", .) %>% as.numeric()

df$Value2 <- round(a*b/1000000,2)

df$NumOfNationalPlayers <- NationalPlayers
df$stadiumseats <- stadiumseats
df$TeamTotlMarkValue <- ttlMarkValue
df$ClubFlag <- clubflag
df$EsodaMetagrafwn <- esodoMetagr
df$EksodaMetagrafwn <- agoresMetagr
df$KatharaEsoda <- net
df$CoachName <- coach
df$CoachNationality <- CoachNational
df$CoachAge <- coachAge
df$CoachContrUntil <- coachContrUntil

df$YearsAsCouch <- YearsAsCouch
df$PointsPerMatch <- pointsPerMatch
df$CoachWins <- Wins
df$CoachLosses <- Losses
df$CoachPhoto <- coachPhoto
  
df
})

df <- do.call(rbind, all)

df$playerContrUntil <- round((dmy(df$Until) - ymd(Sys.Date()))/365,1)
df$CoachContractUntil <- round((ymd(df$CoachContrUntil) - ymd(Sys.Date()))/365,1)

write.csv(df, file="C:\\Users\\dpsaradellis\\Desktop\\sdna.csv")
save(df, file="C:\\Users\\dpsaradellis\\Desktop\\sdna.RData")
# load(file="C:\\Users\\dpsaradellis\\Desktop\\sdna.RData")
