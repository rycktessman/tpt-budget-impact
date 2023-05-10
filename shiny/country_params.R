library(tidyverse)
library(data.table)
library(readxl)

#Load country names and codes
country_info <- read.csv("shiny/countries.csv") %>% 
  filter(!(code %in% c("NIU", "TWN")))

#LTBI PREVALENCE
#missing for kiribati/marshall is/micronesia/niue (use fiji), timor-leste (use indonesia), taiwan (use china), maldives (manually add)
ltbi <- read.csv("data-raw/ltbi_country_estimates_rein_houben_dodd.csv", skip=1) #estimated # ppl with LTBI by country
ltbi <- ltbi %>% mutate(ltbi=substr(All.LTBI, start=1, stop=11))
ltbi <- ltbi %>% mutate(ltbi=str_replace_all(ltbi, fixed("-"), ""))
ltbi <- ltbi %>% mutate(ltbi=parse_number(ltbi))
ltbi <- ltbi %>% filter(iso3 %in% c(country_info$code, "FJI", "CHN")) %>%
  select(iso3, starts_with("ltbi", ignore.case=F))
ltbi <- bind_rows(ltbi, data.frame(iso3="MDV", ltbi=115000*435018/530957)) #based on https://www.sciencedirect.com/science/article/pii/S1201971222002922, adj. for population growth 2014-19
ltbi <- left_join(ltbi, country_info, by=c("iso3"="code"))
ltbi_params <- ltbi %>% mutate(country=case_when(iso3=="FJI"~"Fiji", 
                                          iso3=="TZA"~"United Republic of Tanzania",
                                          iso3=="VEN"~"Venezuela (Bolivarian Republic of)",
                                          iso3=="CHN"~"China",
                                          iso3=="IRN"~"Iran (Islamic Republic of)",
                                          iso3=="VNM"~"Viet Nam",
                                          iso3=="CIV"~"CÃ´te d'Ivoire",
                                          TRUE~country))
#load 2014 population data to estimate ltbi prevalence
load("data/pop.Rda")
pop <- pop %>% filter(Location %in% ltbi$country & Time==2014)
pop <- pop %>% group_by(Location) %>% summarise(PopTotal=sum(PopTotal)*1000)
ltbi <- left_join(ltbi, pop, by=c("country"="Location"))
ltbi <- ltbi %>% mutate(ltbi_prev=ltbi/PopTotal)

#COSTS
#function to calculate inflation
calc_inflation <- function(cost, yr_data, yr_end, country_wdi) {
  yr_seq <- lapply(yr_data, function(x) x:yr_end)
  inf_seq <- lapply(yr_seq, function(x) country_wdi %>% filter(year %in% x) %>% 
                                                  pull(inf_prices))
  inf <- lapply(inf_seq, function(x) if_else(length(x)==1, 1, prod(1 + x)))
  inf <- unlist(inf)
  cost_inf <- cost*inf
  return(cost_inf)
}

#load world bank WDI data on inflation, exchange rates, gni pc to do conversions
wdi_data <- read.csv("data-raw/world_bank_wdi_extract.csv", stringsAsFactors=F)
names(wdi_data) <- c("country", "code", "var_name", "var_code", as.character(2006:2020))
wdi_data <- pivot_longer(wdi_data, cols=as.character(2006:2020), 
                         names_to="year", values_to="value") %>% 
  mutate(year=as.integer(year), value=as.numeric(value)) #warning msg here is fine
wdi_data <- wdi_data %>% mutate(var=case_when(var_code=="NY.GNP.PCAP.CD"~"gnipc_atlas",
                                              var_code=="NY.GNP.PCAP.PP.CD"~"gnipc_ppp",
                                              var_code=="PA.NUS.FCRF"~"lcu_per_usd",
                                              var_code=="FP.CPI.TOTL.ZG"~"inf_prices",
                                              var_code=="NY.GDP.DEFL.KD.ZG"~"inf_gdp"))
wdi_data <- wdi_data %>% filter(code %in% c(country_info$code, "USA", "PER"))  #Peru & USA also used in some calcs 
wdi_data <- wdi_data %>% select(-c("var_name", "var_code")) %>%
  pivot_wider(id_cols=c("country", "code", "year"), names_from="var", values_from="value")
#use IMF for data on inflation and conversions - WDI is missing a lot (dl from here: https://www.imf.org/en/Publications/SPROLLS/world-economic-outlook-databases#sort=%40imfdate%20descending)
#consumer price inflation
imf_inf_data <- read.csv("data-raw/WEOOct2021all.csv", stringsAsFactors=F)
names(imf_inf_data) <- c(names(imf_inf_data)[1:9], as.character(1980:2026), 
                         tail(names(imf_inf_data),1))
imf_inf_data <- pivot_longer(imf_inf_data, cols=as.character(1980:2026),
                         names_to="year", values_to="inf_prices_imf") %>%
  mutate(year=as.integer(year), inf_prices_imf=as.numeric(inf_prices_imf)) #warning msg here is fine
imf_inf_data <- imf_inf_data %>% 
  filter(year>=2006 & year<=2020 &
           ISO %in% c(country_info$code, "USA", "PER") &
           WEO.Subject.Code %in% c("PCPIPCH")) %>% 
  select(WEO.Country.Code, ISO, Country, year, inf_prices_imf)
imf_inf_data <- imf_inf_data %>%
  rename("code_num"="WEO.Country.Code", 
         "code"="ISO", 
         "country"="Country")

#currency conversions
imf_exc_data <- fread("data-raw/imf_data.csv", stringsAsFactors=F,
                      colClasses=c(rep("character", 5), 
                                   rep("numeric", 1196-5)))
names(imf_exc_data) <- c("country", "code_num", "indicator", "indicator_code",
                     names(imf_exc_data)[5:ncol(imf_exc_data)])
imf_exc_data <- imf_exc_data %>% 
  filter(indicator_code=="ENDA_XDC_USD_RATE" &
           code_num %in% c(country_info$code_num, "111", "604", "915") #add Peru US Georgia
         & Attribute!="Status") 
imf_exc_data <- imf_exc_data %>% select(!c(contains("Q"), 
                                           contains("M", ignore.case=F)))
imf_exc_data <- imf_exc_data %>% 
  group_by(country, code_num, indicator, indicator_code, Attribute) %>%
  mutate_all(as.numeric)
#separate rows for quarterly, monthly, annual. but Q and M deleted, so can add
imf_exc_data <- imf_exc_data %>% 
  group_by(country, code_num, indicator, indicator_code, Attribute) %>%
  summarise_all(~sum(., na.rm=T)) 
imf_exc_data <- imf_exc_data %>% select(-V1196)
imf_exc_data <- pivot_longer(imf_exc_data, cols=as.character(1948:2017),
                             names_to="year", values_to="lcu_per_usd_imf")
imf_exc_data <- imf_exc_data %>% ungroup() %>%
  mutate(year=as.numeric(year)) %>%
  filter(year>=2006) %>% 
  select(country, code_num, year, lcu_per_usd_imf)
#merge everything together
wdi_data <- left_join(wdi_data, imf_inf_data %>% select(-country),
                      by=c("code", "year"))
wdi_data <- left_join(wdi_data, imf_exc_data %>% select(-country),
                      by=c("code_num", "year"))
#default to IMF data, use WDI if missing
wdi_data <- wdi_data %>% 
  mutate(lcu_per_usd_imf=if_else(is.na(lcu_per_usd_imf)|lcu_per_usd_imf==0, lcu_per_usd, lcu_per_usd_imf),
         inf_prices_imf=case_when((is.na(inf_prices_imf) & !is.na(inf_prices))~inf_prices,
                                  (is.na(inf_prices_imf) & is.na(inf_prices))~inf_gdp,
                                  TRUE~inf_prices_imf))
#need to impute some data for 2017-2020 - exchange rates
wdi_data[wdi_data$country=="Brazil" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(3.19, 3.65, 3.94, 5.16)
wdi_data[wdi_data$country=="Bangladesh" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(80.44, 83.47, 84.45, 84.87)
wdi_data[wdi_data$country=="Cambodia" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(4050.58, 4051.17, 4061.15, 4092.78)
wdi_data[wdi_data$country=="Chile" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(648.83, 641.28, 702.9, 792.73)
wdi_data[wdi_data$country=="Cuba", "lcu_per_usd_imf"] <-24
wdi_data[wdi_data$code=="COD" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(1464.42, 1622.52, 1647.76, 1851.12)
wdi_data[wdi_data$country=="Maldives" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(15.39, 15.39, 15.38, 15.38)
wdi_data[wdi_data$country=="Marshall Islands", "lcu_per_usd_imf"] <- 1
wdi_data[wdi_data$country=="Philippines" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(50.4, 52.66, 51.8, 49.62)
wdi_data[wdi_data$country=="Turkey" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(3.65, 4.83, 5.67, 7.01)
wdi_data[wdi_data$country=="Turkmenistan", "lcu_per_usd_imf"] <- 
  c(rep(2.85, 10), 3.5, 3.4, 3.4, 3.45, 3.45) #from oanda
wdi_data[wdi_data$country=="Uruguay" & wdi_data$year %in% 2017:2020,
         "lcu_per_usd_imf"] <- c(28.68, 30.73, 35.26, 42.01)
wdi_data[wdi_data$country=="Uzbekistan" & wdi_data$year %in% 2006:2012,
         "lcu_per_usd_imf"] <- c(11250, 11250, 1309, 1462, 1589, 1720, 1878) #from oanda
wdi_data[wdi_data$country=="Somalia" & wdi_data$year %in% 2006:2008,
         "lcu_per_usd_imf"] <- rep(31558.91, 3) #use 2009 conversion for lack of better data
wdi_data[wdi_data$country=="Somalia" & wdi_data$year %in% 2018:2020,
         "lcu_per_usd_imf"] <- rep(23097.99, 3) #use 2017 conversion for lack of better data
wdi_data[wdi_data$code=="VEN" & wdi_data$year %in% 2018:2020,
         "lcu_per_usd_imf"] <- rep(9.98, 3) #use 2017 conversion for lack of better data
wdi_data[wdi_data$country=="Zimbabwe" & wdi_data$year %in% 2009:2019,
         "lcu_per_usd_imf"] <- rep(51.3, 11)
#inflation data are also missing for some years for Somalia & Venezuela
wdi_data[wdi_data$country=="Somalia" & wdi_data$year %in% 2006:2013,
         "inf_prices_imf"] <- rep(4.361879, 8) #use 2014 rate for lack of better data
wdi_data[wdi_data$code=="VEN" & wdi_data$year %in% 2018:2020,
         "inf_prices_imf"] <- rep(2, 3) #for lack of better data, conservatively assume 2% inflation after 2017
wdi_data[wdi_data$country=="Cuba" & wdi_data$year==2020,
         "inf_prices_imf"] <- 3.3040935 #use 2019 rate for lack of better data 

#GNI pc data (Atlas)
wdi_data[wdi_data$code=="USA" & wdi_data$year==2020,
         "gnipc_atlas"] <- 64530 #impute from WB website
wdi_data[wdi_data$country=="Cuba" & wdi_data$year %in% 2017:2020,
         "gnipc_atlas"] <- c(7950, 8630, 8630*(1+(8630-7950)/7950), 8630) #impute from WB website, estimate 2019-20
wdi_data[wdi_data$country=="Marshall Islands" & wdi_data$year==2020,
         "gnipc_atlas"] <- 4940 #impute from WB website
wdi_data[wdi_data$code=="FSM" & wdi_data$year==2020,
         "gnipc_atlas"] <- 3960 #impute from WB website
wdi_data[wdi_data$country=="Turkmenistan" & wdi_data$year==2020,
         "gnipc_atlas"] <- 6740 #assume drop back to 2018 level
wdi_data[wdi_data$code=="YEM" & wdi_data$year %in% 2019:2020,
         "gnipc_atlas"] <- c(820, 670) #impute from WB website
wdi_data[wdi_data$country=="Somalia" & wdi_data$year %in% 2006:2014,
         "gnipc_atlas"] <- (290+290+130)/3 #wt avg of 2015 and 1990
wdi_data[wdi_data$code=="VEN" & wdi_data$year %in% 2015:2020,
         "gnipc_atlas"] <- (3380*((2015:2020)-2014) + 13080*(2020-(2015:2020)))/6 #weighted avg

#select GNI pc atlas, price inflation, and exchange rates
wdi_data <- wdi_data %>% 
  select(country, code, code_num, year,
         gnipc_atlas, inf_prices_imf, lcu_per_usd_imf) %>%
  rename("inf_prices"="inf_prices_imf", "lcu_per_usd"="lcu_per_usd_imf")
#convert inflation from percent to number
wdi_data <- wdi_data %>% mutate(inf_prices=inf_prices/100)

#pull 2020 GNI per capita for relative GNI calcs
gnipc <- wdi_data %>% filter(year==2020) %>% pull(gnipc_atlas)
names(gnipc) <- wdi_data %>% filter(year==2020) %>% pull(code)

#add LCU to INT$ conversion - downloaded separately
lcu_to_int <- read.csv("data-raw/API_PA.NUS.PPP_DS2_en_csv_v2_2764497.csv", 
                       stringsAsFactors=F, skip=4) %>% select(-X)
names(lcu_to_int) <- c("country", "code", "var_name", "var_code", as.character(1960:2020)) 
lcu_to_int <- lcu_to_int %>% filter(code %in% unique(wdi_data$code))
lcu_to_int <- pivot_longer(lcu_to_int, cols=as.character(1960:2020), 
                         names_to="year", values_to="lcu_per_int") %>% 
  mutate(year=as.integer(year), lcu_per_int=as.numeric(lcu_per_int)) #warning msg here is fine
lcu_to_int <- lcu_to_int %>% filter(year %in% unique(wdi_data$year))
#fill in missing years for Somalia and Venezuela
lcu_to_int[lcu_to_int$country=="Cuba" & lcu_to_int$year %in% c(2006:2010, 2012:2020),
           "lcu_per_int"] <- 0.32 #use 2011 value
lcu_to_int[lcu_to_int$code=="FSM" & lcu_to_int$year==2020,
           "lcu_per_int"] <- 1 #manual from world bank website
lcu_to_int[lcu_to_int$country=="Marshall Islands" & lcu_to_int$year==2020,
           "lcu_per_int"] <- 1 #manual from world bank website
lcu_to_int[lcu_to_int$country=="Turkmenistan" & lcu_to_int$year==2020,
           "lcu_per_int"] <- 1.65 #use 2019 value
lcu_to_int[lcu_to_int$code=="VEN" & lcu_to_int$year %in% 2012:2020,
         "lcu_per_int"] <- c(3.04, 4.05, 5.59, 11.7, 1e-3, 5e-3, 1.3e4, 4.37e5, 1.73e7) #imputed from https://data.nasdaq.com/data/ODA/VEN_PPPEX-venezuela-implied-ppp-conversion-rate-lcu-per-usd
lcu_to_int[lcu_to_int$country=="Somalia" & lcu_to_int$year %in% 2006:2010,
           "lcu_per_int"] <- rep(8001, 5) #use 2011 value
lcu_to_int[lcu_to_int$code=="YEM" & lcu_to_int$year %in% 2014:2020,
           "lcu_per_int"] <- c(88.92, 111.85, 115.86, 136.31, 196.19, 232.48, 258.78) #from https://data.nasdaq.com/data/ODA/YEM_PPPEX-yemen-implied-ppp-conversion-rate-lcu-per-usd
wdi_data <- left_join(wdi_data, lcu_to_int %>% select(code, year, lcu_per_int), 
                      by=c("code", "year"))
#merge in region
wdi_data <- left_join(wdi_data %>% mutate(code_num=as.integer(code_num)), 
                      country_info %>% select(-country), 
                      by=c("code", "code_num"))

#cost parameters - give unit cost, currency yr, currency, and country for which unit cost was estimated
cost_data <- unique(wdi_data %>% select(country, code, region))
#costs that vary regionally
costs_r <- list()
costs_r[["AFRO"]] <- list("c_contact"=6.7,
                          "yr_contact"=2017, 
                          "cur_contact"="USD",
                          "ctr_contact"="UGA",
                          "c_xray"=9,
                          "yr_xray"=2014,
                          "cur_xray"="USD",
                          "ctr_xray"="TZA")
costs_r[["SEARO"]] <- list("c_contact"=6.3,
                           "yr_contact"=2016,
                           "cur_contact"="USD",
                           "ctr_contact"="PAK",
                           "c_xray"=3.3,
                           "yr_xray"=2018,
                           "cur_xray"="USD",
                           "ctr_xray"="IND")
costs_r[["WPRO"]] <- list("c_contact"=5.9,
                          "yr_contact"=2012,
                          "cur_contact"="USD",
                          "ctr_contact"="KHM",
                          "c_xray"=2.6,
                          "yr_xray"=2017,
                          "cur_xray"="USD",
                          "ctr_xray"="KHM")
costs_r[["AMRO"]] <- list("c_contact"=7.5,
                          "yr_contact"=2016,
                          "cur_contact"="USD",
                          "ctr_contact"="PER",
                          "c_xray"=4.8,
                          "yr_xray"=2015,
                          "cur_xray"="USD",
                          "ctr_xray"="BRA")
costs_r[["EURO"]] <- list("c_contact"=10.4,
                          "yr_contact"=2018,
                          "cur_contact"="USD",
                          "ctr_contact"="GEO",
                          "c_xray"=5.8,
                          "yr_xray"=2018,
                          "cur_xray"="USD",
                          "ctr_xray"="GEO")
#costs that don't vary by country
costs_all <- list("c_xpert"=23.3,
                  "yr_xpert"=2020,
                  "cur_xpert"="USD",
                  "c_labtox"=18.2,
                  "yr_labtox"=2020,
                  "cur_labtox"="USD")
#costs that vary by country - read in from CSV files
#outpatient costs from WHO CHOICE
costs_outpatient <- read.csv("data-raw/who_choice_outpatient.csv")
costs_outpatient <- left_join(costs_outpatient, 
                              country_info %>% select(country, code),
                              by="country")
costs_outpatient <- costs_outpatient %>% 
  mutate(code=case_when(country=="Swaziland"~"SWZ",
                      country=="United Republic of Tanzania"~"TZA",
                      country=="Venezuela (Bolivarian Republic of)"~"VEN",
                      country=="C?te d'Ivoire"~"CIV",
                      country=="Iran (Islamic Republic of)"~"IRN",
                      country=="Micronesia (Federated States of)"~"FSM",
                      country=="Viet Nam"~"VNM",
                      country=="Niue"~"NIU",
                      country=="Cuba"~as.character(NA), #use regional instead since missing
                      country=="Somalia"~as.character(NA), #use regional instead since missing
                      country=="Zimbabwe"~as.character(NA), #use regional instead since missing
                      country=="Eastern Sub-Saharan Africa"~"SOM",
                      country=="Southern Sub-Saharan Africa"~"ZWE",
                      country=="Caribbean"~"CUB",
                      TRUE~code))
costs_outpatient <- costs_outpatient %>% filter(!is.na(code)) %>%
  select(code, mean)
c_outpatient <- as.list(costs_outpatient$mean)
names(c_outpatient) <- costs_outpatient$code
yr_outpatient <- 2010
cur_outpatient <- "INT"

#inpatient costs from WHO CHOICE
costs_inpatient <- read.csv("data-raw/who_choice_inpatient.csv")
costs_inpatient <- left_join(costs_inpatient, 
                              country_info %>% select(country, code),
                              by="country")
costs_inpatient <- costs_inpatient %>% 
  mutate(code=case_when(country=="Swaziland"~"SWZ",
                        country=="United Republic of Tanzania"~"TZA",
                        country=="Venezuela (Bolivarian Republic of)"~"VEN",
                        country=="C?te d'Ivoire"~"CIV",
                        country=="Iran (Islamic Republic of)"~"IRN",
                        country=="Micronesia (Federated States of)"~"FSM",
                        country=="Viet Nam"~"VNM",
                        country=="Niue"~"NIU",
                        country=="Somalia"~as.character(NA), #use regional instead since missing
                        country=="Zimbabwe"~as.character(NA), #use regional instead since missing
                        country=="Cuba"~as.character(NA), #use regional instead since missing
                        country=="Eastern Sub-Saharan Africa"~"SOM",
                        country=="Southern Sub-Saharan Africa"~"ZWE",
                        country=="Caribbean"~"CUB",
                        TRUE~code))
costs_inpatient <- costs_inpatient %>% filter(!is.na(code)) %>%
  select(mean, code)
c_inpatient <- as.list(costs_inpatient$mean)
names(c_inpatient) <- costs_inpatient$code
yr_inpatient <- 2010
cur_inpatient <- "INT"

#costs of TB treatment - from WHO data for now - may update
costs_tbtx <- read_excel("data-raw/Ines_tbfinance2020_6Sept21.xlsx", 
                      sheet="1.Dta ppc ds",
                      range="A6:J118")
#use argentina for chile and uruguay, DR for jamaica, marshall islands for micronesia, 
#kazakhstan for turkmenistan, tajikistan for uzbekistan
costs_tbtx <- as.data.table(costs_tbtx %>% filter(iso3 %in% c(country_info$code, "ARG")))
costs_tbtx <- costs_tbtx %>% mutate(iso3=if_else(iso3=="ARG", "CHL", iso3))
new_rows <- costs_tbtx %>% filter(iso3 %in% c("CHL", "DOM", "MHL", "KAZ", "TJK")) %>%
  mutate(iso3=case_when(iso3=="CHL"~"URY",
                        iso3=="DOM"~"JAM",
                        iso3=="MHL"~"FSM",
                        iso3=="KAZ"~"TKM",
                        iso3=="TJK"~"UZB"))
costs_tbtx <- bind_rows(costs_tbtx, new_rows)
costs_tbtx <- costs_tbtx %>% select(iso3, c_pp_dots) %>%
  rename("code"="iso3", "mean"="c_pp_dots")
c_tbtx <- as.list(costs_tbtx$mean)
names(c_tbtx) <- costs_tbtx$code
c_tbtx[["NIU"]] <- NA
c_tbtx[["TWN"]] <- NA

#update all costs to be in 2020 USD
#xpert, labs for toxicity, TB treatment - no updates needed
#contact investigation - apply inflation and scale by relative GNI pc
costs_contact <- sapply(names(costs_r), function(x)
  calc_inflation(costs_r[[x]][["c_contact"]],
                 costs_r[[x]][["yr_contact"]],
                 2020, 
                 wdi_data %>% filter(code=="USA")),
  simplify=F, USE.NAMES=T)
costs_contact <- sapply(unique(country_info$code), function(x)
  costs_contact[[country_info %>% filter(code==x) %>% pull(region)]]*
    gnipc[x]/
    gnipc[costs_r[[country_info %>% filter(code==x) %>% 
                     pull(region)]][["ctr_contact"]]],
  simplify=F, USE.NAMES=T)
#CXR - apply inflation and scale 1/4 by relative GNI pc
costs_xray <- sapply(names(costs_r), function(x)
  calc_inflation(costs_r[[x]][["c_xray"]],
                 costs_r[[x]][["yr_xray"]],
                 2020, 
                 wdi_data %>% filter(code=="USA")),
  simplify=F, USE.NAMES=T)
costs_xray <- sapply(unique(country_info$code), function(x)
  costs_xray[[country_info %>% filter(code==x) %>% pull(region)]]*
    (3+(gnipc[x]/
    gnipc[costs_r[[country_info %>% filter(code==x) %>% 
                     pull(region)]][["ctr_xray"]]]))/4,
  simplify=F, USE.NAMES=T)
#inpatient and outpatient - convert to LCU, apply inflation, convert to USD
costs_outpatient <- sapply(unique(country_info$code), function(x)
  c_outpatient[[x]]*wdi_data %>% filter(year==yr_outpatient & code==x) %>%
    pull(lcu_per_int),
  simplify=F, USE.NAMES=T)
costs_outpatient <- sapply(unique(country_info$code), function(x)
  calc_inflation(costs_outpatient[[x]], yr_outpatient, 2020,
                 wdi_data %>% filter(code==x)),
  simplify=F, USE.NAMES=T)
costs_outpatient <- sapply(unique(country_info$code), function(x)
  costs_outpatient[[x]]/wdi_data %>% filter(code==x & year==2020) %>% 
    pull(lcu_per_usd), simplify=F, USE.NAMES=T)
costs_inpatient <- sapply(unique(country_info$code), function(x)
  c_inpatient[[x]]*wdi_data %>% filter(year==yr_inpatient & code==x) %>%
    pull(lcu_per_int),
  simplify=F, USE.NAMES=T)
costs_inpatient <- sapply(unique(country_info$code), function(x)
  calc_inflation(costs_inpatient[[x]], yr_inpatient, 2020,
                 wdi_data %>% filter(code==x)),
  simplify=F, USE.NAMES=T)
costs_inpatient <- sapply(unique(country_info$code), function(x)
  costs_inpatient[[x]]/wdi_data %>% filter(code==x & year==2020) %>% 
    pull(lcu_per_usd), simplify=F, USE.NAMES=T)

#costs are unrealistically low for Liberia - weird currency conversion issues
#replace w/ next lowest-cost country (Burundi)
costs_outpatient[["LBR"]] <- costs_outpatient[["BDI"]]
costs_inpatient[["LBR"]] <- costs_inpatient[["BDI"]]
costs_outpatient[["TWN"]] <- NA
costs_outpatient[["NIU"]] <- NA
costs_inpatient[["TWN"]] <- NA
costs_inpatient[["NIU"]] <- NA

costs_use <- sapply(unique(country_info$code), function(x)
  data.frame("contact"=costs_contact[[x]],
                "inpatient"=costs_inpatient[[x]],
                "lab_tox"=costs_all[["c_labtox"]],
                "outpatient"=costs_outpatient[[x]],
                "tb_tx"=c_tbtx[[x]],
                "xpert"=costs_all[["c_xpert"]],
                "xray"=costs_xray[[x]]),
  simplify=F, USE.NAMES=T)
#venezuela's inpatient & outpatient costs are too high - bc of huge inflation
#leaving for now and will exclude from results

#pull costs
cost_params <- costs_use

#MORTALITY FROM WHO - no uncertainty
life_tables <- read.csv("data-raw/death_rates.csv", stringsAsFactors=F) %>% 
  filter(SpatialDimValueCode %in% country_info$code & Dim1=="Both sexes" & Period==2019) %>%
  rename("code"="SpatialDimValueCode", "year"="Period", "agecat"="Dim2", "rate"="Value") %>% 
  select(code, year, agecat, rate) %>% arrange(code, year, agecat)
#add rows for marshall islands - make it same as micronesia
life_tables <- bind_rows(life_tables,
                         life_tables %>% filter(code=="FSM") %>% mutate(code="MHL"))
life_tables <- life_tables %>% mutate(death_prob=case_when(agecat=="<1 year"~1-exp(-rate),
                                                           agecat=="1-4 years"~1-exp(-rate/4),
                                                           agecat=="85+ years"~1-exp(-rate/15),
                                                           TRUE~1-exp(-rate/5)))
life_tables <- life_tables %>% group_by(code) %>%
  mutate(p_child_die=unique(death_prob[agecat=="1-4 years"]),
         p_adol_die=mean(death_prob[agecat=="5-9 years"|agecat=="10-14 years"]),
         p_adult_die=mean(death_prob[agecat %in% c("15-19 years", "20-24 years", 
                                                   "25-29 years", "30-34 years",
                                                   "35-39 years", "40-44 years",
                                                   "45-49 years", "50-54 years",
                                                   "55-59 years")]))
life_tables <- unique(life_tables %>% select(code, p_child_die, p_adol_die, p_adult_die))
p_child_die <- life_tables$p_child_die
names(p_child_die) <- life_tables$code
p_adol_die <- life_tables$p_adol_die
names(p_adol_die) <- life_tables$code
p_adult_die <- life_tables$p_adult_die
names(p_adult_die) <- life_tables$code
if(FALSE) {
  p_child_die[["TWN"]] <- NA
  p_child_die[["NIU"]] <- NA
  p_adol_die[["TWN"]] <- NA
  p_adol_die[["NIU"]] <- NA
  p_adult_die[["TWN"]] <- NA
  p_adult_die[["NIU"]] <- NA
}


#LIFE EXPECTANCY FROM UN WPP - no uncertainty
#eventually switch so that mortality probs are also from UNWPP (so we don't use conflicting sources)
temp <- tempfile()
temp2 <- tempfile()
download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Abridged_Medium_2022-2100.zip",
                  temp)
unzip(zipfile=temp, exdir=temp2)
life_exp <- fread(file.path(temp2, "WPP2022_Life_Table_Abridged_Medium_2022-2100.csv"))
unlink(c(temp, temp2))
life_exp <- life_exp %>% 
  filter(Location %in% c(country_info$country, "United Republic of Tanzania",
                         "Venezuela (Bolivarian Republic of)",
                         "Micronesia (Fed. States of)", "Côte d'Ivoire",
                         #"Iran (Islamic Republic of)", "Niue", "China, Taiwan Province of China",
                         "Viet Nam", "Türkiye") &
           MidPeriod %in% c(2023, 2023.5) & Sex=="Total") %>%
  filter(Location!="Micronesia") #this is the region of micronesia and we want the country
life_exp <- life_exp %>% 
  mutate(country=case_when(Location=="United Republic of Tanzania"~"Tanzania",
                           Location=="Venezuela (Bolivarian Republic of)"~"Venezuela",
                           Location=="Micronesia (Fed. States of)"~"Micronesia",
                           Location=="Côte d'Ivoire"~"Cote d'Ivoire",
                           Location=="Niue"~"Niue Islands", 
                           Location=="Viet Nam"~"Vietnam",
                           Location=="Iran (Islamic Republic of)"~"Iran",
                           Location=="Türkiye"~"Turkey", 
                           Location=="China, Taiwan Province of China"~"Taiwan",
                           TRUE~Location))
life_exp <- left_join(life_exp, country_info %>% select(country, code),
                      by="country")
life_exp14 <- life_exp %>% filter(AgeGrp=="1-4") %>% pull(ex) #life expectancy at age 1
names(life_exp14) <- life_exp %>% filter(AgeGrp=="1-4") %>% pull(code)
life_exp59 <- life_exp %>% filter(AgeGrp=="5-9") %>% pull(ex) #life expectancy at age 4
names(life_exp59) <- life_exp %>% filter(AgeGrp=="5-9") %>% pull(code)
life_exp1014 <- life_exp %>% filter(AgeGrp=="10-14") %>% pull(ex) #life expectancy at age 10
names(life_exp1014) <- life_exp %>% filter(AgeGrp=="10-14") %>% pull(code)
life_exp1519 <- life_exp %>% filter(AgeGrp=="15-19") %>% pull(ex) #life expectancy at age 15
names(life_exp1519) <- life_exp %>% filter(AgeGrp=="15-19") %>% pull(code) 
life_exp_adult <- life_exp %>% filter(AgeGrp=="35-39") %>% pull(ex) #life expectancy at age 35
names(life_exp_adult) <- life_exp %>% filter(AgeGrp=="35-39") %>% pull(code)
#for children, use 1-4 
life_exp_child <- life_exp14
#for 5-14s, use mean of 5-9 and 10-14
life_exp_adol <- rowMeans(cbind(life_exp1014, life_exp59))


#NOTIFICATIONS BY AGE - TO CALCULATE AGE-SPECIFIC NOTIFICATION RATE/TX COVERAGE
notif <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=notifications")
notif_use <- notif %>% filter(iso3 %in% country_info$code & year %in% c(2018,2019,2020)) %>%
  mutate(notif_04=newrel_m04 + newrel_f04,
         notif_514=newrel_m514 + newrel_f514,
         notif_1524=newrel_m1524 + newrel_f1524,
         notif_15plus=newrel_m15plus + newrel_f15plus,
         notif_all=c_newinc) %>%
  select(iso3, year, starts_with("notif_")) %>% select(-notif_foreign)
#reshape to merge with inc
notif_use <- pivot_longer(notif_use, cols=starts_with("notif"),
                          names_to=c(".value", "age_group"),
                          names_sep="_")
notif_use <- notif_use %>% 
  mutate(age_group=case_when(age_group=="04"~"0-4",
                             age_group=="514"~"5-14",
                             age_group=="1524"~"15-24",
                             TRUE~age_group))
#Turkmenistan < 15 missing for all years, so use props from Kazakhstan
notif_use <- notif_use %>% group_by(year) %>%
  mutate(notif=if_else(iso3=="TKM" & age_group=="0-4", 
                       notif[iso3=="TKM" & age_group=="all"]*notif[iso3=="KAZ" & age_group=="0-4"]/
                         notif[iso3=="KAZ" & age_group=="all"], as.numeric(notif)),
         notif=if_else(iso3=="TKM" & age_group=="5-14", 
                       notif[iso3=="TKM" & age_group=="all"]*notif[iso3=="KAZ" & age_group=="5-14"]/
                         notif[iso3=="KAZ" & age_group=="all"], as.numeric(notif)))
#for children, assume 19.4% [85/439, add uncertainty for extrapolation to other countries] of notifications are improper (not true cases), so adjust notif rate down
#adult TB begins around 8-10 years, so use half the misdx ratio among ages 5-14
mis_dx <- 0.194 #http://perspectivesinmedicine.cshlp.org/content/4/9/a017855.full.pdf+html
notif_use <- notif_use %>%  mutate(notif=case_when(age_group=="0-4"~notif*(1-mis_dx),
                                                   age_group=="5-14"~notif*(1-mis_dx/2),
                                                   TRUE~notif))
notif_use <- as.data.table(notif_use)[, notif_all:=
                                        sum(notif[!(age_group %in% c("15-24", "all"))]),
                                          by=list(iso3, year)] 

#INCIDENCE BY AGE - TO CALCULATE AGE-SPECIFIC NOTIFICATION RATE/TX COVERAGE
inc <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=estimates_age_sex")
inc_est <- inc %>% filter(iso3 %in% country_info$code &
                            age_group %in% c("0-4", "5-14", "15plus", "15-24") &
                            risk_factor=="all" &
                            sex %in% c("m", "f"))
#total cases by age group
inc_est <- inc_est %>% group_by(iso3, age_group) %>%
  summarise(cases=sum(best))
inc_est_sum <- inc_est %>% group_by(iso3) %>% 
  summarise(cases=sum(cases[age_group!="15-24"])) %>%
  mutate(age_group="all")
inc_est <- bind_rows(inc_est, inc_est_sum)
#calculate % cases by age
inc_est <- inc_est %>% group_by(iso3) %>% mutate(case_prop=cases/cases[age_group=="all"])

#estimate notifications that are from contact investigation
contact_tpt <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=contact_tpt")
ipt_notif <- contact_tpt %>% filter(iso3 %in% country_info$code) %>%
  select(iso3, year, newinc_con04_prevtx,
         newinc_con5plus_prevtx, newinc_con_prevtx)
#IPT is by age, but doesn't mean only those age grp contacts were investigated
#cases ID'd via contact investigation should be evenly split across ages - assigned based on case %s
ipt_notif <- ipt_notif %>% 
  mutate(ipt=case_when(!is.na(newinc_con_prevtx)~newinc_con_prevtx,
                       (!is.na(newinc_con5plus_prevtx) & !is.na(newinc_con04_prevtx))~newinc_con04_prevtx+newinc_con5plus_prevtx,
                       !is.na(newinc_con04_prevtx)~newinc_con04_prevtx,
                       !is.na(newinc_con5plus_prevtx)~newinc_con5plus_prevtx,
                       TRUE~as.integer(0)))
ipt_notif <- ipt_notif %>% mutate(tb_ci=0.1*ipt) #assume 1:11 ratio active TB:not active TB among contacts
#assume between 0 and 100% of cases found from contact investigation would have been notified later anyway
notif_later <- 0.5
ipt_notif <- ipt_notif %>% group_by(iso3) %>%
  mutate(tb_ci=tb_ci*notif_later,
         tb_ci_mean=mean(tb_ci, na.rm=T))

#merge cases by age, notifications, and estimated CI notifications
cdr <- left_join(notif_use, ipt_notif,
                         by=c("iso3", "year"))
cdr <- as.data.table(left_join(cdr, inc_est,
                         by=c("iso3", "age_group")))

#only using 2020 data now
cdr <- cdr[(year==2020 & !(iso3 %in% c("BWA", "MDV")))|(year==2019 & iso3=="MDV")|(year==2018 & iso3=="BWA"), ]
#estimate notif from contact invest. by age based on cases by age
cdr <- cdr[, c("notif_ci","notif_ci_mu") :=
             list(tb_ci*case_prop,
                  tb_ci_mean*case_prop)]
#subtract estimated CI notif by age from notif by age
cdr <- cdr[, c("notif_adj", "notif_adj_mu") := 
             list(notif - notif_ci,
                  notif - notif_ci_mu)]
cdr <- cdr[notif_adj<=0, notif_adj:=1]
cdr <- cdr[notif_adj_mu<=0, notif_adj_mu:=1]
#calculate adjusted CDR
cdr <- cdr[, c("cdr", "cdr_mu") := 
             list(notif_adj/cases,
                  notif_adj_mu/cases)]
#use 2020 notif from contact estimate unless missing, then use mean
cdr <- cdr %>% mutate(cdr=if_else(is.na(cdr), cdr_mu, cdr))
#if 15-24 missing for all 3 years, use avg of 5-14 and 15 plus
cdr <- cdr %>% group_by(iso3) %>%
  mutate(cdr=if_else(is.na(cdr) & age_group=="15-24", 
                     (cdr[age_group=="5-14"]+cdr[age_group=="15plus"])/2,
                     cdr))
cdr <- as.data.table(cdr)

#convert to list
p_notif_child <- sapply(unique(cdr$iso3), function(x) 
  cdr[iso3==x & age_group=="0-4", cdr],
  simplify=F, USE.NAMES=T)
p_notif_adol <- sapply(unique(cdr$iso3), function(x) 
  cdr[iso3==x & age_group=="5-14", cdr],
  simplify=F, USE.NAMES=T)
p_notif_1524 <- sapply(unique(cdr$iso3), function(x) 
  cdr[iso3==x & age_group=="15-24", cdr],
  simplify=F, USE.NAMES=T)
p_notif_adult <- sapply(unique(cdr$iso3), function(x) 
  cdr[iso3==x & age_group=="15plus", cdr],
  simplify=F, USE.NAMES=T)

#TREATMENT OUTCOMES - TO MULTIPLY BY NOTIFICATION RATE TO CALCULATE % SUCCESSFULLY TREATED
outcomes <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=outcomes")
#2019 is latest year, if countries didn't report in 2019 they didn't report in preceeding yrs either
outcomes_use <- outcomes %>% filter(iso3 %in% country_info$code & year %in% c(2019)) %>%
  mutate(success_014=newrel_014_succ/newrel_014_coh,
         success_adult=(newrel_succ-newrel_014_succ)/(newrel_coh-newrel_014_coh),
         success_plhiv=tbhiv_succ/tbhiv_coh) %>%
  select(iso3, year, starts_with("success"))
p_success_child <- sapply(unique(outcomes_use$iso3), function(x)
  outcomes_use %>% filter(iso3==x) %>% pull(success_014),
  simplify=F, USE.NAMES=T)
p_success_adult <- sapply(unique(outcomes_use$iso3), function(x)
  outcomes_use %>% filter(iso3==x) %>% pull(success_adult),
  simplify=F, USE.NAMES=T)
p_success_plhiv <- sapply(unique(outcomes_use$iso3), function(x)
  outcomes_use %>% filter(iso3==x) %>% pull(success_plhiv),
  simplify=F, USE.NAMES=T)
missing_codes <- c(outcomes_use %>% filter(is.na(success_adult)) %>% pull(iso3), "BLR", "BTN", "GAB", "MDV", "FSM")
missing_codes_plhiv <- c(outcomes_use %>% filter(is.na(success_plhiv)) %>% pull(iso3), "MHL", "MDV", "FSM", "NIU", "BTN")
#sample from all countries combined for countries w/ no data (or v small countries with 1 notif per year)
p_success_child_combine <- mean(unlist(p_success_child), na.rm=T)
p_success_child[missing_codes] <- NULL
new_list <- sapply(missing_codes, function(x)
  p_success_child_combine, simplify=F, USE.NAMES=T)
p_success_child <- c(p_success_child, new_list)

p_success_adult_combine <- mean(unlist(p_success_adult), na.rm=T)
p_success_adult[missing_codes] <- NULL
new_list <- sapply(missing_codes, function(x)
  p_success_adult_combine, simplify=F, USE.NAMES=T)
p_success_adult <- c(p_success_adult, new_list)

p_success_plhiv_combine <- mean(unlist(p_success_plhiv), na.rm=T)
p_success_plhiv[missing_codes_plhiv] <- NULL
new_list <- sapply(missing_codes_plhiv, function(x)
  p_success_plhiv_combine, simplify=F, USE.NAMES=T)
p_success_plhiv <- c(p_success_plhiv, new_list)

#also save % of cases by age
#used in proxy of secondary impact analysis only
cases_prop <- inc_est %>% select(iso3, age_group, case_prop) %>%
  filter(age_group %in% c("0-4", "5-14", "15plus"))

#save everything to Rda file
save(cost_params, ltbi_params, country_info, wdi_data,
     p_child_die, p_adol_die, p_adult_die, 
     p_notif_child, p_notif_adol, p_notif_1524, p_notif_adult,
     p_success_child, p_success_plhiv, p_success_adult, #for use in PLHIV model only, to adjust p_notif_adult
     life_exp_child, life_exp_adol, life_exp_adult, 
     cases_prop, file="shiny/country_params.Rda")