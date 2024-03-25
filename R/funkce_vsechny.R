#' Křížení - excelová tabulka
#' @encoding UTF-8
#'
#'@description Funkce, která vytvoří tebulky s křížením libovolného počtu proměnných na vážených datech a vrátí excelovou tabulku.
#'
#' @param df Vstupní dataset. Sloupce musí obsahovat labely.
#' @param vektor_radek Výčet proměnných, přes které budou ve finální tabulce ve
#'   sloupci ve formátu: "jmeno sloupce" nebo: c("jméno sloupce 1","jméno
#'   sloupce 2") - pokud je proměnných více. Sloupce použité, zde již nemohou
#'   být uvedeny v druhém parametru (před křížením je v takovém případě třeba
#'   vytvořit kopii sloupce s jiným názvem).
#' @param vektor_sloupec Výčet proměnných, přes které budeme křížit. Ve finální
#'   tabulce budou v horním řádku. Vložit ve formátu: "jmeno sloupce" nebo:
#'   c("jméno sloupce 1","jméno sloupce 2") - pokud je proměnných více
#' @param w Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný
#'   w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např.
#'   "VÁHA".
#' @param jmeno_souboru Pokud chceme, aby se sloupec jmenoval jinak než
#'   "křížení", vložíme název. Např.: "Q1 x Q8".
#'
#' @return
#' @export
#'
#' @examples f_krizeni(data,c("mat","inte"),c("poh","vzd","age"), w = "v", jmeno_souboru = "test")
#'
#'
f_krizeni <- function (df, vektor_radek, vektor_sloupec, w = "w", jmeno_souboru = "krizeni") {

  df <- df %>% rename(w = !!as.symbol(w))

  df <-  df %>% mutate_at(vars(-w),to_factor)

  #------------------------------ Zpracování proměnných pro řádky-----------------------------------------------------------------


  long_data_base <- pivot_longer(df,all_of(vektor_radek),names_to = "otazka",values_to = "odpovedi")

  # Celkový počet odpovědí na otázku pro všechny zvolené otázky

  vahy_otazek <- long_data_base %>% filter(odpovedi != is.na(odpovedi) & odpovedi != "sys_filtered_off" & odpovedi != "sys_empty") %>% group_by(otazka)  %>% summarise(vaha_otazka = sum(w)) #round

  # Počet odpovědí na jednotlivé varianty odpovědí pro všechny zvolené otázky

  long_data <- long_data_base %>% filter(odpovedi != is.na(odpovedi) & odpovedi != "sys_filtered_off" & odpovedi != "sys_empty") %>% group_by(otazka,odpovedi) %>% summarise(w = sum(w))

  #Tvorba df se zněním otázek

  z <-as.data.frame(unlist(sapply(df, var_label)))
  z <- rownames_to_column(z, var = "otazka")
  names(z)[2] <- "Zneni"


  # Připojení znění otázek k tabulce s variantami odpovědí

  long_data <- long_data %>% left_join(z, by = "otazka")

  # long_data <- long_data %>% mutate(r = w) #round

  # Připojení celkové váhy otázek k tabulce s variantami odpovědí a výpočet procent

  long_data <- long_data %>% left_join(vahy_otazek, by = "otazka")

  long_data <- long_data %>% mutate(procenta =round(w/vaha_otazka*100,1))

  long_data <<- long_data

  # tvorba df pro absolutní počty

  abs_count <- tibble(otazka = vektor_radek)

  #---------------------------------------------Zpracování proměnných pro sloupce----------------------------------------------------------------

  #Tvorba dlouhých dat a součty pro jednotlivé varianty odpovědí

  long_data_columns <- pivot_longer(df,all_of(vektor_sloupec),names_to = "otazka",values_to = "odpovedi")

  long_data_columns2 <- long_data_columns %>% filter(odpovedi != is.na(odpovedi) & odpovedi != "sys_filtered_off" & odpovedi != "sys_empty") %>% group_by(otazka,odpovedi) %>% summarise(r = sum(w)) #round


  long_data_columns2 <<- long_data_columns2

  #příprava tabulky pro x_square
  x_square_base <- long_data %>% select(otazka,odpovedi)


  # příprava tabulky pro počet řádek
  pocet_radek <- tibble(moznosti =NULL, vaha_skupina = NULL, otazka = NULL)


  #-------------------------------------------------Křížení------------------------------------------------------------------

  for (name in vektor_sloupec) {


    #počet odpovědí pro jednotlivé křížené možnosti

    krizeni <- long_data_base %>% filter(odpovedi != is.na(odpovedi)& odpovedi != "sys_filtered_off" & odpovedi != "sys_empty") %>% group_by(otazka,odpovedi,!!as.symbol(name)) %>% summarise(r = sum(w)) %>% filter (!!as.symbol(name) != is.na(!!as.symbol(name))) #round

    #počet odpověedí pro jednotlivé křížené otázky

    vahy_skupiny <- long_data_base %>% filter(odpovedi != is.na(odpovedi) & odpovedi != "sys_filtered_off" & odpovedi != "sys_empty") %>% group_by(otazka,!!as.symbol(name)) %>% summarise(vaha_skupina = sum(w)) %>% filter (!!as.symbol(name) != is.na(!!as.symbol(name))) #round

    vahy_skupiny <- vahy_skupiny %>% ungroup ()

    #propojení počtu odpovědí pro možnosti a celou otázku v křížení a výpočet procent

    krizeni <- krizeni %>% left_join(vahy_skupiny, by = c("otazka" = "otazka", setNames(name,name) ))

    krizeni <- krizeni  %>% mutate(procenta = round(r/vaha_skupina*100,1))

    # krizeni <<-krizeni




    #----------------------------x_square-------------------------------------------------------------------------------------------------

    # četnost konkrétních variant odpovědí (nekřížená)

    x_square <- krizeni %>% group_by(otazka,odpovedi) %>% summarize(popularita_odpovedi_w = round(sum(r))) #round

    # připojení četností konkrétních variant odpovědí ke křížené tabulce

    x_square <- x_square %>% right_join(select(krizeni,otazka,odpovedi,r,vaha_skupina,!!as.symbol(name)), by = c("otazka" = "otazka", "odpovedi" = "odpovedi"))

    #váha skupiny

    x_square <- x_square %>% mutate(vaha_skupina = round(vaha_skupina))

    #váha celé otázky

    vaha_filter_otazka <- krizeni %>% group_by(otazka) %>% summarize(vaha_otazka = round(sum(r)))

    x_square <- x_square %>% right_join(vaha_filter_otazka, by = "otazka")

    #výpočet adjustovaných standardizovaných reziduií

    x_square <- x_square %>% mutate(std_rez = (r-(popularita_odpovedi_w*vaha_skupina)/vaha_otazka)/(sqrt(popularita_odpovedi_w*vaha_skupina*(1-popularita_odpovedi_w/vaha_otazka)*(1-vaha_skupina/vaha_otazka)/vaha_otazka)))

    # pivot a uspořádání tabulky dle pořadí vektor_sloupec

    x_square_wide <- x_square %>% select(-popularita_odpovedi_w,-r,-vaha_skupina,-vaha_otazka) %>% arrange(!!as.symbol(name)) %>% pivot_wider(names_from = name, names_prefix = name, values_from = std_rez, values_fill = 0)

    #připojení reziduí k připravené tabulce

    x_square_base <- x_square_base %>% full_join(x_square_wide,c("otazka" = "otazka", "odpovedi" = "odpovedi"))



    #---------------řazení základní tabulky----------------------------------------------------------------------

    krizeni_radek <- krizeni %>% ungroup() %>% select(.,!!as.symbol(name), vaha_skupina, otazka)

    krizeni_radek <- unique(krizeni_radek)

    krizeni_radek <- krizeni_radek %>% mutate(vaha_skupina = round(vaha_skupina))

    krizeni_radek <- krizeni_radek %>% rename(moznosti = !!as.symbol(name)) %>% arrange(., moznosti)

    pocet_radek <- pocet_radek %>% rbind(krizeni_radek) #celkový počet napříč cykly

    #aboslutní počty
    krizeni_count <- krizeni %>% arrange(!!as.symbol(name)) %>% pivot_wider(names_from = name,names_prefix = sprintf("%s_", name), names_glue = , values_from = r, values_fill = 0) %>% group_by(otazka) %>% summarise(across(starts_with(name),~round(sum(.x))))

    abs_count <- abs_count %>% left_join(krizeni_count, by = "otazka")


    krizeni <- krizeni %>% select(-r,-vaha_skupina)

    # pivot (a tvorba jmen sloupců) a uspořádání tabulky s procenty dle pořadí vektor_sloupec

    krizeni_wide <- krizeni %>% arrange(!!as.symbol(name)) %>% pivot_wider(names_from = name,names_prefix = sprintf("%s_", name), names_glue = , values_from = procenta, values_fill = 0)

    #připojení tabulky s procenty pro danou otázku k základu

    long_data <-long_data %>% full_join(krizeni_wide,c("otazka" = "otazka", "odpovedi" = "odpovedi"))


  }



  #řazení otázek

  pocet_opakovani <- df %>% select(all_of(vektor_radek)) %>% sapply(function (x) {length(levels(x))})

  repeated_vector <- as_tibble(rep(vektor_radek,pocet_opakovani))

  odpovedi2 <- df %>% select(all_of(vektor_radek)) %>% sapply(function (x) {levels(x)}) %>% unlist() %>% as.vector()

  repeated_vector2 <- repeated_vector %>% cbind(odpovedi2)

  repeated_vector2 <- repeated_vector2 %>% rename(otazka = value, odpovedi = odpovedi2)


  temp <<- repeated_vector2 %>% left_join(long_data, by= c("otazka" = "otazka", "odpovedi"="odpovedi"))

  temp2 <<- repeated_vector2 %>% left_join(x_square_base, by= c("otazka" = "otazka", "odpovedi"="odpovedi"))

  temp <<- select(temp, c(otazka,Zneni,odpovedi, vaha_otazka, w, everything()))


  # odstranění opakování otázky a její váhy v řádcích + doplnění řádku s počty případů

  cur <- ""
  ind0 <-0
  add <-0

  for (a in temp[["otazka"]]) {
    if (a == cur) {
      temp[[ind0+add,"Zneni"]] <<- NA
      temp[[ind0+add,"vaha_otazka"]] <<- NA
    }

    else {
      # temp <<- insertRows(temp, (1 + ind0 + add + nrow(filter(temp,otazka== a))), new = c(a,rep(NA,5), filter(pocet_radek, pocet_radek$otazka == a)$vaha_skupina))

      temp <<- insertRows(temp, (1 + ind0 + add + nrow(filter(temp,otazka== a))), new = c(a,rep(NA,5), select(filter(abs_count, abs_count$otazka == a),-otazka)))

      temp2 <<- insertRows(temp2, (1 + ind0 + add +nrow(filter(temp2,otazka== a))), new = c(a,rep(NA,5+length(krizeni_radek$vaha_skupina))))

      add <-add+1
    }


    cur <- a
    ind0 <-ind0+1

  }



  temp <- temp[1:nrow(temp)-1,]
  temp2 <- temp2[1:nrow(temp2)-1,]


  temp <- temp %>% mutate(across(c(-otazka,-odpovedi,-Zneni),as.numeric))


  temp2 <- temp2 %>% mutate(across(c(-otazka,-odpovedi),as.numeric))

  temp <- temp %>% mutate(w = round(w))


  #Odstranění řádku s sys_filtered_off

  temp <- temp %>% filter(odpovedi !="sys_filtered_off"| is.na(odpovedi))

  temp2 <- temp2 %>% filter(odpovedi !="sys_filtered_off"| is.na(odpovedi))

  temp <<- temp
  temp2 <<- temp2

  #přejmenování prvního řádku

  temp <- temp  %>% rename_with(~str_replace_all(.,"_", " "), .cols = -c(otazka, Zneni,odpovedi,vaha_otazka,w))

  temp <- temp  %>% rename(`kód otázky` = otazka,
                           `znění` = Zneni,
                           `varianty odpovědí`= odpovedi,
                           `počet dotázaných` = vaha_otazka,
                           `počet odpovědí u varianty` = w)



  #určení jména souboru

  file <- paste0(jmeno_souboru,".xlsx")

  wb <- createWorkbook()
  sheet <- createSheet(wb, "Křížení")
  addDataFrame(as.data.frame(temp), sheet)



  # vyříznutí okna do excelové tabulky, ke které se pak budu vztahovat
  cb <- CellBlock(sheet, 2, 8, nrow(temp), ncol(temp) - 6, create = FALSE)


  #podbarvení dle velikosti reziduí

  x <- temp2 %>% ungroup() %>% select(-otazka, -odpovedi)
  x <- sapply(x, as.numeric)

  ind1 <- which(x > 1.960, arr.ind=T)
  CB.setFill(cb, Fill(foregroundColor = "#AFCED5", backgroundColor = "#AFCED5"), ind1[,1], ind1[,2])
  ind2 <- which(x > 3.291, arr.ind=T)
  CB.setFill(cb, Fill(foregroundColor = "#699FAC", backgroundColor = "#699FAC"), ind2[,1], ind2[,2])
  ind3 <- which(x < -1.960, arr.ind=T)
  CB.setFill(cb, Fill(foregroundColor = "#E6B6B0", backgroundColor = "#E6B6B0"), ind3[,1], ind3[,2])
  ind4 <- which(x < -3.291, arr.ind=T)
  CB.setFill(cb, Fill(foregroundColor = "#CD7471", backgroundColor = "#CD7471"), ind4[,1], ind4[,2])


  cb <- CellBlock(sheet, 2, 1, nrow(temp), (ncol(temp)+1), create = FALSE)

  #tlusté čáry
  actual <- ""
  ind <- 1

  for (value in temp[["kód otázky"]]) {
    if (value != actual) {
      actual <- value
      CB.setBorder(cb, Border(color="black", position="TOP", pen="BORDER_THICK"), ind, 1:ncol( temp)+1)

    }
    ind <- ind + 1
  }

  CB.setBorder(cb, Border(color="black", position="BOTTOM", pen="BORDER_THICK"), nrow(temp), 1:ncol(temp)+1)


  actual2 <- ""
  ind2 <- 1

  for (colname in colnames(temp)) {
    str_n <- str_extract(as.character(colname),"[a-zA-Z]*[0-9]*")
    if (str_n != actual2) {
      actual2 <- str_n
      CB.setBorder(cb, Border(color="black", position="RIGHT", pen="BORDER_THICK"), 1:nrow(temp),ind2)

    }
    ind2 <- ind2 + 1
  }

  CB.setBorder(cb, Border(color="black", position="RIGHT", pen="BORDER_THICK"), 1:nrow(temp),ncol(temp)+1)

  saveWorkbook(wb, file)
}


#' EU Segmentace index
#' @encoding UTF-8
#' @description Funkce, která vrátí kopii původního datasetu doplněnou o sloupce NESPCR, NESPEU a EU_SEGMENTY. Tyto sloupce obsahují indexy pro EU segmentaci.
#'
#'
#'
#' @param df Vstupní dataset. Sloupce musí obsahovat labely. Sloupce, které jsou k výpočtu třeba nesmí být konvertovány na faktory.
#'
#' @return
#' @export
#'
#' @examples data2 <- f_euro(data) #šipkou vytvoříme kopii dat s doplněným indexem
f_euro <- function (df) {

  # a0 <- df[,str_detect(sapply(df,var_label),"Zamyslíte-li se nad vývojem u nás v posledních více než třiceti letech")] %>% colnames() #12
  # a1 <- df[,str_detect(sapply(df,var_label),"o současné ekonomické situaci v České republice?")] %>% colnames() #13
  # a2 <- df[,str_detect(sapply(df,var_label),"Myslíte si, že většině lidí se dá důvěřovat?")] %>% colnames()#19
  # a3 <- df[,str_detect(sapply(df,var_label),"Jsou pro Vás osobně důležité volby do Poslanecké sněmovny Parlamentu ČR")] %>% colnames() #20
  # a4 <- df[,str_detect(sapply(df,var_label),"s naším členstvím v Evropské unii")] %>% colnames() #34
  # a5 <- df[,str_detect(sapply(df,var_label),"Cítíte Vy osobně sounáležitost s Evropou, cítíte se být Evropanem")] %>% colnames() #35
  # a6 <- df[,str_detect(sapply(df,var_label),"Myslíte si, že naše národní zájmy jsou v rozporu se zájmy Evropské unie?")] %>% colnames() #36
  # a7 <- df[,str_detect(sapply(df,var_label),"Podporujete zavedení eura jako měny v České republice?")] %>% colnames() #37
  # a8 <- df[,str_detect(sapply(df,var_label),"jasným stoupencem evropské integrace a Evropské unie, i když nesouhlasíte s některými konkrétními opatřeními EU")] %>% colnames() #38
  # a9 <- df[,str_detect(sapply(df,var_label),"referendum o vstupu do EU, hlasoval")] %>% colnames() #39
  # a10 <- df[,str_detect(sapply(df,var_label),"se situace v České republice vyvíjí všeobecně správným nebo nesprávným směrem")] %>% colnames() #4

  a0 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"Zamyslíte-li se nad vývojem u nás v posledních více než třiceti letech")) ] #12
  a1 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"o současné ekonomické situaci v České republice?") )] #13
  a2 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"Myslíte si, že většině lidí se dá důvěřovat?") )]#19
  a3 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"Jsou pro Vás osobně důležité volby do Poslanecké sněmovny Parlamentu ČR"))] #20
  a4 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"s naším členstvím v Evropské unii"))] #34
  a5 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"Cítíte Vy osobně sounáležitost s Evropou, cítíte se být Evropanem"))] #35
  a6 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"Myslíte si, že naše národní zájmy jsou v rozporu se zájmy Evropské unie?") )]#36
  a7 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"Podporujete zavedení eura jako měny v České republice?") )] #37
  a8 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"jasným stoupencem evropské integrace a Evropské unie, i když nesouhlasíte s některými konkrétními opatřeními EU"))] #38
  a9 <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"referendum o vstupu do EU, hlasoval") ) ]#39
  a10 <- colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"se situace v České republice vyvíjí všeobecně správným nebo nesprávným směrem"))] #4


  df <- df %>% mutate(NESPCR = 0)
  df <- df %>% mutate(NESPCR = ifelse((!!as.symbol(a0)==4 | !!as.symbol(a0)== 5),NESPCR+1,NESPCR),
                      NESPCR = ifelse((!!as.symbol(a10)== 2),NESPCR+1,NESPCR),
                      NESPCR = ifelse((!!as.symbol(a1) == 3| !!as.symbol(a1) == 4),NESPCR+1,NESPCR),
                      NESPCR = ifelse((!!as.symbol(a2) == 3| !!as.symbol(a2) == 4),NESPCR+1,NESPCR),
                      NESPCR = ifelse((!!as.symbol(a3) == 3| !!as.symbol(a3) ==4),NESPCR+1,NESPCR))

  df <- df %>% mutate(NESPCR = case_when(NESPCR==5 ~ 5,
                                         NESPCR==4 ~ 5,
                                         NESPCR==3 ~ 4,
                                         NESPCR==2 ~ 3,
                                         NESPCR==1 ~ 2,
                                         NESPCR==0 ~ 1))


  df <- df %>% add_value_labels(NESPCR = c("zcela otevření" =1,
                                           "celkem otevření"=2,
                                           "vlažní"=3,
                                           "pochybující"=4,
                                           "uzavření"=5))


  df <- df %>% mutate(ra6= 5 - !!as.symbol(a6))


  df <- df %>% mutate(NESPEU = (!!as.symbol(a4)+ !!as.symbol(a5)+ra6+!!as.symbol(a7)+!!as.symbol(a8)+!!as.symbol(a9))/6)


  df <- df %>% mutate(NESPEU = case_when(NESPEU <=1.7 ~1,
                                         NESPEU >1.7 & NESPEU <=2.2 ~2,
                                         NESPEU >2.2 & NESPEU<=2.9 ~3,
                                         NESPEU >2.9 & NESPEU<=3.4 ~4,
                                         NESPEU >3.4 ~5))



  df <- df %>% add_value_labels(NESPEU = c("silní příznivci EU" =1,
                                           "umírnění příznivci EU"=2,
                                           "neutrální"=3,
                                           "umírnění odpůrci EU"=4,
                                           "silní odpůrci EU"=5))


  df <- df %>% mutate(EU_SEGMENTY = case_when((NESPEU==1) ~ 1,
                                              (NESPEU==2 & NESPCR ==1) ~ 1,
                                              (NESPEU==2 & (NESPCR ==2|NESPCR ==3|NESPCR ==4)) ~ 2,
                                              (NESPEU==2 & NESPCR ==5) ~ 4,
                                              (NESPEU==3 & (NESPCR ==1|NESPCR ==2)) ~ 2,
                                              (NESPEU==3 & NESPCR ==3) ~ 3,
                                              (NESPEU==3 &  (NESPCR ==4|NESPCR ==5)) ~ 4,
                                              (NESPEU==4 &  (NESPCR ==1|NESPCR ==2)) ~ 3,
                                              (NESPEU==4 &  NESPCR ==3) ~ 4,
                                              (NESPEU==4 &  (NESPCR ==4|NESPCR ==5)) ~ 5,
                                              (NESPEU==5 &  (NESPCR ==1|NESPCR ==2)) ~ 6,
                                              (NESPEU==5 &  (NESPCR ==3|NESPCR ==4|NESPCR ==5)) ~ 5))

  df <- df %>% add_value_labels(EU_SEGMENTY = c("Přesvědčení" =1,
                                                "Vlažní"=2,
                                                "Kritici"=3,
                                                "Nedůvěřiví"=4,
                                                "Odpůrci"=5,
                                                "Aktivní odpůrci"=6))

  df <- df %>% select(-ra6)

}



#' KPMG index
#' @encoding UTF-8
#' @description Funkce, která vrátí kopii původního datasetu doplněnou o sloupce inow ,ifut,iall, I_KPMG (číslo) a indexred (čtyři skupiny) s hodnotami KPMG indexu. Zároveň do konzole vytiskne celkovou hodnotu indexu. Volitelně též vytvoří excelovou tabulku s váženými průměry indexu pro zvolené skupiny a s názvem "KPMG skupiny.xlsx".
#'
#' @param df Vstupní dataset. Sloupce musí obsahovat labely. Sloupce, které jsou k výpočtu třeba nesmí být konvertovány na faktory.
#' @param tab Volitelný argument, pokud zůstane prázdný, funkce pouze dopočítá indexy. Pokud vložíme jména sloupců (např. demografické proměnné) ve formátu: "jmeno sloupce" nebo: c("jméno sloupce 1","jméno sloupce 2") vygeneruje se zároveň excelová tabulka s průměry indexu I_KPMG v jednotlivých skupinách.
#' @param w Volitený argument, smysluplný pouze při tvorbě excelové tabulky. Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např."VÁHA".
#'
#' @return
#' @export
#'
#' @examples dataX <- f_KPMG(data,tab=c("poh","vzdelani","age"), w = "VAHA")
f_KPMG <- function (df, tab = NULL, w = "w") {

  df <- df %>% rename(w = !!as.symbol(w))

  inow <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"Jak vycházíte se současným příjmem, který máte v domácnosti")) ]
  ipast <- colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"finanční situace Vaší domácnosti se v porovnání se situací před 12 měsíci")) ]
  ifut <-  colnames(df)[which(str_detect(unlist(sapply(df,var_label)),"že se finanční situace Vaší domácnosti za budoucích 12 měsíců")) ]

  df <- df %>% mutate(ipast = case_when(!!as.symbol(ipast) == 1 ~ 1,
                                        !!as.symbol(ipast) == 2 ~ 0.5,
                                        !!as.symbol(ipast) == 3 ~ 0,
                                        !!as.symbol(ipast) == 4 ~ -0.5,
                                        !!as.symbol(ipast) == 5 ~ -1))

  df <- df %>% mutate(ifut = case_when(!!as.symbol(ifut) == 1 ~ 1,
                                       !!as.symbol(ifut) == 2 ~ 0.5,
                                       !!as.symbol(ifut) == 3 ~ 0,
                                       !!as.symbol(ifut) == 4 ~ -0.5,
                                       !!as.symbol(ifut) == 5 ~ -1))

  df <- df %>% mutate(inow = case_when(!!as.symbol(inow) == 1 ~ -1,
                                       !!as.symbol(inow) == 2 ~ -1,
                                       !!as.symbol(inow) == 3 ~ -0.75,
                                       !!as.symbol(inow) == 4 ~ 0.75,
                                       !!as.symbol(inow) == 5 ~ 1,
                                       !!as.symbol(inow) == 6 ~ 1))

  df <- df %>% mutate(iall   =    apply(select(df,ipast, inow, ifut), 1, function (x) {mean(x, na.rm = TRUE)}))



  df <- df %>% mutate (I_KPMG =        (0.189975 +iall) / 1.189975 *100)



  df <- df %>% mutate(indexred = case_when(I_KPMG < -19                ~ "4",
                                           I_KPMG >= -19 & I_KPMG < 16 ~ "3",
                                           I_KPMG >=16 & I_KPMG < 50   ~ "2",
                                           I_KPMG >50                  ~ "1"))

  df <- df %>% add_value_labels(indexred = c("velmi dobře" ="1",
                                             "dobře"="2",
                                             "špatně"="3",
                                             "velmi špatně"="4"))

  #tiskne celkový průměr
  print(df %>% summarize(Celkovy_index = sum(w*I_KPMG)/sum(w)))




  #vytváří excel pro skupiny

  if (!is.null(tab)) {

    skupiny <- tibble(promenne = NULL, prumer = NULL)

    for( i in tab) {
      temp <- df %>% group_by(!!as.symbol(i)) %>% summarise(prumer = sum(w*I_KPMG)/sum(w))
      colnames(temp)[1] <- "promenne"
      temp <- temp %>% mutate(promenne = to_factor(promenne, levels = "labels"))
      skupiny <- rbind(skupiny,temp)
    }
    write.xlsx(skupiny,"KPMG skupiny.xlsx")
  }

  df

}


#' Váhy
#'@encoding UTF-8
#' @description Vysoce experimentální funkce :-). Vrátí kopii původního datasetu doplněnou o navážená data (název sloupce s váhou - w).
#' Data budou vážena na proměnné uvedené v argumentu names v daném pořadí. Funkce vyžaduje jako další vstup excelovou tabulku s názvem "vahy.xlsx"
#' uloženou ve stejné složce jako projekt na kterém pracujete. Tato tabulka nakrmí funkci populačním rozložením pro proměnné na které chceme vážit nebo pro proměnné,
#' pro které chceme efekt vah sledovat. Tabulka umožňuje rovněž vložení arbitrárních vah (např. pro nevoliče). Funkce rovněž umožňuje vytvořit excelovou tabulku s přehledem efektu vah na data (Vahy přehled.xlsx).
#' @param df Vstupní dataset.
#' @param names Výčet proměnných, na které chceme vážit. Váhy budou použity v pořadí, jaké uvedeme. Vložit ve formátu: "jmeno sloupce" nebo c("jméno sloupce 1","jméno sloupce 2") - pokud je proměnných více. Název sloupce se musí shodovat s názvem sloupce v doprovodné excelové tabulce.
#' @param w Volitelný argument. Jsou-li data již vážena a chceme je dále vážit, uvedeme zde název výchozí váhy např.: "v".
#' @param tab Chceme-li vytvořit excelovou tabulku s přehledem rozložení libovolných proměnných vložíme názvy těchto proměnných ve formátu: "jmeno sloupce" nebo: c("jméno sloupce 1","jméno sloupce 2",...)
#'
#' @return
#' @export
#'
#' @examples dataX <- vahy(data3,names =c("el21","vol21"), tab = c("poh","el21","vol21"))
vahy <- function (df,names,w = 1, tab = NULL) {

  if (str_detect(w,"^[0-9]*.?,?[0-9]*$")) df <- df %>% mutate(w = as.numeric(w))
  else  df <- df %>% mutate(w = eval(as.symbol(w)))

  guide <- read.xlsx("vahy.xlsx",1, colIndex = c(1:9))

  guide <- guide %>% filter(!is.na(promenna))

  guide <- guide %>% mutate(across(podil_populace:kontrola,as.numeric))


  for(name in names) {

    # df1 <- df %>% filter(!!as.symbol(name) != guide[guide$promenna == name,][!is.na(guide$arbitrarni_vaha),]$skupina)

    platne <- guide[guide$promenna == name & is.na(guide$arbitrarni_vaha),]$skupina

    df1 <- df %>% filter(!!as.symbol(name) %in% platne)

    delitel <- sum(df1$w)


    temp <- df %>% group_by(!!as.symbol(name)) %>% summarise(pocet = sum(w))

    temp <- temp %>% mutate(con = as.character(!!as.symbol(name)))



    guide_temp <- guide %>% mutate(skupina = as.character(skupina)) %>% filter(promenna == name)

    temp <- temp %>% left_join(guide_temp, by = c("con"="skupina"))

    temp <- temp %>% mutate(vaha_temp = if_else(!is.na(arbitrarni_vaha),arbitrarni_vaha, podil_populace/(pocet/delitel)))

    df <- df %>% mutate(con = as.character(!!as.symbol(name)))

    df <- df %>% left_join(select(temp, con, vaha_temp), by = "con")


    df <- df %>% mutate(w = w*vaha_temp)


    korekce <- nrow(df)/sum(df$w)

    df <- df %>% mutate(w = w*korekce)

    df <- df %>% select(-con,-vaha_temp)

  }

  #vytváří excel pro skupiny

  if (!is.null(tab)) {

    skupiny <- tibble(skupina = NULL, podil_s_vahou = NULL,promenna = NULL,nazev = NULL)

    celek <- sum(df$w)


    for( i in tab) {

      temp <- df %>% group_by(!!as.symbol(i)) %>% summarise(podil_s_vahou = sum(w)/celek)
      colnames(temp)[1] <- "skupina"
      temp <- temp %>% mutate(promenna = i,
                              nazev = to_factor(skupina, levels = "labels"),
                              skupina = as.character(skupina))

      skupiny <- rbind(skupiny,temp)
    }


    guide <- guide %>% mutate(skupina = as.character(skupina))

    skupiny <- skupiny %>% left_join(select(guide,promenna,skupina,pocet_ve_vzorku,podil_ve_vzorku,podil_populace,arbitrarni_vaha,kontrola), by= c("promenna"="promenna","skupina"="skupina"))

    skupiny <- skupiny %>% mutate(rozdil = podil_s_vahou - podil_populace)

    skupiny <- select(skupiny, promenna, nazev, skupina, pocet_ve_vzorku,podil_ve_vzorku,podil_populace,podil_s_vahou,rozdil,arbitrarni_vaha,kontrola)


    write.xlsx(skupiny,"Vahy přehled.xlsx")
  }

  print(summary(df$w))

  df

}


#' Přehled rozložení vzorku
#'@encoding UTF-8
#' @description Vytvoří excelovou tabulku s přehledem rozložení zvolených proměnných v datech. Tento soubor je možné využít pro úsedek o tom, na které proměnné vážit a po doplnění (a přejmenování na "vahy.xlsx") rovněž jako podklad pro vložení cílových hodnot na které budeme vážit funkcí "vahy". Vytvořený soubor nalezneme pdo názvem "rozlozeni_vzorku.xlsx". Zároveň vrací identický dataframe rozlozeni_vzorku.
#' @param df Vstupní dataset, u kterého chceme zjistit rozložení proměnných.
#' @param names Výčet proměnných pro které chceme rozložení zjistit. Vložit ve formátu: "jmeno sloupce" nebo c("jméno sloupce 1","jméno sloupce 2"), je-li proměnných více.
#' @param w Volitelný argument. Jsou-li data již vážena a zajímají-li nás již vážené frekvence (např. před dalším vážením).
#'
#' @return
#' @export
#'
#' @examples vahy_prehled(data,names =c("el21","vol21","vzd","poh"))
vahy_prehled <- function (df,names,w = 1, jmeno_souboru = "rozlozeni_vzorku.xlsx") {

  df <- df %>% mutate(w = as.numeric(w))

  tib <- tibble(promenna = NULL, nazev = NULL, skupina = NULL, pocet_ve_vzorku = NULL, podil_ve_vzorku = NULL)

  for( i in names) {
    n_temp <- df %>% filter(!is.na(!!as.symbol(i))) %>% summarise(sum(w)) %>% as.numeric()
    temp <- df %>% group_by(!!as.symbol(i)) %>% summarise(pocet_ve_vzorku =sum(w), podil_ve_vzorku = pocet_ve_vzorku/n_temp)
    colnames(temp)[1] <- "skupina"
    temp <- temp %>% mutate(promenna = i,
                            nazev = to_factor(skupina, levels = "labels"),
                            skupina = as.character(skupina))


    tib <- rbind(tib,temp)

  }

  tib <- tib %>% select(promenna,nazev, skupina, pocet_ve_vzorku, podil_ve_vzorku)

  rozlozeni_vzorku <<-tib

  write.xlsx(tib, jmeno_souboru)
}








#' Svislý sloupcový graf s doplňkem do 100 %
#'@encoding UTF-8
#' @description Funkce vytvoří svislý sloupcový graf, který zobrazuje rozložení jedné proměnné (otázka v jednom sloupci) vůči libovolnému počtu dalších proměnných (např. demografických). Funkce umožňuje
#' měnit popisky na ose x, otočit legendu, nebo seřadit sloupce dle vlastních preferencí. Graf je možné buď automaticky uložit, nebo si upravit velikost dle potřeb a pak jej zkopírovat.
#' @param df Vstupní dataset. Sloupce musí obsahovat labely.
#' @param question Název jednoho slooupce ve formátu: "q12", pro který chceme křížit další proměnné.
#' @param rowname_vektor Seznam proměnných přes které chceme křížení provést. Tyto budou na ose x. Vložit ve formátu: "jmeno sloupce" nebo c("jméno sloupce 1","jméno sloupce 2",...) - pokud je proměnných více.
#' @param poradi_sloupcu Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_x, vložíme seznam již s novými názvy.
#' @param popisky_x Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param popisky_legenda Volitelný argument, pokud chceme změnit popisky legendy. Stačí vložit dvojce, které chceme změnit v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....)
#' @param otocit_legendu  Otočí pořadí legendy. Stačí vložit libovolný text v uvozovkách např. "A" , nebo text TRUE..
#' @param w Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např."VÁHA".
#' @param nadpis Pokud chceme, aby graf obsahoval znění otázky vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param save Pokud chceme, aby se graf automaticky uložil vložíme libovolný text v uvozovkách např. "A", nebo text TRUE. Graf se bude jmenovat: jmeno_otazky.jpeg.
#'
#' @return
#' @export
#'
#' @examples vertical.bar.complement(data,"mat",c("poh","vzd","age"), w = "VAHA", save = TRUE )
vertical.bar.complement <- function(df, question, rowname_vektor,  popisky_x = NULL,poradi_sloupcu = NULL, popisky_legenda = NULL, otocit_legendu = FALSE, w = "w", nadpis = NULL, save = NULL) {
  #Nadpis

  if(!is.null(nadpis))  nadpis <- select(df,question)[[1]] %>% var_label() %>% sub(" -.*","",.) %>% sub(".*\\. ","",.)


  #úprava df (přejmenování váhy a tvorba faktorů)

  df <-  df %>% rename(w = !!as.symbol(w))

  df <-  df %>% mutate_at(vars(-w),to_factor)



  #tvorba dlouhých dat a vážených procent a filter
  df <- pivot_longer(df,cols = rowname_vektor,names_to = "otazka",values_to = "odpovedi")

  df <- df %>% filter(odpovedi != is.na(odpovedi) & odpovedi != "sys_filtered_off" & odpovedi != "sys_empty")

  t1 <- df %>% group_by(odpovedi) %>% summarise(vaha_otazka = sum(w))

  df <- df %>% group_by(!!as.symbol(question),odpovedi) %>% summarise(soucet = sum(w))

  df <- df %>% left_join(t1, by ="odpovedi")

  df <- df %>% mutate(per = soucet/vaha_otazka)


  #Volba barev

  delka <- length(unique(df[[question]]))

  if (delka == 2 ) {barvy <- c("#404040","#811339")}
  if (delka == 3 ) {barvy <- c("#404040","#D6C4D1","#811339")}
  if (delka == 4 ) {barvy <- c("#404040","#A6A6A6","#B37188","#811339")}
  if (delka == 5 ) {barvy <- c("#404040","#A6A6A6","#D6C4D1","#B37188","#811339")}
  if (delka == 6 ) {barvy <- c("#404040","#A6A6A6","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 7 ) {barvy <- c("#404040","#A6A6A6","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 8 ) {barvy <- c("#404040","#A6A6A6","#9A9A78","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 9 ) {barvy <- c("#404040","#A6A6A6","#EAE9EA","#9A9A78","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}

  #změna popisek osy_x
  # if (!is.null(popisky_x)) {levels(df[["odpovedi"]]) <- popisky_x }

  if (!is.null(popisky_x)) {

    n <- popisky_x[c(which(1:length(popisky_x) %%2 !=0))]
    st <- popisky_x[c(which(1:length(popisky_x) %%2 ==0))]

    names(st) <- n

    df[["odpovedi"]] <- do.call(fct_recode, append(list(df[["odpovedi"]]), as.list(st)))

  }


  #změna pořadí sloupců na ose x
  if (!is.null(poradi_sloupcu)) {df$odpovedi <-factor(df$odpovedi, levels = poradi_sloupcu)}


  #změna popisek legendy  - popisky vkládat jako vektor(c(nové jméno a, staré jméno a, nové jméno b, .....))


  if (!is.null(popisky_legenda)) {

    n <- popisky_legenda[c(which(1:length(popisky_legenda) %%2 !=0))]
    st <- popisky_legenda[c(which(1:length(popisky_legenda) %%2 ==0))]

    names(st) <- n

    df[[question]] <- do.call(fct_recode, append(list(df[[question]]), as.list(st)))}



  a <<-df

  #změna řazení legendy (jen obrácení)

  if (otocit_legendu == FALSE) {df[[question]] <- factor(df[[question]], levels = rev(levels(df[[question]])))
  df <- arrange(df, !!as.symbol(question)) }


  pop_x <- str_replace_all(levels(df$odpovedi),"\\ ","\n")

  g <- df %>% ggplot(aes(x = odpovedi, y = per, fill = !!as.symbol(question),label = str_replace(round(per*100),"\\.",",")))+
    geom_bar(stat = "identity", position = "fill")+
    scale_fill_manual(values = barvy) +
    theme_minimal()+
    scale_x_discrete(labels = pop_x)+
    geom_text(size = 6, color = "black" ,position = position_stack(vjust= 0.5))+
    scale_y_continuous(labels = scales::percent)+
    guides(fill = guide_legend(reverse = TRUE))+
    theme(legend.text=element_text(size=12),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          plot.title = element_text(size=22, margin=margin(10,-10,10,0))) +
    labs(title = str_wrap(nadpis,width = 700))

  print(g)

  if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = length(unique(df$odpovedi))*2.5,height = 20, units = "cm",limitsize = FALSE)}
}



#' Horizontální sloupcový graf s přehledem možností s doplňkem do 100 %
#'@encoding UTF-8
#'@description Funkce vytvoří horizontální sloupcový graf, který zobrazuje rozložení odpovědí pro sadu vybraných výroků, otázek  (otázky ve více sloupcích se společným začátekm názvu). Funkce umožňuje
#' měnit popisky na ose y, otočit legendu, nebo seřadit sloupce dle vlastních preferencí. Graf je možné buď automaticky uložit, nebo si upravit velikost dle potřeb a pak jej zkopírovat.
#'
#' @param df Vstupní dataset. Sloupce musí obsahovat labely.
#' @param question Nejdelší společná část názvu sloupců, které chceme do grafu vynést (např. "Q2_"), nebo úplný název jediného sloupce, pokud chceme graf s jedním sloupcem (např."Q2_1").
#' @param poradi_sloupcu Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_y, vložíme seznam již s novými názvy.
#' @param popisky_y Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param popisky_legenda Volitelný argument, pokud chceme změnit popisky legendy. Stačí vložit dvojce, které chceme změnit v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....)
#' @param otocit_legendu Otočí pořadí legendy. Stačí vložit libovolný text v uvozovkách např. "A" , nebo text TRUE..
#' @param w Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např."VÁHA".
#' @param nadpis Pokud chceme, aby graf obsahoval znění otázky vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param save Pokud chceme, aby se graf automaticky uložil vložíme libovolný text v uvozovkách např. "A", nebo text TRUE. Graf se bude jmenovat: jmeno_otazky.jpeg.
#'
#' @return
#' @export
#'
#' @examples horizontal.bar.complement.multiple(data1,"d2_", w="VAHA",poradi_sloupcu = NULL, otocit_legendu = FALSE, nadpis = TRUE, save = TRUE)
horizontal.bar.complement.multiple <- function(df, question, poradi_sloupcu = NULL, popisky_y = NULL, popisky_legenda = NULL, otocit_legendu = FALSE, w = "w", nadpis = NULL, save = NULL) {


  #Nadpis

  if(!is.null(nadpis))  nadpis <- select(df,starts_with(question))[[1]] %>% var_label() %>% sub(" -.*","",.) %>% sub(".*\\. ","",.)

  #úprava df (přejmenování váhy a tvorba faktorů)

  df <-  df %>% rename(w = !!as.symbol(w))

  df <-  df %>% mutate_at(vars(-w),to_factor)


  #Přejmenování sloupců (osa y) podle labelů a případná změna popisek

  if (is.null(popisky_y)) {popisky_y <- df %>% select(starts_with(question)) %>% var_label() %>% sub(".*- ","",.) }

  else {

    #přejmenování osy y

    popisky_y_zaklad <- df %>% select(starts_with(question)) %>% var_label() %>% sub(".*- ","",.)

    n <- popisky_y[c(which(1:length(popisky_y) %%2 !=0))]
    st <- popisky_y[c(which(1:length(popisky_y) %%2 ==0))]

    names(st) <- n

    popisky_y_zaklad[which(popisky_y_zaklad %in% st)] <- n


    popisky_y <- popisky_y_zaklad

  }

  df <- df %>% rename_at(vars(contains(question)), ~ popisky_y)

  #Tvorba dlouhých dat

  df <- pivot_longer(df,cols = contains(popisky_y),names_to = "otazka",values_to = "value")

  df <- df %>% filter(!is.na(value))

  t1 <- df %>% group_by(otazka) %>% summarise(vaha_otazka = sum(w))

  df <- df %>% group_by(otazka,value) %>% summarise(polozka = sum(w))

  df <- df %>% left_join(t1, by ="otazka")

  df <- df %>% mutate(per = polozka/vaha_otazka)


  #Volba barev
  delka <- length(unique(df$value))

  if (delka == 2 ) {barvy <- c("#404040","#811339")}
  if (delka == 3 ) {barvy <- c("#404040","#D6C4D1","#811339")}
  if (delka == 4 ) {barvy <- c("#404040","#A6A6A6","#B37188","#811339")}
  if (delka == 5 ) {barvy <- c("#404040","#A6A6A6","#D6C4D1","#B37188","#811339")}
  if (delka == 6 ) {barvy <- c("#404040","#A6A6A6","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 7 ) {barvy <- c("#404040","#A6A6A6","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 8 ) {barvy <- c("#404040","#A6A6A6","#9A9A78","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 9 ) {barvy <- c("#404040","#A6A6A6","#EAE9EA","#9A9A78","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}

  #změna popisek legendy  - popisky vkládat jako vektor(c(nové jméno a, staré jméno a, nové jméno b, .....))

  if (!is.null(popisky_legenda)) {

    n <- popisky_legenda[c(which(1:length(popisky_legenda) %%2 !=0))]
    st <- popisky_legenda[c(which(1:length(popisky_legenda) %%2 ==0))]

    names(st) <- n
    print(st)
    df$value <- do.call(fct_recode, append(list(df$value), as.list(st)))
  }


  #Řazení legendy (jen obrácení)

  if (otocit_legendu == FALSE) {df$value <- factor(df$value, levels = rev(levels(df$value)))}

  else {df$value <- factor(df$value, levels = levels(df$value))}



  #Řazení na y

  if (!is.null(poradi_sloupcu)) {df$otazka <-factor(df$otazka, levels = poradi_sloupcu)
  df <- arrange(df, otazka) }

  else {
    temp <- df %>% filter(value == levels(df$value)[length(levels(df$value))]) %>% arrange(.,per)

    df$otazka <-factor(df$otazka, levels = temp$otazka)

  }

  p <- df %>%  ggplot(aes(x = otazka, y= per, fill = value, label = str_replace(round(per*100),"\\.",",")))+
    geom_bar(stat = "identity", position = "fill")+
    scale_fill_manual(values = barvy)+
    theme_minimal()+
    geom_text(size = 6, color = "black", position = position_stack(vjust= 0.5))+
    scale_x_discrete(labels= function(x) str_wrap(x, width = 40))+
    scale_y_continuous(labels = scales::percent)+
    coord_flip()+
    theme(legend.text=element_text(size=12),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.direction = "horizontal",
          plot.title = element_text(size=18, margin=margin(10,0,30,0)),
          plot.title.position = "plot")+
    guides(fill = guide_legend(reverse = TRUE))+
    labs(title = str_wrap(nadpis, width = 700))
  print(p)

  #uložení
  if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = 30,height = length(unique(df$otazka))*3.5, units = "cm")}
}




#' Sloupcový graf
#'@encoding UTF-8
#'@description Funkce vytvoří jednoduchý sloupcový graf, který zobrazuje rozložení vybraných variant odpovědí pro sadu výroků - otázek (otázky ve více sloupcích se společným začátekm názvu). Funkce umožňuje
#' zvolit si varianty odpovědí, které chceme zobrazit a případně je sloučit (např. určitě ano + spíše ano), měnit popisky u názvů sloupců, otočit osy, nebo seřadit sloupce dle vlastních preferencí. Graf je možné buď automaticky uložit, nebo si upravit velikost dle potřeb a pak jej zkopírovat.
#'
#'
#' @param df Vstupní dataset. Sloupce musí obsahovat labely.
#' @param question Nejdelší společná část názvu sloupců, které chceme do grafu vynést (např. "Q2_"), nebo úplný název jediného sloupce, pokud chceme graf s jedním sloupcem (např."Q2_1").
#' @param poradi_sloupcu Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_y, vložíme seznam již s novými názvy.
#' @param popisky_y Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param popisky_legenda Volitelný argument, pokud chceme změnit popisky legendy. Stačí vložit dvojce, které chceme změnit v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....)
#' @param moznosti Možnosti odpovědí, které chceme zobrazit např. c("Určitě ano","Spíše ano").
#' @param sloucit Pokud chceme, aby zvolené možnosti byly zobrazeny dohromady jednou barvou (např. sečíst možnosti určitě ano a spíše ano), vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param otocit_osy Pokud chceme, aby sloupce nebyly orientovány horizontálně ale vertikálně vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param w Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např."VÁHA".
#' @param nadpis Pokud chceme, aby graf obsahoval znění otázky vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param save Pokud chceme, aby se graf automaticky uložil vložíme libovolný text v uvozovkách např. "A", nebo text TRUE. Graf se bude jmenovat: jmeno_otazky.jpeg.
#'
#' @return
#' @export
#'
#' @examples bar.chart(data1,"d2_", w="VAHA",moznosti = c("Určitě ano","Spíše ano"), nadpis = "A", sloucit = "A",save = "A")
bar.chart <- function(df, question, poradi_sloupcu = NULL, popisky_y = NULL, popisky_legenda = NULL, moznosti = NULL, sloucit = NULL, otocit_osy = NULL, w = "w", nadpis = NULL, save = NULL) {


  #Nadpis

  if(!is.null(nadpis))  nadpis <- select(df,starts_with(question))[[1]] %>% var_label() %>% sub(" -.*","",.) %>% sub(".*\\. ","",.)

  #úprava df (přejmenování váhy a tvorba faktorů)

  df <-  df %>% rename(w = !!as.symbol(w))

  df <-  df %>% mutate_at(vars(-w),to_factor)


  #Přejmenování sloupců (osa y) podle labelů a případná změna popisek

  if (is.null(popisky_y)) {popisky_y <- df %>% select(starts_with(question)) %>% var_label() %>% sub(".*- ","",.) }

  else {

    #přejmenování osy y

    popisky_y_zaklad <- df %>% select(starts_with(question)) %>% var_label() %>% sub(".*- ","",.)

    n <- popisky_y[c(which(1:length(popisky_y) %%2 !=0))]
    st <- popisky_y[c(which(1:length(popisky_y) %%2 ==0))]

    names(st) <- n

    popisky_y_zaklad[which(popisky_y_zaklad %in% st)] <- n


    popisky_y <- popisky_y_zaklad

  }

  df <- df %>% rename_at(vars(contains(question)), ~ popisky_y)


  #Tvorba dlouhých dat

  df <- pivot_longer(df,cols = contains(popisky_y),names_to = "otazka",values_to = "value")

  df <- df %>% filter(!is.na(value))

  t1 <- df %>% group_by(otazka) %>% summarise(vaha_otazka = sum(w))

  df <- df %>% group_by(otazka,value) %>% summarise(polozka = sum(w))

  df <- df %>% left_join(t1, by ="otazka")

  df <- df %>% mutate(per = polozka/vaha_otazka)


  #výběr možností

  df <- df[df$value %in% c(moznosti),]


  #sloučení možností

  if (!is.null(sloucit)) {

    df <- df %>% group_by(otazka) %>% summarize(per = sum(per))

    df <- df %>% mutate(value = paste(moznosti,sep= "", collapse = " + "))

    a <<- df

  }



  #výběr barev
  delka <- length(unique(df$value))

  if (delka == 1 ) {barvy <- c("#811339")}
  if (delka == 2 ) {barvy <- c("#B37188","#811339")}
  if (delka == 3 ) {barvy <- c("#D6C4D1","#B37188","#811339")}
  if (delka == 4 ) {barvy <- c("#A6A6A6","#D6C4D1","#B37188","#811339")}
  if (delka == 5 ) {barvy <- c("#404040","#A6A6A6","#D6C4D1","#B37188","#811339")}
  if (delka == 6 ) {barvy <- c("#404040","#A6A6A6","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 7 ) {barvy <- c("#404040","#A6A6A6","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 8 ) {barvy <- c("#404040","#A6A6A6","#9A9A78","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}
  if (delka == 9 ) {barvy <- c("#404040","#A6A6A6","#EAE9EA","#9A9A78","#D0D0A7","#E2E2D5","#D6C4D1","#B37188","#811339")}

  #změna popisek legendy  - popisky vkládat jako vektor(c(nové jméno a, staré jméno a, nové jméno b, .....))

  if (!is.null(popisky_legenda)) {

    n <- popisky_legenda[c(which(1:length(popisky_legenda) %%2 !=0))]
    st <- popisky_legenda[c(which(1:length(popisky_legenda) %%2 ==0))]

    names(st) <- n
    print(st)
    df$value <- do.call(fct_recode, append(list(df$value), as.list(st)))
  }



  #Řazení na y

  #(odstranění nepoužitých levelů)

  df$value <- factor(df$value)

  if (!is.null(poradi_sloupcu)) {df$otazka <-factor(df$otazka, levels = poradi_sloupcu)
  df <- arrange(df, otazka) }

  else {
    temp <- df %>% filter(value == levels(df$value)[length(levels(df$value))]) %>% arrange(.,per)

    df$otazka <-factor(df$otazka, levels = temp$otazka)

  }


  if (!is.null(otocit_osy)) {
    p <- df %>%  ggplot(aes(x = otazka, y= per, fill = value, label = str_replace(round(per*100),"\\.",",")))+
      geom_bar(stat = "identity", position = "stack")+
      scale_fill_manual(values = barvy)+
      theme_minimal()+
      geom_text(size = 6, color = "black", position = position_stack(vjust= 0.5))+
      scale_x_discrete(labels= function(x) str_wrap(x, width = 30))+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      theme(legend.text=element_text(size=12),
            axis.text.x = element_text(size = 14,angle = 90),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            plot.title = element_text(size=18, margin=margin(10,0,30,0)),
            plot.title.position = "plot")+
      guides(fill = guide_legend(reverse = TRUE))+
      labs(title = str_wrap(nadpis, width = 12*length(unique(df$otazka))))

    #uložení
    if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = length(unique(df$otazka))*3.5,height = 30, units = "cm")}
  }

  else  {
    p <- df %>%  ggplot(aes(x = otazka, y= per, fill = value, label = str_replace(round(per*100),"\\.",",")))+
      geom_bar(stat = "identity", position = "stack")+
      scale_fill_manual(values = barvy)+
      theme_minimal()+
      geom_text(size = 6, color = "black", position = position_stack(vjust= 0.5))+
      scale_x_discrete(labels= function(x) str_wrap(x, width = 40))+
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      coord_flip()+
      theme(legend.text=element_text(size=12),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            plot.title = element_text(size=18, margin=margin(10,0,30,0)),
            plot.title.position = "plot")+
      guides(fill = guide_legend(reverse = TRUE))+
      labs(title = str_wrap(nadpis, width = 700))


    #uložení
    if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = 30 ,height =length(unique(df$otazka))*3.5 , units = "cm")}
  }
  print(p)
}




#' Bubliny nepravé
#'@encoding UTF-8
#'@description Funkce vytvoří bublinový graf, který zobrazuje rozložení jedné proměnné (otázka v jednom sloupci) vůči libovolnému počtu dalších proměnných (např. demografických).
#'Zobrazuje při tom všechny možnosti odpovědí a jde tak vlastně o jiný typ zobrazení dat ze Svislý sloupcový graf s doplňkem do 100 %. Funkce umožňuje měnit popisky na ose x,
#' nebo seřadit sloupce dle vlastních preferencí. Graf je možné buď automaticky uložit, nebo si upravit velikost dle potřeb a pak jej zkopírovat.
#' @param df Vstupní dataset. Sloupce musí obsahovat labely.
#' @param question Název jednoho slooupce ve formátu: "q12", pro který chceme křížit další proměnné.
#' @param rowname_vektor Seznam proměnných přes které chceme křížení provést. Tyto budou na ose x. Vložit ve formátu: "jmeno sloupce" nebo c("jméno sloupce 1","jméno sloupce 2",...) - pokud je proměnných více.
#' @param poradi_sloupcu Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_x, vložíme seznam již s novými názvy.
#' @param popisky_x Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param popisky_y Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param w Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např."VÁHA".
#' @param nadpis Pokud chceme, aby graf obsahoval znění otázky vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param save Pokud chceme, aby se graf automaticky uložil vložíme libovolný text v uvozovkách např. "A", nebo text TRUE. Graf se bude jmenovat: jmeno_otazky.jpeg.
#'
#' @return
#' @export
#'
#' @examples bubliny.neprave(data1,"mat",c("poh","vzd","age"), w = "VAHA", popisky_y = c("AAA","V zásadě chudí"))
bubliny.neprave <- function(df, question, rowname_vektor, poradi_sloupcu = NULL, popisky_x = NULL, popisky_y = NULL, w = "w", nadpis = NULL, save = NULL) {

  #Nadpis

  if(!is.null(nadpis))  nadpis <- select(df,question)[[1]] %>% var_label() %>% sub(" -.*","",.) %>% sub(".*\\. ","",.)


  #úprava df (přejmenování váhy a tvorba faktorů)

  df <-  df %>% rename(w = !!as.symbol(w))

  df <-  df %>% mutate_at(vars(-w),to_factor)



  #tvorba dlouhých dat a vážených procent a filter
  df <- pivot_longer(df,cols = rowname_vektor,names_to = "otazka",values_to = "odpovedi")

  df <- df %>% filter(odpovedi != is.na(odpovedi) & odpovedi != "sys_filtered_off" & odpovedi != "sys_empty")

  t1 <- df %>% group_by(odpovedi) %>% summarise(vaha_otazka = sum(w))

  df <- df %>% group_by(!!as.symbol(question),odpovedi) %>% summarise(soucet = sum(w))

  df <- df %>% left_join(t1, by ="odpovedi")

  df <- df %>% mutate(per = soucet/vaha_otazka)


  #změna popisek osy_x

  if (!is.null(popisky_x)) {

    n <- popisky_x[c(which(1:length(popisky_x) %%2 !=0))]
    st <- popisky_x[c(which(1:length(popisky_x) %%2 ==0))]

    # popisky_x <- str_replace(levels(df[["odpovedi"]]),st,n)

    names(st) <- n

    df[["odpovedi"]] <- do.call(fct_recode, append(list(df[["odpovedi"]]), as.list(st)))

  }


  #změna pořadí sloupců na ose x
  if (!is.null(poradi_sloupcu)) {df$odpovedi <-factor(df$odpovedi, levels = poradi_sloupcu)
  df <- arrange(df, odpovedi) }

  #změna popisek legendy  - popisky vkládat jako vektor(c(nové jméno a, staré jméno a, nové jméno b, .....))


  if (!is.null(popisky_y)) {

    n <- popisky_y[c(which(1:length(popisky_y) %%2 !=0))]
    st <- popisky_y[c(which(1:length(popisky_y) %%2 ==0))]

    names(st) <- n

    df[[question]] <- do.call(fct_recode, append(list(df[[question]]), as.list(st)))}



  g <- df %>% ggplot(aes(x = fct_inorder(str_replace_all(odpovedi,"\\ ","\n")) , y = !!as.symbol(question), size = per, colour = per,  label = round(per*100)))+
    geom_point()+
    scale_radius(range = c(4, 25))+
    geom_text(size = 6, color = "black")+
    theme_minimal()+
    geom_text(size = 6, color = "black")+
    scale_x_discrete(labels= function(x) str_wrap(x, width = 8))+
    scale_y_discrete(labels= function(x) str_wrap(x, width = 40))+
    scale_color_gradient(low = "#DCF3E1",
                         high = "#0A4C19")+
    theme(legend.text=element_text(size=12),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 16),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none",
          plot.title = element_text(size=18, margin=margin(10,-10,10,0))) +
    labs(title = str_wrap(nadpis,width = length(unique(df$odpovedi))*7))

  print(g)

  if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = length(unique(df$odpovedi))*2.5,height = 2.5*length(unique(df[[question]])), units = "cm",limitsize = FALSE)}
}


#' Bubliny
#'@encoding UTF-8
#'@description Umožňuje zobrazit sadu otázek v několika sloupcích se stejnou sadou odpovědí jako bublinový graf a zobrazit podíl vybraného typu odpovědi nebo jejich kombinaci. Velikost bubliny udává poměr odpovědí pro zvolenou variantu.
#'
#'
#' @param df Vstupní dataset. Sloupce musí obsahovat labely.
#' @param question Nejdelší společná část názvu sloupců, které chceme do grafu vynést (např. "Q2_"), nebo úplný název jediného sloupce, pokud chceme graf s jednou řadou bublin (např."Q2_1").
#' @param poradi_sloupcu_y Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_y, vložíme seznam již s novými názvy.
#' @param poradi_sloupcu_x Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_x, vložíme seznam již s novými názvy.
#' @param popisky_x Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param popisky_y Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param moznosti Možnosti odpovědí, které chceme zobrazit např. c("Určitě ano","Spíše ano").
#' @param otocit_osy Pokud chceme prohodit osy, vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param w Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např."VÁHA".
#' @param nadpis Pokud chceme, aby graf obsahoval znění otázky vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param save Pokud chceme, aby se graf automaticky uložil vložíme libovolný text v uvozovkách např. "A", nebo text TRUE. Graf se bude jmenovat: jmeno_otazky.jpeg.
#'
#' @return
#' @export
#'
#' @examples bubliny(data1,"d6_",w = "VAHA", poradi_sloupcu_x = c("Policie ČR","Hasiči","Vědecké instituce","Hobby uživatelé","Komerční uživatelé")
#' @examples bubliny(data,"dá_",w = "VAHA",moznosti = c("Určitě ano","Spíše ano"))
bubliny <- function(df, question, popisky_x = NULL, popisky_y = NULL, poradi_sloupcu_y = NULL, poradi_sloupcu_x = NULL, moznosti = NULL, otocit_osy = NULL, w = "w", nadpis = NULL, save = NULL) {


  #Nadpis

  if(!is.null(nadpis))  nadpis <- select(df,starts_with(question))[[1]] %>% var_label() %>% sub(" -.*","",.) %>% sub(".*\\. ","",.)

  #úprava df (přejmenování váhy a tvorba faktorů)

  df <-  df %>% rename(w = !!as.symbol(w))

  df <-  df %>% mutate_at(vars(-w),to_factor)


  #uložení a úprava jmen (osa_y) před otočením dat

  jmena <- sapply(select(df,starts_with(question)), var_label) %>% as_tibble() %>% rename(zn = "value")

  jmena <- jmena %>% mutate(zn = sub(".*- ","",zn))

  jmena2 <- colnames(select(df,starts_with(question))) %>% as_tibble() %>% rename(otazka = "value")

  jmena <- jmena %>% cbind(jmena2)


  #změna popisek osa y  - popisky vkládat jako vektor(c(nové jméno a, staré jméno a, nové jméno b, .....))

  if (!is.null(popisky_y)) {

    n <- popisky_y[c(which(1:length(popisky_y) %%2 !=0))]
    st <- popisky_y[c(which(1:length(popisky_y) %%2 ==0))]

    names(st) <- n
    print(st)
    jmena$zn <- do.call(fct_recode, append(list(jmena$zn), as.list(st)))
  }

  #Tvorba dlouhých dat

  df <- pivot_longer(df,cols = starts_with(question),names_to = "otazka",values_to = "value")

  df <- df %>% filter(!is.na(value))

  t1 <- df %>% group_by(otazka) %>% summarise(vaha_otazka = sum(w))

  df <- df %>% group_by(otazka,value) %>% summarise(polozka = sum(w))

  df <- df %>% left_join(t1, by ="otazka")

  df <- df %>% mutate(per = polozka/vaha_otazka) %>% ungroup()

  df <- df %>% left_join(jmena, by = "otazka")

  df <- df %>% filter(value != "0")


  #změna popisek osa x  - popisky vkládat jako vektor(c(nové jméno a, staré jméno a, nové jméno b, .....))

  if (!is.null(popisky_x)) {

    n <- popisky_x[c(which(1:length(popisky_x) %%2 !=0))]
    st <- popisky_x[c(which(1:length(popisky_x) %%2 ==0))]

    names(st) <- n
    print(st)
    df$value <- do.call(fct_recode, append(list(df$value), as.list(st)))
  }


  #výběr možností

  #   df <- df[df$value %in% c(moznosti),]
  #
  #  df <- df %>% group_by(otazka) %>% summarize(per = sum(per))
  #
  #  df <- df %>% mutate(value = paste(moznosti,sep= "", collapse = " + "))
  #
  # d <<- df

  #Řazení na x

  #(odstranění nepoužitých levelů)

  df$value <- factor(df$value)

  if (!is.null(poradi_sloupcu_x)) {df$value <-factor(df$value, levels = poradi_sloupcu_x)}

  else {

    temp <- df %>% group_by(value) %>% summarise(prumer = mean(per))  %>% arrange(.,desc(prumer))

    df$value <-factor(df$value, levels = temp$value)

  }

  #Řazení na y

  #(odstranění nepoužitých levelů)

  df$value <- factor(df$value)

  if (!is.null(poradi_sloupcu_y)) {df$zneni <-factor(df$zn, levels = poradi_sloupcu_y)}

  else {
    temp <- df %>% group_by(zn) %>% summarise(prumer = mean(per))  %>% arrange(.,prumer)

    df$zn <-factor(df$zn, levels = temp$zn)
  }


  if (is.null(otocit_osy)) {
    g <- df %>% ggplot(aes(x = value , y = zn, size = per, colour = per,  label = round(per*100)))+
      geom_point()+
      scale_radius(range = c(4, 25))+
      geom_text(size = 6, color = "black")+
      theme_minimal()+
      geom_text(size = 6, color = "black")+
      scale_x_discrete(labels= function(x) str_wrap(x, width = 8))+
      scale_y_discrete(labels= function(x) str_wrap(x, width = 40))+
      scale_color_gradient(low = "#DCF3E1",
                           high = "#0A4C19")+
      theme(legend.text=element_text(size=12),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none",
            plot.title = element_text(size=18, margin=margin(10,-10,10,0))) +
      labs(title = str_wrap(nadpis,width = length(unique(df$odpovedi))*7))

    #uložení
    if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = length(unique(df$otazka))*3.5,height = 30, units = "cm")}
  }

  print(g)
}



#' Bubliny cross
#'
#'@description Umožňuje zobrazit sadu otázek v několika sloupcích se stejnou sadou odpovědí, kříženou s vybranými dalšími otázkami jako bublinový graf a zobrazit podíl vybraného typu odpovědi nebo jejich kombinaci. Velikost bubliny udává poměr odpovědí pro zvolenou variantu.
#'
#' @param df Vstupní dataset. Sloupce musí obsahovat labely.
#' @param question Nejdelší společná část názvu sloupců, které chceme do grafu vynést (např. "Q2_"), nebo úplný název jediného sloupce, pokud chceme graf s jednou řadou bublin (např."Q2_1").
#' @param rowname_vector Seznam proměnných přes které chceme křížení provést. Tyto budou na ose x. Vložit ve formátu: "jmeno sloupce" nebo c("jméno sloupce 1","jméno sloupce 2",...) - pokud je proměnných více.
#' @param poradi_sloupcu_y Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_y, vložíme seznam již s novými názvy.
#' @param poradi_sloupcu_x Volitelný argument, pokud chceme změnit pořadí sloupců. Pokud ano, je třeba vložit seznam všech (!) názvů. Pokud jsme změnili názvy skrze argument popisky_x, vložíme seznam již s novými názvy.
#' @param popisky_x Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param popisky_y Volitelný argument, pokud chceme změnit názvy sloupců. Pokud ano, je třeba vložit seznam změn v následujícím formátu: c(nové jméno a, staré jméno a, nové jméno b, staré jméno b .....).
#' @param moznosti Možnosti odpovědí, které chceme zobrazit např. c("Určitě ano","Spíše ano").
#' @param otocit_osy Pokud chceme prohodit osy, vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param w Pokud chceme jako váhu použít jiný sloupec, než sloupec pojmenovaný w. Vložíme název sloupce, který obsahuje váhu, kterou chceme použít. např."VÁHA".
#' @param nadpis Pokud chceme, aby graf obsahoval znění otázky vložíme libovolný text v uvozovkách např. "A", nebo text TRUE.
#' @param save Pokud chceme, aby se graf automaticky uložil vložíme libovolný text v uvozovkách např. "A", nebo text TRUE. Graf se bude jmenovat: jmeno_otazky.jpeg.
#'
#' @return
#' @export
#'
#' @examples bubliny.cross(data1,"d2_",rowname_vector = c("inte"), w = "VAHA", moznosti = c("Určitě ano","Spíše ano"), otocit_osy = "ano", popisky_x = c("Mám rád Miloše","Mám rád Hroše"))
bubliny.cross <- function(df, question, rowname_vector, poradi_sloupcu_y = NULL, poradi_sloupcu_x = NULL, popisky_x = NULL, popisky_y = NULL, moznosti = NULL, otocit_osy = NULL, w = "w", nadpis = NULL, save = NULL) {


  #Nadpis

  if(!is.null(nadpis))  nadpis <- select(df,starts_with(question))[[1]] %>% var_label() %>% sub(" -.*","",.) %>% sub(".*\\. ","",.)

  #úprava df (přejmenování váhy a tvorba faktorů)

  df <-  df %>% rename(w = !!as.symbol(w))

  df <-  df %>% mutate_at(vars(-w),to_factor)

  # přejmenování otázek

  if (is.null(popisky_y)) {popisky_y <- df %>% select(starts_with(question)) %>% var_label() %>% sub(".*- ","",.) }

  else {


    popisky_y_zaklad <- df %>% select(starts_with(question)) %>% var_label() %>% sub(".*- ","",.)

    n <- popisky_y[c(which(1:length(popisky_y) %%2 !=0))]
    st <- popisky_y[c(which(1:length(popisky_y) %%2 ==0))]

    names(st) <- n

    popisky_y_zaklad[which(popisky_y_zaklad %in% st)] <- n


    popisky_y <- popisky_y_zaklad

  }





  df <- df %>% rename_at(vars(contains(question)), ~ popisky_y)



  #tvorba dlouhých dat a vážených procent a filter
  df <- pivot_longer(df,cols = rowname_vector,names_to = "otazka",values_to = "odpovedi")

  df <- df %>% filter(odpovedi != is.na(odpovedi) & odpovedi != "sys_filtered_off" & odpovedi != "sys_empty")

  t1 <- df %>% group_by(odpovedi) %>% summarise(vaha_otazka = sum(w))

  df <- pivot_longer(df, cols = starts_with(popisky_y), names_to = "otazka_2", values_to = "odpovedi_2")

  df <- df %>% group_by(odpovedi,otazka_2,odpovedi_2) %>% summarise(soucet = sum(w))

  df <- df %>% left_join(t1, by ="odpovedi")

  df <- df %>% mutate(per = soucet/vaha_otazka)


  #změna popisek osa x  - popisky vkládat jako vektor(c(nové jméno a, staré jméno a, nové jméno b, .....))

  if (!is.null(popisky_x)) {

    n <- popisky_x[c(which(1:length(popisky_x) %%2 !=0))]
    st <- popisky_x[c(which(1:length(popisky_x) %%2 ==0))]

    names(st) <- n
    print(st)
    df$odpovedi <- do.call(fct_recode, append(list(df$odpovedi), as.list(st)))
  }


  #výběr možností

  df <- df[df$odpovedi_2 %in% c(moznosti),]

  df <- df %>% group_by(odpovedi,otazka_2) %>% summarize(per = sum(per))

  df <- df %>% mutate(value = paste(moznosti,sep= "", collapse = " + "))


  #Řazení na x

  #(odstranění nepoužitých levelů)

  df$value <- factor(df$value)

  if (!is.null(poradi_sloupcu_x)) {df$odpovedi <-factor(df$odpovedi, levels = poradi_sloupcu_x)}


  #Řazení na y

  #(odstranění nepoužitých levelů)

  df$value <- factor(df$value)

  if (!is.null(poradi_sloupcu_y)) {df$otazka_2 <-factor(df$otazka_2, levels = poradi_sloupcu_y)}

  else {
    temp <- df %>% group_by(otazka_2) %>% summarise(prumer = mean(per))  %>% arrange(.,prumer)

    df$otazka_2 <-factor(df$otazka_2, levels = temp$otazka_2)
  }


  if (is.null(otocit_osy)) {
    g <- df %>% ggplot(aes(x = odpovedi , y = otazka_2, size = per, colour = per,  label = round(per*100)))+
      geom_point()+
      scale_radius(range = c(4, 25))+
      geom_text(size = 6, color = "black")+
      theme_minimal()+
      geom_text(size = 6, color = "black")+
      scale_x_discrete(labels= function(x) str_wrap(x, width = 8))+
      scale_y_discrete(labels= function(x) str_wrap(x, width = 40))+
      scale_color_gradient(low = "#DCF3E1",
                           high = "#0A4C19")+
      theme(legend.text=element_text(size=12),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none",
            plot.title = element_text(size=18, margin=margin(10,-10,10,0))) +
      labs(title = str_wrap(nadpis,width = length(unique(df$odpovedi))*7))

    #uložení
    if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = length(unique(df$odpovedi))*3.5,height = length(unique(df$otazka_2))*3.5, units = "cm")}
  }

  if (!is.null(otocit_osy)) {
    g <- df %>% ggplot(aes(x = odpovedi , y = otazka_2, size = per, colour = per,  label = round(per*100)))+
      geom_point()+
      scale_radius(range = c(4, 25))+
      geom_text(size = 6, color = "black")+
      theme_minimal()+
      coord_flip()+
      geom_text(size = 6, color = "black")+
      scale_x_discrete(labels= function(x) str_wrap(x, width = 40))+
      scale_y_discrete(labels= function(x) str_wrap(x, width = 8))+
      scale_color_gradient(low = "#DCF3E1",
                           high = "#0A4C19")+
      theme(legend.text=element_text(size=12),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none",
            plot.title = element_text(size=18, margin=margin(10,-10,10,0))) +
      labs(title = str_wrap(nadpis,width = length(unique(df$odpovedi))*7))

    #uložení
    if (!is.null(save)) {ggsave(filename = sprintf("%s.jpeg",question), width = length(unique(df$otazka_2))*3.5,height = length(unique(df$odpovedi))*3.5, units = "cm")}
  }


  print(g)
}
