TODAYS_DATE <- format.Date(Sys.time(),"%Y-%m-%d")
TODAYS_YEAR <- as.numeric(format.Date(Sys.time(),"%Y"))
TODAYS_MONTH <- as.numeric(format.Date(Sys.time(),"%m"))
TODAYS_WEEK <- as.numeric(format.Date(Sys.time(),"%V"))
TODAYS_WEEK_WITHIN_SEASON <- (TODAYS_WEEK+25)%%52+1

if(TODAYS_MONTH<=6){
  TODAYS_SEASON <- sprintf("%s/%s",TODAYS_YEAR-1,TODAYS_YEAR)
} else {
  TODAYS_SEASON <- sprintf("%s/%s",TODAYS_YEAR,TODAYS_YEAR+1)
}

DATA_CAPTION <- list(
  "NB"=sprintf("\nKilde: MSIS/Folkehelseinstituttet. Uttrekningsdato: %s",TODAYS_DATE),
  "EN"=sprintf("\nSource: MSIS/Norwegian Institute of Public Health. Extraction date: %s",TODAYS_DATE)
)

MONTHS_SHORT_SEASON <- list(
  "NB"=c("J","A","S","O","N","D","J","F","M","A","M","J") ,
  "EN"=c("J","A","S","O","N","D","J","F","M","A","M","J") 
)

MONTHS_LONG_YEAR <- list(
  "NB"=c("Januar","Februar","Mars","April","Mai","Juni","Juli","August","September","Oktober","November","Desember") ,
  "EN"=c("January","February","March","April","May","June","July","August","September","October","November","December") 
)

THEME_BASE_SIZE <- 18

NORCHAR <- new.env(parent = emptyenv())
NORCHAR$AA <- "\u00C5"
NORCHAR$aa <- "\u00E5"
NORCHAR$OE <- "\u00D8"
NORCHAR$oe <- "\u00F8"
NORCHAR$AE <- "\u00C6"
NORCHAR$ae <- "\u00E6"

FYLKE_LEVELS <- c(
  "\u00D8stfold",
  "Akershus",
  "Oslo",
  "Hedmark",
  "Oppland",
  "Buskerud",
  "Vestfold",
  "Telemark",
  "Aust-Agder",
  "Vest-Agder",
  "Rogaland",
  "Hordaland",
  "Sogn og Fjordane",
  "M\u00F8re og Romsdal",
  "Tr\u00F8ndelag",
  "Nordland",
  "Troms",
  "Finnmark"
)

CountriesNBtoEN <- function(){
  faroeIslands <- paste0("F",NORCHAR$ae,"r",NORCHAR$OE,"yene")
  austria <- paste0(NORCHAR$OE,"sterrike")
  southAfrica <- paste0("S",NORCHAR$oe,"r-Afrika")
  southKorea <- paste0("S",NORCHAR$oe,"r-Korea")
  
  c(
    "Afghanistan"="Afghanistan",
    "Albania"="Albania",
    "Algerie"="Algeria",
    "Andorra"="Andorra",
    "Angola"="Angola",
    "Argentina"="Argentina",
    "Armenia"="Armenia",
    "Aserbajdsjan"="Azerbaijan",
    "Australia"="Australia",
    "Bangladesh"="Bangladesh",
    "Belgia"="Belgium",
    "Bhutan"="Bhutan",
    "Bolivia"="Bolivia",
    "Bosnia-Hercegovina"="Bosnia and Herzegovina",
    "Botswana"="Botswana",
    "Brasil"="Brazil",
    "Bulgaria"="Bulgaria",
    "Burkina Faso"="Burkina Faso",
    "Burundi"="Burundi",
    "Canada"="Canada",
    "Chile"="Chile",
    "Colombia"="Colombia",
    "Costa Rica"="Costa Rica",
    "Cuba"="Cuba",
    "Danmark"="Denmark",
    "Djibouti"="Djibouti",
    "Dominikanske Republikk"="Dominican Republic",
    "Ecuador"="Ecuador",
    "Egypt"="Egypt",
    "Ekvatorial Guinea"="Equatorial Guinea",
    "El Salvador"="El Salvador",
    "Elfenbenskysten"="Cote d'Ivoire",
    "Eritrea"="Eritrea",
    "Estland"="Estonia",
    "Etiopia"="Ethiopia",
    faroeIslands="Faroe Islands",
    "Filippinene"="Philippines",
    "Finland"="Finland",
    "Forente Arabiske Emirater"="UAE",
    "Frankrike"="France",
    "Gabon"="Gabon",
    "Gambia"="Gambia",
    "Georgia"="Georgia",
    "Ghana"="Ghana",
    "Guatemala"="Guatemala",
    "Guinea"="Guinea",
    "Guinea-Bissau"="Guinea-Bissau",
    "Guyana"="Guyana",
    "Hellas"="Greece",
    "Honduras"="Honduras",
    "Hongkong"="Hong Kong",
    "Hviterussland"="Belarus",
    "India"="India",
    "Indonesia"="Indonesia",
    "Irak"="Iraq",
    "Iran"="Iran",
    "Irland"="Ireland",
    "Island"="Iceland",
    "Israel"="Israel",
    "Italia"="Italy",
    "Japan"="Japan",
    "Jemen"="Yemen",
    "Jordan"="Jordan",
    "Jugoslavia"="Yugoslavia",
    "Kambodsja"="Cambodia",
    "Kamerun"="Cameroon",
    "Kapp Verde"="Cape Verde",
    "Kasakhstan"="Kazakhstan",
    "Kenya"="Kenya",
    "Kina"="China",
    "Kirgisistan"="Kyrgyzstan",
    "Kongo (Dem.Rep.)"="DRC",
    "Kongo-Brazzaville"="Repub. Congo",
    "Kosovo"="Kosovo",
    "Kroatia"="Croatia",
    "Laos"="Laso",
    "Latvia"="Latvia",
    "Libanon"="Lebanon",
    "Liberia"="Liberia",
    "Libya"="Libya",
    "Litauen"="Lithuania",
    "Madagaskar"="Madagascar",
    "Makedonia"="Macedonia",
    "Malawi"="Malawi",
    "Malaysia"="Malaysia",
    "Mali"="Mali",
    "Marokko"="Morocco",
    "Mauretania"="Mauretania",
    "Mexico"="Mexico",
    "Moldova"="Moldova",
    "Mongolia"="Mongolia",
    "Montenegro"="Montenegro",
    "Mosambik"="Mozambique",
    "Myanmar"="Myanmar",
    "Namibia"="Nanibia",
    "Nederland"="Netherlands",
    "Nepal"="Nepal",
    "Niger"="Niger",
    "Nigeria"="Nigeria",
    "Nord-Korea"="DPRK",
    "Norge"="Norway",
    "Oman"="Oman",
    austria="Austria",
    "Pakistan"="Pakistan",
    "Palestina"="Palestine",
    "Papua Ny-Guinea"="PNG",
    "Peru"="Peru",
    "Polen"="Poland",
    "Portugal"="Portugal",
    "Romania"="Romania",
    "Russland"="Russia",
    "Rwanda"="Rwanda",
    "Saudi-Arabia"="Saudi Arabia",
    "Senegal"="Senegal",
    "Sentralafrikanske Republikk"="CAR",
    "Serbia"="Serbia",
    "Sierra Leone"="Sierra Leone",
    "Somalia"="Somalia",
    southAfrica="South Africa",
    southKorea="South Korea",
    "Spania"="Spain",
    "Sri Lanka"="Sri Lanka",
    "Storbritannia"="UK",
    "Sudan"="Sudan",
    "Sveits"="Switzerland",
    "Sverige"="Sweden",
    "Swaziland"="Swaziland",
    "Syria"="Syria",
    "Taiwan"="Taiwan",
    "Tanzania"="Tanzania",
    "Thailand"="Thailand",
    "Togo"="Togo",
    "Tsjekkia"="Czech Republic",
    "Tsjekkoslovakia"="Czechoslovakia",
    "Tunisia"="Tunisia",
    "Turkmenistan"="Turkmenistan",
    "Tyrkia"="Turkey",
    "Tyskland"="Germany",
    "Uganda"="Uganda",
    "Ukraina"="Ukraine",
    "Ungarn"="Hungry",
    "Uruguay"="Uruguay",
    "USA"="USA",
    "Usbekistan"="Uzbekistan",
    "Venezuela"="Venezuela",
    "Vietnam"="Vietnam",
    "Zambia"="Zambia",
    "Zimbabwe"="Zimbabwe"
  )
}