#---------------------------------------------------------------------------------------------------
# PGLS: determine if there are differences among 20 trees
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------
# Save the 20 trees:
#---------------------------------------------------------------------------------------------------

#Write ".tree" document based on Stein et al. (2018), and available at Vertlife.org
# Define the Newick tree string
tree_strings <- c(
  tree1 <- "((((Torpedo_marmorata:214.85005317449998,((Pteromylaeus_bovinus:91.50245422,Myliobatis_aquila:91.502454218):32.800000464,(Pteroplatytrygon_violacea:67.38805183400001,Dasyatis_pastinaca:67.388052835):56.914401848):90.54759749):5.390461274,(Dipturus_oxyrinchus:64.16484737670001,((Raja_clavata:24.346123428,Raja_asterias:24.346123427):16.25614888,Raja_polystigma:40.60227231100001):23.562577063):156.07566608):45.93316368,((Hexanchus_griseus:229.03500451699998,(((Etmopterus_spinax:147.770477239,Dalatias_licha:147.77047624):1.426077166,Oxynotus_centrina:149.19655441089998):8.958236332,Centrophorus_granulosus:158.154790725):70.88021277):13.33747358,(Scyliorhinus_canicula:167.34385451999998,Galeus_melastomus:167.34385350099998):75.02862358):23.80120104):102.930732,Chimaera_monstrosa:369.104410122);",
  tree2 <- "((((Torpedo_marmorata:191.36374479199998,((Dasyatis_pastinaca:38.9914514304,Pteroplatytrygon_violacea:38.991450434):59.773013538,(Pteromylaeus_bovinus:73.30883299,Myliobatis_aquila:73.30883299999999):25.455631971000003):92.599278816):4.262637766,((Raja_polystigma:33.981406285199995,(Raja_asterias:8.45077901066,Raja_clavata:8.450778011):25.530627277):8.986935983,Dipturus_oxyrinchus:42.968342270600004):152.65803928799997):42.56409644,((Hexanchus_griseus:189.12185319399998,(((Dalatias_licha:76.91510284499999,Etmopterus_spinax:76.9151008515):8.015863581,Oxynotus_centrina:84.93096542772):6.343034832,Centrophorus_granulosus:91.274000259):97.84785193):13.24642541,(Scyliorhinus_canicula:126.516757344,Galeus_melastomus:126.5167553547):75.85152225):35.82220039):118.47474,Chimaera_monstrosa:356.665219974);",
  tree3 <- "((((Torpedo_marmorata:231.2630943853,((Dasyatis_pastinaca:64.070088858,Pteroplatytrygon_violacea:64.070088863):64.136016042,(Pteromylaeus_bovinus:99.14472273300001,Myliobatis_aquila:99.144721739):29.061382158):103.05699048599999):4.910247326,(((Raja_clavata:27.8821758342,Raja_asterias:27.882175828999998):16.42603768,Raja_polystigma:44.30821351):26.742760004,Dipturus_oxyrinchus:71.05097251800001):165.1223691915):48.56759027,((Hexanchus_griseus:246.44739588600004,(((Dalatias_licha:152.75381932399998,Etmopterus_spinax:152.7538203334):8.685532417,Oxynotus_centrina:161.43935075579998):9.990968014,Centrophorus_granulosus:171.43032076):75.01707612999999):13.8736089,(Scyliorhinus_canicula:182.237344236,Galeus_melastomus:182.237343249):78.083662542):24.41992819):102.93531,Chimaera_monstrosa:387.676241992);",
  tree4 <- "((((Torpedo_marmorata:222.080302921,((Pteromylaeus_bovinus:94.010871058,Myliobatis_aquila:94.01087106439999):25.474538993,(Dasyatis_pastinaca:69.721768204,Pteroplatytrygon_violacea:69.721768209):49.76364284399999):102.59489387200001):5.019626707,(((Raja_polystigma:11.6324452268,Raja_clavata:11.632446226999999):16.08446541,Raja_asterias:27.716911638):42.382095462,Dipturus_oxyrinchus:70.0990060926):157.00092454):48.08871771,((Hexanchus_griseus:237.8813559292,(((Etmopterus_spinax:145.374606396,Dalatias_licha:145.374605406):8.353168438,Oxynotus_centrina:153.72777385348002):9.987855771,Centrophorus_granulosus:163.715630609):74.16572529999999):14.37788103,(Scyliorhinus_canicula:171.9291187037,Galeus_melastomus:171.929120699):80.33011623600001):22.92941141):100.3486815,Chimaera_monstrosa:375.53732984769994);",
  tree5 <-  "((((Torpedo_marmorata:240.193710024,((Pteroplatytrygon_violacea:67.993612958,Dasyatis_pastinaca:67.99361295):71.69695301799999,(Pteromylaeus_bovinus:103.16026020410001,Myliobatis_aquila:103.16025919860999):36.5303067606):100.50314305399999):5.233642871,(((Raja_asterias:29.77929281,Raja_clavata:29.7792928093):22.908198180999996,Raja_polystigma:52.687490993000004):23.52998175,Dipturus_oxyrinchus:76.21747374219999):169.20987915):48.99610098,((Hexanchus_griseus:256.180880249,(((Dalatias_licha:156.32981350699998,Etmopterus_spinax:156.32981351):10.90230156,Oxynotus_centrina:167.23211605803002):10.0270611,Centrophorus_granulosus:177.25917517):78.92170407):14.18391233,(Scyliorhinus_canicula:187.03260729800002,Galeus_melastomus:187.032608305):83.332185245):24.05866132):109.2560613,Chimaera_monstrosa:403.679513095);",
  tree6 <- "((((Torpedo_marmorata:231.506801199,((Pteromylaeus_bovinus:94.983172239,Myliobatis_aquila:94.983171239):29.684472069,(Dasyatis_pastinaca:78.493803203,Pteroplatytrygon_violacea:78.49380221):46.173842107):106.839157904):26.85867397,(Dipturus_oxyrinchus:68.71936468850001,((Raja_clavata:9.867964464,Raja_asterias:9.867963463999999):1.463249678,Raja_polystigma:11.331213142):57.388150554999996):189.64611147000002):36.05371235,((Hexanchus_griseus:238.72348449400002,(((Etmopterus_spinax:146.100443369,Dalatias_licha:146.100444363):8.90374864,Oxynotus_centrina:155.00419300398):11.06383486,Centrophorus_granulosus:166.068027865):72.6554566):15.37025687,(Scyliorhinus_canicula:173.047315023,Galeus_melastomus:173.04731703047003):81.0464263131):40.32544621):80.32561439,Chimaera_monstrosa:374.74480090158);",
  tree7 <- "((((Torpedo_marmorata:273.3730632435,((Dasyatis_pastinaca:94.823765816,Pteroplatytrygon_violacea:94.823766827):65.155146301,(Pteromylaeus_bovinus:119.89920164,Myliobatis_aquila:119.89920062600001):40.079711494):113.394150111):6.61551065,(((Raja_clavata:37.7110972966,Raja_asterias:37.7110973):23.81757357,Raja_polystigma:61.52867186539999):29.09725658,Dipturus_oxyrinchus:90.62592844400001):189.36264649):58.05705387,((Hexanchus_griseus:291.38842919999996,(((Dalatias_licha:187.06960195,Etmopterus_spinax:187.06959995091):5.544505176,Oxynotus_centrina:192.6141081091):10.87238305,Centrophorus_granulosus:203.48649117250002):87.90193902):23.242437,(Scyliorhinus_canicula:214.319729056,Galeus_melastomus:214.31972803300002):100.31113715):23.41476158):112.1655526,Chimaera_monstrosa:450.21118036300004);",
  tree8 <- "((((Torpedo_marmorata:206.5743511384,((Pteromylaeus_bovinus:87.80873195999999,Myliobatis_aquila:87.80873195240001):31.722087182000003,(Pteroplatytrygon_violacea:64.98544968799999,Dasyatis_pastinaca:64.98544869):54.545370448999996):87.043529998):4.866797146,((Raja_polystigma:40.441940591999995,(Raja_clavata:22.838403233,Raja_asterias:22.83840323):17.60353735639):21.06438388,Dipturus_oxyrinchus:61.506323468400005):149.93482281000001):43.68548306,((Hexanchus_griseus:219.857057649,(((Dalatias_licha:141.50647606,Etmopterus_spinax:141.50647406500002):2.423405183,Oxynotus_centrina:143.9298822413):8.63233688,Centrophorus_granulosus:152.562217131):67.29484052):12.42198353,(Scyliorhinus_canicula:160.196695832,Galeus_melastomus:160.196696839):72.082344357):22.84758816):98.5326035,Chimaera_monstrosa:353.659232858);",
  tree9 <-   "((((Torpedo_marmorata:222.970004212,((Pteroplatytrygon_violacea:73.335884392,Dasyatis_pastinaca:73.335884393):55.2784386092,(Pteromylaeus_bovinus:95.9927524,Myliobatis_aquila:95.9927514):32.621571611):94.3556852053):5.761151301,((Raja_polystigma:48.9116696497,(Raja_clavata:25.816191818,Raja_asterias:25.816191816):23.095477835):17.82920707,Dipturus_oxyrinchus:66.7408767228):161.990279809):46.33765353,((Hexanchus_griseus:236.86444726699997,(((Dalatias_licha:143.968992285,Etmopterus_spinax:143.96899328400002):11.71467615,Oxynotus_centrina:155.6836684306):8.677096429,Centrophorus_granulosus:164.36076485):72.50368141999999):14.70002094,(Scyliorhinus_canicula:176.19697030560002,Galeus_melastomus:176.1969673017):75.3674999):23.50434185):100.3808548,Chimaera_monstrosa:375.44966586099997);",
  tree10 <- "((((Torpedo_marmorata:212.09634995200003,((Dasyatis_pastinaca:68.344760298,Pteroplatytrygon_violacea:68.344760307):54.35461743700001,(Pteromylaeus_bovinus:90.47903065999999,Myliobatis_aquila:90.47902966000001):32.220346069):89.396972207):4.885293384,(Dipturus_oxyrinchus:64.285870133,(Raja_asterias:24.59363817,(Raja_polystigma:23.213020259,Raja_clavata:23.213021259):1.3806169143):39.69223195):152.6957722):44.20225491,((Hexanchus_griseus:226.84364924600004,(((Dalatias_licha:142.85896994499998,Etmopterus_spinax:142.858967946):4.351481452,Oxynotus_centrina:147.21045140057998):9.18853952,Centrophorus_granulosus:156.398989922):70.44465832):12.51524822,(Scyliorhinus_canicula:164.816404051,Galeus_melastomus:164.816406055):74.5424924):21.82499978):96.35648548,Chimaera_monstrosa:357.54038271400003);",
  tree11 <- "((((Torpedo_marmorata:235.39056833600003,((Dasyatis_pastinaca:62.43337808700001,Pteroplatytrygon_violacea:62.43337808300001):74.7738740093,(Pteromylaeus_bovinus:102.06409989707,Myliobatis_aquila:102.064099895):35.143151194):98.18331825):6.016333717,(((Raja_polystigma:11.264205203,Raja_clavata:11.264206203):17.807526885,Raja_asterias:29.071732092999998):43.4641782484,Dipturus_oxyrinchus:72.535910339):168.8709917):50.23492327,((Hexanchus_griseus:247.541058357,(((Etmopterus_spinax:161.7071478786,Dalatias_licha:161.70714688):4.09213295,Oxynotus_centrina:165.7992808291):9.544316155,Centrophorus_granulosus:175.343596986):72.19746234):18.87831019,(Scyliorhinus_canicula:184.71151904699997,Galeus_melastomus:184.71151806719996):81.707849455):25.2224568):101.5756261,Chimaera_monstrosa:393.21745241199994);",
  tree12 <- "((((Torpedo_marmorata:222.305024612073,((Pteromylaeus_bovinus:94.53752957500001,Myliobatis_aquila:94.537530589):33.783604531,(Dasyatis_pastinaca:69.72199914,Pteroplatytrygon_violacea:69.721998142):58.5991369684):93.983889504):4.906749766,((Raja_asterias:25.810217351000002,(Raja_polystigma:11.84524073,Raja_clavata:11.845240728):13.964976621):40.970539959,Dipturus_oxyrinchus:66.780758303):160.43101709):45.91073975,((Hexanchus_griseus:236.88724661500004,(((Dalatias_licha:148.52945143800002,Etmopterus_spinax:148.5294514328):8.650367357,Oxynotus_centrina:157.17981978891999):9.167066471,Centrophorus_granulosus:166.346886257):70.54036135999999):13.68055292,(Scyliorhinus_canicula:174.75937657420002,Galeus_melastomus:174.759376579):75.80842295059999):22.5547136):101.0447079,Chimaera_monstrosa:374.167219974);",
  tree13 <- "((((Torpedo_marmorata:248.94388989599997,((Dasyatis_pastinaca:82.87272149,Pteroplatytrygon_violacea:82.87272249399999):56.463527285,(Pteromylaeus_bovinus:103.517155892,Myliobatis_aquila:103.517155891):35.81909389):109.60764114):11.69954733,(((Raja_clavata:22.324180389800002,Raja_asterias:22.324181392):31.111349,Raja_polystigma:53.435531389999994):22.32690211,Dipturus_oxyrinchus:75.7624324999):184.88100376999998):46.56410202,((Hexanchus_griseus:258.688736535,(((Dalatias_licha:165.37557185999998,Etmopterus_spinax:165.375571857):1.326411936,Oxynotus_centrina:166.70198377999998):10.43312258,Centrophorus_granulosus:177.135106375):81.55363016999999):21.03918552,(Scyliorhinus_canicula:186.59217131399998,Galeus_melastomus:186.592171301):93.13575076000001):27.47961817):96.13347115,Chimaera_monstrosa:403.341011397);",
  tree14 <- "((((Torpedo_marmorata:195.63038084200002,((Pteromylaeus_bovinus:74.741010213,Myliobatis_aquila:74.741011195):33.297590977,(Dasyatis_pastinaca:35.86202692,Pteroplatytrygon_violacea:35.862026919):72.176573259):87.591779665):4.218630208,(Dipturus_oxyrinchus:43.654593696700005,((Raja_clavata:15.290265369,Raja_asterias:15.290264372):1.032039294,Raja_polystigma:16.322304662999997):27.332290033):156.19441636800002):44.33463709,((Hexanchus_griseus:190.29510438699998,(((Dalatias_licha:83.86920574300001,Etmopterus_spinax:83.869205736):2.542028856,Oxynotus_centrina:86.4112355913):6.267271448,Centrophorus_granulosus:92.67850703900001):97.61659735):14.47937931,(Scyliorhinus_canicula:127.482751928,Galeus_melastomus:127.48275192999301):77.29173177000001):39.40916345):128.750967,Chimaera_monstrosa:372.93461511500004);",
  tree15 <- "((((Torpedo_marmorata:217.1607459221,((Dasyatis_pastinaca:57.1630837924,Pteroplatytrygon_violacea:57.1630838):64.684515823,(Pteromylaeus_bovinus:89.915715035,Myliobatis_aquila:89.91571402099999):31.931882592):95.313147285):15.12493216,(((Raja_clavata:7.213394305,Raja_polystigma:7.213394305):16.358815722,Raja_asterias:23.57221003036):37.00361923,Dipturus_oxyrinchus:60.575828259999994):171.70984783999998):38.31562617,((Hexanchus_griseus:233.94201252,(((Dalatias_licha:132.1306289153,Etmopterus_spinax:132.13062792800002):15.66518608,Oxynotus_centrina:147.79581399580002):10.97125881,Centrophorus_granulosus:158.76707480480002):75.17493970999999):15.34168233,(Scyliorhinus_canicula:166.576460639,Galeus_melastomus:166.57645963262):82.70723521400001):21.31760838):90.94037598,Chimaera_monstrosa:361.54168017999996);",
  tree16 <- "((((Torpedo_marmorata:219.45075396599998,((Pteroplatytrygon_violacea:56.02995269959999,Dasyatis_pastinaca:56.029952699999996):70.376810415,(Pteromylaeus_bovinus:93.30481255000001,Myliobatis_aquila:93.30481254200001):33.101950579):93.04399084199999):5.913002497,((Raja_polystigma:49.35622041,(Raja_asterias:24.88830944,Raja_clavata:24.888309446):24.467910961999998):16.02133232,Dipturus_oxyrinchus:65.37755373300001):159.986202733):46.61326396,((Hexanchus_griseus:235.942959076,(((Dalatias_licha:142.37436607900003,Etmopterus_spinax:142.374366077):11.88801746,Oxynotus_centrina:154.2623845369):9.204284772,Centrophorus_granulosus:163.4666683095):72.47628976):13.47873186,(Scyliorhinus_canicula:173.17964353,Galeus_melastomus:173.1796435264):76.2420474):22.5553295):100.7753023,Chimaera_monstrosa:372.752320662);",
  tree17 <- "((((Torpedo_marmorata:215.837275512,((Pteroplatytrygon_violacea:57.622952204,Dasyatis_pastinaca:57.622953202):67.1304548998,(Pteromylaeus_bovinus:90.91930641345999,Myliobatis_aquila:90.919306415):33.834101704000005):91.083867405):5.089417668,(Dipturus_oxyrinchus:63.8640385101,(Raja_polystigma:24.055368125,(Raja_asterias:3.000629064,Raja_clavata:3.000630064):21.054738058):39.808671389):157.06265467999998):45.19053505,((Hexanchus_griseus:229.902457489,(((Etmopterus_spinax:139.070837872,Dalatias_licha:139.07083888600002):10.55353118,Oxynotus_centrina:149.6243700653):9.225535349,Centrophorus_granulosus:158.84990542):71.052552081):13.45600389,(Scyliorhinus_canicula:167.13521623280002,Galeus_melastomus:167.1352152451):76.22324614):22.75876684):101.8647784,Chimaera_monstrosa:367.98200657999996);",
  tree18 <- "((((Torpedo_marmorata:222.450728121,((Pteromylaeus_bovinus:96.547369608,Myliobatis_aquila:96.547371607):35.199940655000006,(Dasyatis_pastinaca:76.06875802909998,Pteroplatytrygon_violacea:76.068757024):55.67855423):90.70341887800001):5.666544191,((Raja_polystigma:47.032964039999996,(Raja_asterias:25.075871561,Raja_clavata:25.075871551):21.957093479999997):18.834432862,Dipturus_oxyrinchus:65.8673968957):162.24987742):45.74765644,((Hexanchus_griseus:237.691764637,(((Etmopterus_spinax:150.5397219559,Dalatias_licha:150.53972195999998):7.969200557,Oxynotus_centrina:158.5089225242):9.668281922,Centrophorus_granulosus:168.177205442):69.51456121):13.41922109,(Scyliorhinus_canicula:174.055189853,Galeus_melastomus:174.05519085109998):77.05579588):22.75394301):100.5740727,Chimaera_monstrosa:374.43900248470004);",
  tree19 <- "((((Torpedo_marmorata:219.71472680009998,((Dasyatis_pastinaca:67.55114181,Pteroplatytrygon_violacea:67.5511408):60.07640502300001,(Pteromylaeus_bovinus:93.022314132,Myliobatis_aquila:93.02231413269999):34.605231699):92.087179966):12.91640923,(Dipturus_oxyrinchus:69.483209068,(Raja_clavata:27.849491847099998,(Raja_polystigma:5.57977882,Raja_asterias:5.5797788200000005):22.269713022):41.633717229000005):163.147926986):44.22966776,((Hexanchus_griseus:241.085575781,(((Oxynotus_centrina:161.8272496158,Etmopterus_spinax:161.8272516159):0.002704016643,Dalatias_licha:161.82995262999998):9.72264481,Centrophorus_granulosus:171.5525994486):69.53297629000001):10.26192049,(Scyliorhinus_canicula:171.03432845010002,Galeus_melastomus:171.03432746100003):80.31316877):25.51330757):93.36028704,Chimaera_monstrosa:370.22108985);",
  tree20 <- "((((Torpedo_marmorata:197.447334971,((Pteroplatytrygon_violacea:44.089373519000006,Dasyatis_pastinaca:44.089373528):65.9563140622,(Pteromylaeus_bovinus:75.418423776,Myliobatis_aquila:75.418424784):34.627261802999996):87.4016494015):4.239861695,(Dipturus_oxyrinchus:44.1321446085,((Raja_clavata:3.423724021,Raja_polystigma:3.4237230211):11.7886782689,Raja_asterias:15.212402289):28.9197433195):157.555053069):45.31436884,((Hexanchus_griseus:190.987841855,(((Dalatias_licha:79.13853274700001,Etmopterus_spinax:79.1385327438):7.537738246,Oxynotus_centrina:86.6762709838):6.528031497,Centrophorus_granulosus:93.2043014863):97.78354037):14.68528556,(Scyliorhinus_canicula:128.405879551,Galeus_melastomus:128.40588054699998):77.267248861):41.3284381):134.7145933,Chimaera_monstrosa:381.7161607968);"
)

output_dir  <- paste0(input_data, "/trees")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Loop through the tree strings, create phylogenetic tree objects, and save them
for (i in 1:length(tree_strings)) {
  tree <- read.tree(text = tree_strings[i])
  
  # Define the filename for the tree
  tree_filename <- paste0(output_dir, "/tree", i, ".tree")
  
  # Save the tree to a file
  write.tree(tree, file = tree_filename)
}

#---------------------------------------------------------------------------------------------------
# Load data
#---------------------------------------------------------------------------------------------------

# Fit the model using the phylogenic parameters reflecting the biology and ecology of each species:
setwd(input_data)
data <- read.csv("GLMM_predictors2.csv", sep = ";")
names(data)
head(data)

#Format new predictors:
names(data)
data <- data %>%
  mutate(mean_bodymass = as.numeric(gsub(",", ".", mean_bodymass)),  # Convert comma decimal to dot decimal
         mean_depth = as.numeric(gsub(",", ".", mean_depth)),
         mean_temperature = as.numeric(gsub(",", ".", mean_temperature)),
         mean_oxygen = as.numeric(gsub(",", ".", mean_oxygen)),
         AVM = as.numeric(gsub(",", ".", AVM)),
         PCM = as.numeric(gsub(",", ".", PCM)))

data <- data %>% 
  mutate(RespirationMode = factor(RespirationMode, c(1,2,4)))

data$lnmass <- log1p(data$mean_bodymass)

data <- data %>% 
  mutate(mean_bodymass = scale(lnmass),
         mean_depth = scale(mean_depth),
         mean_temperature = scale(mean_temperature),
         mean_oxygen = scale(mean_oxygen))

summary(data)

#---------------------------------------------------------------------------------------------------
# Loop the models through the 20 data sets with the 20 different trees:
#---------------------------------------------------------------------------------------------------

# List of input_data for the 20 trees
input_data_list <- lapply(1:20, function(i) {
  paste0("tree", i, "_data")
})

# Create a list to store the results for each tree
results_list <- list()
bestmodels <- list()
secbestmodels <- list()

# Set directory where the trees are:
wd <- paste0(input_data, "/trees")
setwd(wd)

# Create a list of tree filenames for tree1.tree through tree20.tree
output_dir  <- paste0(input_data, "/trees")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
tree_filenames <- lapply(1:20, function(i) {
  paste0(output_dir, "/tree", i, ".tree")
})


# Loop through the input data list and load the corresponding tree for each dataset
for (i in 1:length(input_data_list)) {
  input_data <- input_data_list[i]  # Get the current input data
  
  # Load the corresponding phylogenetic tree
  tree_filename <- tree_filenames[i]
  tree_filename <- gsub("\\\\", "/", tree_filename)
  print(paste("Checking tree file:", tree_filename))  # Debug statement
  
  if (file.exists(tree_filename)) {
  
    phy <- read.tree(tree_filename)
  
  # Perform your analysis using phy
  cd <- comparative.data(phy, data, names.col = "Species")
  
  predictors <- c("mean_bodymass", "mean_depth", "mean_oxygen")
  
  # Create and store the models
  #models <- list()
  
  ## Loop through all possible combinations of predictors #CHANGE TO INCLUDE INTERACTION
  #for (i in 1:(2^length(predictors) - 1)) {
  #  binary_vector <- intToBits(i)[1:length(predictors)]
  #  selected_predictors <- predictors[as.logical(binary_vector)]
  #  formula <- as.formula(paste("AVM ~", paste(selected_predictors, collapse = " + ")))
  #  model <- pgls(formula, data = cd, lambda = "ML")
  #  models[[i]] <- model
  #}
  
  # 0 intercept only
  zslope_0 <- pgls(AVM ~ 1, data = cd, lambda = "ML")
  
  # 1. mass only: AVM varies with mass only 
  zslope_wt <- pgls(AVM ~ mean_bodymass, data = cd, lambda = "ML") 
  
  # 2. mass only: AVM varies with depth-temp index only 
  zslope_depthtemp <- pgls(AVM ~ mean_depth, data = cd, lambda = "ML") 
  
  # 3. mass only: AVM varies with oxy only 
  zslope_oxy <- pgls(AVM ~ mean_oxygen, data = cd, lambda = "ML") 
  
  # 4. mean_bodymass + Oxy 
  zslope_mass_oxy <- pgls(AVM ~ mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 
  
  # 5. mean_bodymass + depthtemp 
  zslope_mass_depthtemp  <- pgls(AVM ~ mean_bodymass + mean_depth, data = cd, lambda = "ML") 
  
  # 6. depthtemp + Oxy
  zslope_oxy_depthtemp  <- pgls(AVM ~ mean_depth + mean_oxygen, data = cd, lambda = "ML") 
  
  # 7. depthtemp * mass
  zslope_mass_int_depthtemp  <- pgls(AVM ~ mean_depth * mean_bodymass, data = cd, lambda = "ML") 
  
 # 8.  mass * depthtemp + oxy
  zslope_mass_int_depthtem_oxy  <- pgls(AVM ~ mean_depth * mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 
  
  # 9.  mass + depthtemp * oxy
  zslope_oxy_depthtemp_mass  <- pgls(AVM ~ mean_depth + mean_oxygen + mean_bodymass, data = cd, lambda = "ML") 
  
  models <- list(
    "Intercept Only" = zslope_0,
    "mass only" = zslope_wt,
    "depthtemp only" = zslope_depthtemp,
    "oxy only" = zslope_oxy,
    "mass + Oxy" = zslope_mass_oxy,
    "mass + depthtemp" = zslope_mass_depthtemp,
    "depthtemp + Oxy" = zslope_oxy_depthtemp,
    "depthtemp * mass" = zslope_mass_int_depthtemp,
    "mass * depthtemp + oxy" = zslope_mass_int_depthtem_oxy,
    "mass + depthtemp + oxy" = zslope_oxy_depthtemp_mass
  )
  
  # manually extracting information from each model object
  aics <- sapply(models, function (x) bbmle::AIC(x)) 
  aiccs <- sapply(models, function (x) x$aicc) 
  formulas <- sapply(models,   # tidying formulas for easier reading
                     function (x) deparse(formula(x), width.cutoff = 90L)) %>%
    sub("^log\\(\\w+\\)\\s\\~\\s", "", .) %>% 
    gsub("_scaled", "", .) %>%
    gsub("\\_wt", "(M)", .) 
  r2s <- sapply(models, function (x) summary(x)$r.squared) 
  ar2s <- sapply(models, function (x) summary(x)$adj.r.squared)
  LL <- sapply(models, function (x) x$model$log.lik) 
  ks <- sapply(models, function (x) x$k) 
  
  models_table <- data.frame(formulas, ks, LL, aics, aiccs, r2s, ar2s) %>%
    rename(Model = formulas, n = ks, AIC = aics,
           AICc = aiccs, R_sq = r2s, adj_R_sq = ar2s) %>%
    mutate(Model = as.character(formulas), 
           LL = round(LL, 1), 
           AIC = round(AIC, 1), AICc = round(AICc, 1), 
           dAIC = round(AIC - min(AIC), 2),
           dAICc = round(AICc - min(AICc), 2),
           R_sq = round(R_sq, 2), adj_R_sq = round(adj_R_sq, 2), 
           Weights = round(exp(-dAICc/2)/sum(exp(-dAICc/2)), 3)) 

  ### Top-ranked model diagnostic plots and analyses
  topmod <- which(models_table$dAICc == 0)
  bestmodel <- models[[topmod]]
  
  #Save also 2nd best:
  # Sort the models_table by dAICc in ascending order
  sorted_models <- models_table[order(models_table$dAICc), ]
  # Select the model with the second lowest value of dAICc
  second_lowest_dAICc_model <- sorted_models[2, ]
  # Retrieve the model index for the second lowest dAICc model
  secmodel <- which(models_table$dAICc == second_lowest_dAICc_model$dAICc)
  secbestmodel <- models[[secmodel]]
  
  # Store the results for the current tree in the results list
  results_list[[i]] <- models_table  # Store models_table for the current tree
  bestmodels[[i]] <- bestmodel
  secbestmodels[[i]] <- secbestmodel
  } else {
    cat("Tree file does not exist:", tree_filename, "\n")
  }
  }

print(results_list[[1]]) #4
print(bestmodels)
print(secbestmodels[[1]])
summary(bestmodels[[1]])

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Save data from models:
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create a list to store the results of pgls.profile() for each best model
pgls_profile_results <- list()

# Loop through the best models and apply pgls.profile()
for (i in 1:length(bestmodels)) {
  bestmodel <- bestmodels[[i]]  # Get the current best model
  
  # profile plot of lambda for PGLS model
  profile_result <- pgls.profile(bestmodel)  # lambda closer to 1 indicates strong phylogenetic signal
  pgls_profile_results[[i]] <- profile_result
  
  #print(paste("Profile plot for best model", i, ":", profile_result))
}

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 95% CI intervals for variance among models in coefficients: 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Calculate 95% confidence intervals for the coefficient estimates
# Extract coefficients for each model from bestmodels
coefs_list <- lapply(bestmodels, function(model) {
  coef_values <- coef(model)
  data.frame(
    Intercept = coef_values[1],
    Coefficient_2 = coef_values[2],
    Coefficient_3 = coef_values[3]
    )
})

# Combine the coefficient data for all models
coefficients_df <- do.call(rbind, coefs_list)

stacked_df <- stack(coefficients_df)
# Renaming the resulting columns
colnames(stacked_df) <- c("value", "coef")

# Calculate median and CI per variable, grouped by the "var" column
stacked_df_sum <- stacked_df %>%
  dplyr::group_by(coef) %>%
  dplyr::summarize(median = median(value),
            cil = quantile(value, prob = 0.025),
            ciu = quantile(value, prob = 0.975))

# Print the summary dataframe
print(stacked_df_sum)

#Calcualte CI and Mean and save it to dataframe:
result_df <- stacked_df %>%
  dplyr::group_by(coef) %>%
  dplyr::summarise(
    mean = mean(value),
    median = median(value),
    cil = quantile(value, prob = 0.025),
    ciu = quantile(value, prob = 0.975)
  ) 

result_df

#Plot:
result_df$coef <- factor(result_df$coef, levels = result_df$coef)

p <- ggplot(data=result_df, mapping=aes(x=coef, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="#377EB8") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

print(p)

# Export the plot
outfile <- paste0(output_data, "/Coeff_CI_95.png")
ggsave(outfile, p, width=15.8, height=18.4, units="cm", dpi=300)


#Repeat for second best model:
# Create a list to store the results of pgls.profile() for each best model
pgls_profile_results <- list()

# Loop through the best models and apply pgls.profile()
for (i in 1:length(secbestmodels)) {
  secbestmodel <- secbestmodels[[i]]  # Get the current best model
  
  # profile plot of lambda for PGLS model
  profile_result <- pgls.profile(secbestmodel)  # lambda closer to 1 indicates strong phylogenetic signal
  pgls_profile_results[[i]] <- profile_result
  
  print(paste("Profile plot for best model", i, ":", profile_result))
}


#Calculate 95% confidence intervals for the coefficient estimates
# Extract coefficients for each model from secbestmodels
coefs_list <- lapply(secbestmodels, function(model) {
  coef_values <- coef(model)
  data.frame(
    Intercept = coef_values[1],
    Coefficient_2 = coef_values[2],
    Coefficient_3 = coef_values[3]
  )
})

# Combine the coefficient data for all models
coefficients_df <- do.call(rbind, coefs_list)

stacked_df <- stack(coefficients_df)
# Renaming the resulting columns
colnames(stacked_df) <- c("value", "coef")

# Calculate median and CI per variable, grouped by the "var" column
stacked_df_sum <- stacked_df %>%
  group_by(coef) %>%
  summarize(median = median(value),
            cil = quantile(value, prob = 0.025),
            ciu = quantile(value, prob = 0.975))

# Print the summary dataframe
print(stacked_df_sum)

#Calcualte CI and Mean and save it to dataframe:
result_df <- stacked_df %>%
  group_by(coef) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    cil = quantile(value, prob = 0.025),
    ciu = quantile(value, prob = 0.975)
  ) 

result_df

#Plot:

result_df$coef <- factor(result_df$coef, levels = result_df$coef)

p <- ggplot(data=result_df, mapping=aes(x=coef, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="#377EB8") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

# Export the plot
outfile <- paste0(output_data, "/Model2_Coeff_CI_95.png")
ggsave(outfile, p, width=15.8, height=18.4, units="cm", dpi=300)

#---------------------------------------------------------------------------------------------------
# Repeat with PCM as reponse variable:
#---------------------------------------------------------------------------------------------------
# Fit the model using the phylogenic parameters reflecting the biology and ecology of each species:
setwd(input_data)
data <- read.csv("GLMM_predictors2.csv", sep = ";")
names(data)
head(data)

#Format new predictors:
names(data)
data <- data %>%
  mutate(mean_bodymass = as.numeric(gsub(",", ".", mean_bodymass)),  # Convert comma decimal to dot decimal
         mean_depth = as.numeric(gsub(",", ".", mean_depth)),
         mean_temperature = as.numeric(gsub(",", ".", mean_temperature)),
         mean_oxygen = as.numeric(gsub(",", ".", mean_oxygen)),
         AVM = as.numeric(gsub(",", ".", AVM)),
         PCM = as.numeric(gsub(",", ".", PCM)))

data <- data %>% 
  mutate(RespirationMode = factor(RespirationMode, c(1,2,4)))

data$lnmass <- log1p(data$mean_bodymass)

data <- data %>% 
  mutate(mean_bodymass = scale(lnmass),
         mean_depth = scale(mean_depth),
         mean_temperature = scale(mean_temperature),
         mean_oxygen = scale(mean_oxygen))

summary(data)

#Scale parameters:
data <- data %>% 
  mutate(mean_bodymass = scale(lnmass),
         mean_depth = scale(mean_depth),
         mean_temperature = scale(mean_temperature),
         mean_oxygen = scale(mean_oxygen))

summary(data)

# List of input_data for the 20 trees
input_data_list <- lapply(1:20, function(i) {
  paste0("tree", i, "_data")
})

# Create a list to store the results for each tree
results_list <- list()
bestmodels <- list()
secbestmodels <- list()

# Set directory where the trees are:
wd <- paste0(input_data, "/trees")
setwd(wd)

# Create a list of tree filenames for tree1.tree through tree20.tree
output_dir  <- paste0(input_data, "/trees")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
tree_filenames <- lapply(1:20, function(i) {
  paste0(output_dir, "/tree", i, ".tree")
})


# Loop through the input data list and load the corresponding tree for each dataset
for (i in 1:length(input_data_list)) {
  input_data <- input_data_list[i]  # Get the current input data
  
  # Load the corresponding phylogenetic tree
  tree_filename <- tree_filenames[i]
  tree_filename <- gsub("\\\\", "/", tree_filename)
  print(paste("Checking tree file:", tree_filename))  # Debug statement
  
  if (file.exists(tree_filename)) {
    
    phy <- read.tree(tree_filename)
    
    # Perform your analysis using phy
    cd <- comparative.data(phy, data, names.col = "Species")
    
    predictors <- c("mean_bodymass", "mean_depth", "mean_oxygen")
    
    # Create and store the models
    #models <- list()
    
    ## Loop through all possible combinations of predictors #CHANGE TO INCLUDE INTERACTION
    #for (i in 1:(2^length(predictors) - 1)) {
    #  binary_vector <- intToBits(i)[1:length(predictors)]
    #  selected_predictors <- predictors[as.logical(binary_vector)]
    #  formula <- as.formula(paste("AVM ~", paste(selected_predictors, collapse = " + ")))
    #  model <- pgls(formula, data = cd, lambda = "ML")
    #  models[[i]] <- model
    #}
    
    # 0 intercept only
    PCMslope_0 <- pgls(PCM ~ 1, data = cd, lambda = "ML")
    
    # 1. mass only: AVM varies with mass only 
    PCMslope_wt <- pgls(PCM ~ mean_bodymass, data = cd, lambda = "ML") 
    
    # 2. mass only: AVM varies with depth-temp index only 
    PCMslope_depthtemp <- pgls(PCM ~ mean_depth, data = cd, lambda = "ML") 
    
    # 3. mass only: AVM varies with oxy only 
    PCMslope_oxy <- pgls(PCM ~ mean_oxygen, data = cd, lambda = "ML") 
    
    # 4. mean_bodymass + Oxy 
    PCMslope_mass_oxy <- pgls(PCM ~ mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 
    
    # 5. mean_bodymass + depthtemp 
    PCMslope_mass_depthtemp  <- pgls(PCM ~ mean_bodymass + mean_depth, data = cd, lambda = "ML") 
    
    # 6. depthtemp + Oxy
    PCMslope_oxy_depthtemp  <- pgls(PCM ~ mean_depth + mean_oxygen, data = cd, lambda = "ML") 
    
    # 7. depthtemp * mass
    PCMslope_mass_int_depthtemp  <- pgls(PCM ~ mean_depth * mean_bodymass, data = cd, lambda = "ML") 
    
    # 8.  mass * depthtemp + oxy
    PCMslope_mass_int_depthtem_oxy  <- pgls(PCM ~ mean_depth * mean_bodymass + mean_oxygen, data = cd, lambda = "ML") 
    
    # 9.  mass + depthtemp * oxy
    PCMslope_oxy_depthtemp_mass  <- pgls(PCM ~ mean_depth + mean_oxygen + mean_bodymass, data = cd, lambda = "ML") 
    
    PCMmodels <- list(
      "Intercept Only" = PCMslope_0,
      "mass only" = PCMslope_wt,
      "depthtemp only" = PCMslope_depthtemp,
      "oxy only" = PCMslope_oxy,
      "mass + Oxy" = PCMslope_mass_oxy,
      "mass + depthtemp" = PCMslope_mass_depthtemp,
      "depthtemp + Oxy" = PCMslope_oxy_depthtemp,
      "depthtemp * mass" = PCMslope_mass_int_depthtemp,
      "mass * depthtemp + oxy" = PCMslope_mass_int_depthtem_oxy,
      "mass + depthtemp + oxy" = PCMslope_oxy_depthtemp_mass
    )
    
    # manually extracting information from each model object
    aics <- sapply(PCMmodels, function (x) bbmle::AIC(x)) 
    aiccs <- sapply(PCMmodels, function (x) x$aicc) 
    formulas <- sapply(PCMmodels,   # tidying formulas for easier reading
                       function (x) deparse(formula(x), width.cutoff = 90L)) %>%
      sub("^log\\(\\w+\\)\\s\\~\\s", "", .) %>% 
      gsub("_scaled", "", .) %>%
      gsub("\\_wt", "(M)", .) 
    r2s <- sapply(PCMmodels, function (x) summary(x)$r.squared) 
    ar2s <- sapply(PCMmodels, function (x) summary(x)$adj.r.squared)
    LL <- sapply(PCMmodels, function (x) x$model$log.lik) 
    ks <- sapply(PCMmodels, function (x) x$k) 
    
    PCMmodels_table <- data.frame(formulas, ks, LL, aics, aiccs, r2s, ar2s) %>%
      rename(Model = formulas, n = ks, AIC = aics,
             AICc = aiccs, R_sq = r2s, adj_R_sq = ar2s) %>%
      mutate(Model = as.character(formulas), 
             LL = round(LL, 1), 
             AIC = round(AIC, 1), AICc = round(AICc, 1), 
             dAIC = round(AIC - min(AIC), 2),
             dAICc = round(AICc - min(AICc), 2),
             R_sq = round(R_sq, 2), adj_R_sq = round(adj_R_sq, 2), 
             Weights = round(exp(-dAICc/2)/sum(exp(-dAICc/2)), 3)) 
    
    ### Top-ranked model diagnostic plots and analyses
    PCMtopmod <- which(PCMmodels_table$dAICc == 0)
    bestmodel <- PCMmodels[[PCMtopmod]]
    
    #Save also 2nd best:
    # Sort the models_table by dAICc in ascending order
    sorted_models <- PCMmodels_table[order(PCMmodels_table$dAICc), ]
    # Select the model with the second lowest value of dAICc
    second_lowest_dAICc_model <- sorted_models[2, ]
    # Retrieve the model index for the second lowest dAICc model
    secmodel <- which(PCMmodels_table$dAICc == second_lowest_dAICc_model$dAICc)
    secbestmodel <- PCMmodels[[secmodel]]
    
    # Store the results for the current tree in the results list
    results_list[[i]] <- PCMmodels_table  # Store models_table for the current tree
    bestmodels[[i]] <- bestmodel
    secbestmodels[[i]] <- secbestmodel
  } else {
    cat("Tree file does not exist:", tree_filename, "\n")
  }
}

print(results_list[[16]]) #4
print(bestmodels)
print(secbestmodels[[1]])
summary(bestmodels[[16]])

# Create a list to store the results of pgls.profile() for each best model
pgls_profile_results <- list()

# Loop through the best models and apply pgls.profile()
for (i in 1:length(bestmodels)) {
  bestmodel <- bestmodels[[i]]  # Get the current best model
  
  # profile plot of lambda for PGLS model
  profile_result <- pgls.profile(bestmodel)  # lambda closer to 1 indicates strong phylogenetic signal
  pgls_profile_results[[i]] <- profile_result
  
  #print(paste("Profile plot for best model", i, ":", profile_result))
}

#Calculate 95% confidence intervals for the coefficient estimates
# Extract coefficients for each model from bestmodels
coefs_list <- lapply(bestmodels, function(model) {
  coef_values <- coef(model)
  data.frame(
    Intercept = coef_values[1],
    Coefficient_2 = coef_values[2],
    Coefficient_3 = coef_values[3],
    Coefficient_4 = coef_values[4]
  )
})

# Combine the coefficient data for all models
coefficients_df <- do.call(rbind, coefs_list)

stacked_df <- stack(coefficients_df)
# Renaming the resulting columns
colnames(stacked_df) <- c("value", "coef")

# Calculate median and CI per variable, grouped by the "var" column
stacked_df_sum <- stacked_df %>%
  dplyr::group_by(coef) %>%
  dplyr::summarize(median = median(value),
                   cil = quantile(value, prob = 0.025),
                   ciu = quantile(value, prob = 0.975))

# Print the summary dataframe
print(stacked_df_sum)

#Calcualte CI and Mean and save it to dataframe:
result_df <- stacked_df %>%
  dplyr::group_by(coef) %>%
  dplyr::summarise(
    mean = mean(value),
    median = median(value),
    cil = quantile(value, prob = 0.025),
    ciu = quantile(value, prob = 0.975)
  ) 

result_df

#Plot:
result_df$coef <- factor(result_df$coef, levels = result_df$coef)

p <- ggplot(data=result_df, mapping=aes(x=coef, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="#377EB8") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

print(p)

# Export the plot
outfile <- paste0(output_data, "/Coeff_CI_95_PCM.png")
ggsave(outfile, p, width=15.8, height=18.4, units="cm", dpi=300)


#Repeat for second best model:
# Create a list to store the results of pgls.profile() for each best model
pgls_profile_results <- list()

# Loop through the best models and apply pgls.profile()
for (i in 1:length(secbestmodels)) {
  secbestmodel <- secbestmodels[[i]]  # Get the current best model
  
  # profile plot of lambda for PGLS model
  profile_result <- pgls.profile(secbestmodel)  # lambda closer to 1 indicates strong phylogenetic signal
  pgls_profile_results[[i]] <- profile_result
  
  print(paste("Profile plot for best model", i, ":", profile_result))
}


#Calculate 95% confidence intervals for the coefficient estimates
# Extract coefficients for each model from secbestmodels
coefs_list <- lapply(secbestmodels, function(model) {
  coef_values <- coef(model)
  data.frame(
    Intercept = coef_values[1],
    Coefficient_2 = coef_values[2],
    Coefficient_3 = coef_values[3]
  )
})

# Combine the coefficient data for all models
coefficients_df <- do.call(rbind, coefs_list)

stacked_df <- stack(coefficients_df)
# Renaming the resulting columns
colnames(stacked_df) <- c("value", "coef")

# Calculate median and CI per variable, grouped by the "var" column
stacked_df_sum <- stacked_df %>%
  group_by(coef) %>%
  summarize(median = median(value),
            cil = quantile(value, prob = 0.025),
            ciu = quantile(value, prob = 0.975))

# Print the summary dataframe
print(stacked_df_sum)

#Calcualte CI and Mean and save it to dataframe:
result_df <- stacked_df %>%
  group_by(coef) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    cil = quantile(value, prob = 0.025),
    ciu = quantile(value, prob = 0.975)
  ) 

result_df

#Plot:

result_df$coef <- factor(result_df$coef, levels = result_df$coef)

p <- ggplot(data=result_df, mapping=aes(x=coef, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="#377EB8") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

# Export the plot
outfile <- paste0(output_data, "/Model2_Coeff_CI_95.png")
ggsave(outfile, p, width=15.8, height=18.4, units="cm", dpi=300)


