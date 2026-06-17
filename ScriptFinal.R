##Welcome

## All the code used for the paper : "Demographic and genetic effect of stocking on 
# two endangered Atlantic salmon (Salmo salar) populations from the same watershed" is available here
# Some readme will help you proceed with the analysis
# Have fun



###____________________________________________________________________________________________________________________________________
#Charging package

library(ggplot2)###
library(ggspatial)###
library(sf)###
library(rnaturalearth)###
library(rnaturalearthdata)###
library(cowplot)###
library(ggh4x)
library(emmeans)
library(dplyr)
library(tidyr)
library(scales)
library(lme4)
library(ggeffects)
library(hierfstat)
library(reshape2)
library(related)
library(adegenet)
library(MASS)
library(ggpubr)
library(purrr)
library(boot)

###Making the map (Figure 1A)_______________________________________________________________________________________________________________
setwd("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map")
#Map of the world
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()

#Import shapefile
watercourses <- st_read("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/Shapefile")
bv <- st_read("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/ShapefileBV")
bv2 <- st_read("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/ShapefileBV2")
mig <- read.csv("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/mig.csv",header = T, sep = ";")
mig <- st_as_sf(mig, coords = c("long", "lat"), crs = 4326)
parr <- read.csv("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/parr.csv",header = T, sep = ";")
parr <- st_as_sf(parr, coords = c("long", "lat"), crs = 4326)
smolt1 <- read.csv("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/smolt_obj1.csv",header = T, sep = ";")
smolt1 <- st_as_sf(smolt1, coords = c("long", "lat"), crs = 4326)
alevins <- read.csv("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/alevins_1.csv",header = T, sep = ";")
alevins <- st_as_sf(alevins, coords = c("long", "lat"), crs = 4326)
smolt2<- read.csv("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/smolt_obj2.csv",header = T, sep = ";")
smolt2 <- st_as_sf(smolt2, coords = c("long", "lat"), crs = 4326)
LARSEM <- read.csv("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/LARSEM.csv",header = T, sep = ";")
LARSEM <- st_as_sf(LARSEM, coords = c("long", "lat"), crs = 4326)
Station <- read.csv("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/Station.csv",header = T, sep = ";")
Station <- st_as_sf(Station, coords = c("long", "lat"), crs = 4326)


PU <- subset(watercourses, watercourses$NOMRIV_1=="Rivière Puyjalon")
RO <- subset(watercourses, watercourses$NOMRIV_1=="Rivière Romaine")

#Carte du canada
canada <- ggplot() +
  geom_sf(data = world, fill = "grey")+
  geom_sf(data = watercourses,aes(fill = NOMRIV_1))+
  geom_sf(data = PU, color = "#DC143C")+
  geom_sf(data = RO, color = "#000080", fill = "#000080")+
  geom_sf(data = LARSEM, color = "deeppink",size = 6, pch = 18)+
  coord_sf(xlim = c(-54, -75), c(43, 54), expand = FALSE)+
  scale_fill_manual(values= c("#DC143C","#000080"),labels = c("Puyjalon","Romaine"))+
  geom_rect(aes(
    xmin = -63.9, 
    ymin = 50.2, 
    xmax = -63.4, 
    ymax = 50.5), 
    fill = NA, 
    colour = "black", linewidth = 0.8)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.background = element_blank())+
  annotate(geom = "text", label = "Atlantic\nOcean", x = -56, y =44,size = 2.5)+
  annotate(geom = "text", label = "Canada", x = -70, y =53,size = 2.5)
canada


#Map des échantillonnages

legend_items <- data.frame(
  name = c("  ", "Parr1", "Smolt1", "Adult1",
           "   ", "Fry", "Parr2", "Smolt2", "Adult2",
           "    ", "Station", "LARSEM"),
  lon = -63.7,
  lat = 50.4
)

# Plot
main_map <- ggplot() +
  geom_sf(data = world, color = "grey85", fill = "grey85") +
  geom_sf(data = bv, color = "grey85", fill = "grey85") +
  geom_sf(data = bv2, color = "grey85", fill = "grey85") +
  geom_sf(data = watercourses, aes(fill = NOMRIV_1)) +
  
  # --- Rivers (filled, no legend) ---
  geom_sf(data = PU, fill = "#DC143C", color = "#DC143C", alpha = 0.6, show.legend = FALSE) +
  geom_sf(data = RO, fill = "#000080", color = "#000080", alpha = 0.6, show.legend = FALSE) +
  
  # --- Sampling sites ---
  geom_sf(data = parr, aes(fill = "Parr1"), size = 5, pch = 24) +
  geom_sf(data = smolt1, aes(fill = "Smolt1"), size = 5, pch = 24) +
  geom_sf(data = mig, aes(fill = "Adult1"), size = 5, pch = 24) +
  geom_sf(data = alevins, aes(fill = "Fry"), size = 5, pch = 24) +
  geom_sf(data = parr, aes(fill = "Parr2"), size = 5, pch = 24) +
  geom_sf(data = smolt1, aes(fill = "Smolt2"), size = 5, pch = 24) +
  geom_sf(data = mig, aes(fill = "Adult2"), size = 5, pch = 24) +
  geom_sf(data = Station, aes(fill = "Station"), size = 6, pch = 23) +
  geom_sf(data = LARSEM, aes(fill = "LARSEM"), size = 6, pch = 23) +
  
  # Fake layer to control legend (no rivers)
  geom_point(data = legend_items,
             aes(x = lon, y = lat, fill = name),
             shape = 21, size = 0, alpha = 0) +
  
  coord_sf(xlim = c(-63.85, -63.42), ylim = c(50.28, 50.48), expand = FALSE) +
  
  # --- Manual fill scale (rivers removed from legend) ---
  scale_fill_manual(
    values = c(
      "  "      = NA,
      "Parr1"   = "orange",
      "Smolt1"  = "yellow",
      "Adult1"  = "forestgreen",
      "   "     = NA,
      "Fry"     = "pink",
      "Parr2"   = "orange",
      "Smolt2"  = "yellow",
      "Adult2"  = "forestgreen",
      "    "    = NA,
      "Station" = "skyblue",
      "LARSEM"  = "deeppink"
    ),
    breaks = legend_items$name,
    labels = c(
      expression(bold("Broodstock")),
      "Parr", "Smolt", "Adult",
      expression(bold("Tissues sampling")),
      "Fry", "Parr", "Smolt", "Adult",
      expression(bold("Incubation")),
      "In-Site", "LARSEM"
    )
  ) +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c(NA, "orange", "yellow", "forestgreen",
               NA, "pink", "orange", "yellow", "forestgreen",
               NA, "skyblue", "deeppink"),
      color = c(NA, "orange", "yellow", "forestgreen",
                NA, "pink", "orange", "yellow", "forestgreen",
                NA, "skyblue", "deeppink"),
      shape = c(NA, 24, 24, 24,
                NA, 24, 24, 24, 24,
                NA, 23, 23)
    )
  )) +
  
  # --- Annotate river names directly on map ---
  annotate("text", x = -63.51, y = 50.343, label = "Puyjalon River",
           color = "#DC143C", fontface = "bold", size = 5) +
  annotate("text", x = -63.55, y = 50.297, label = "Romaine River",
           color = "#000080", fontface = "bold", size = 5) +
  
  # --- Map style ---
  theme_classic() +
  ggspatial::annotation_scale(location = "tl", bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40"),
      line_col = "grey20")
  ) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_blank(),
        legend.background = element_rect(color = "black"),
        strip.text.x = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = c(0.72, 0.76),
        legend.text = element_text(size = 16))

main_map
#map assembly

map <- ggdraw(main_map) +
  draw_plot(canada,
            x = 0.07,
            y =0.45,
            width = 0.35,
            height =0.35)
map

#Figure 1A
ggsave(filename = "E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Map/Map2.png",width = 10, height = 7, dpi = 1200)

###Analysis on the microsatellite for supplementary materials (Figure S2)________________________________________________________________________

Locus <- read.delim("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Checking microsat Supp Mat S2/Locus.txt")
# Convert all columns of Locus to factors
Locus[] <- lapply(Locus, as.factor)
summary(Locus)

# 1. Filter only kept loci
Locus_kept <- Locus %>%
  filter(Kept == "Oui")

# 2. Count loci per chromosome
locus_counts <- Locus_kept %>%
  group_by(Chromosome) %>%
  summarise(Count = n()) %>%
  ungroup()

# 3. Define all possible chromosomes (1 to 29)
all_chr <- tibble(Chromosome = as.character(1:29))

# 4. Join to include chromosomes with zero loci
locus_counts_full <- all_chr %>%
  left_join(locus_counts, by = "Chromosome") %>%
  mutate(Count = replace_na(Count, 0)) %>%
  mutate(Chromosome = factor(Chromosome, levels = as.character(1:29)))  # order 1-29

# 5. Plot for Figure S2A
chr <- ggplot(locus_counts_full, aes(x = Chromosome, y = Count)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Chromosome",
    y = "Number of loci",
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 15))
chr

#Number of alleles per usat using the reference populations
#Import dataset
PopRef41_fin  <- read.structure("STRUCTURE_Filtered_Individuals301_Loci41.stru", 
                                n.ind = 301, n.loc = 41, col.lab = 1, col.pop = 2,col.others = NULL,
                                row.marknames = 1, ask = FALSE)

PopRef41_fin_FST <- genind2hierfstat(PopRef41_fin)

# 1. Number of alleles per locus
num_alleles <- nAll(PopRef41_fin)  # adegenet function: returns a named vector

# 2. Convert to a data frame for plotting
allele_df <- tibble(
  Locus = names(num_alleles),
  Num_Alleles = as.numeric(num_alleles)
)

# 3. Quick summary
summary(allele_df$Num_Alleles)

# 4. Plot number of alleles per locus (Figure S2B)
NAl <- ggplot(allele_df, aes(x = reorder(Locus, -Num_Alleles), y = Num_Alleles)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Locus", y = "Number of alleles"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 15)
  )

NAl

# 1. Calculate FST per locus using hierfstat's basic.stats for Figure 2C
fst_stats <- basic.stats(PopRef41_fin_FST)

# 2. Extract per-locus FST
fst_per_locus <- fst_stats$perloc[, "Fst"]

# Convert to data frame safely
fst_df <- tibble(
  Locus = rownames(fst_stats$perloc),  # use rownames instead of names()
  FST = as.numeric(fst_per_locus)
)

# Check
fst_df

# 4. Quick summary
summary(fst_df$FST)

# 5. Plot FST per locus (Figure S2C)
FST <- ggplot(fst_df, aes(x = reorder(Locus, -FST), y = FST)) +
  geom_col(fill = "tomato") +
  labs(
    x = "Locus",
    y = expression(F[ST])
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 15)
  )
FST

#Assemble Figure S2
Loci <-ggdraw()+
  draw_plot(chr, x = 0, y = 0.66, width = 1, height = 0.33)+
  draw_plot(NAl, x = 0, y = 0.33, width = 1, height = 0.33)+
  draw_plot(FST, x = 0, y = 0, width = 1, height = 0.33)+
  draw_plot_label(label = c("A","B","C"), size = 18,
                  x = c(0,0,0), y = c(0.99,0.666,0.333))

ggsave(filename = "~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Supp Mat/Loci.png",width = 11, height = 14, dpi = 900)


####Objective_1 _ Broodstock capacity to maintain genetic diversity __________________________________________________________________

#Import cheptel arrivals (Figure S5)

cheptel <- read.delim("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/cheptel.txt")
cheptel$Sex <- as.factor(cheptel$Sex)
cheptel$Population <- as.factor(cheptel$Population)
cheptel$Year <- as.factor(cheptel$Year)
cheptel$Stage <- as.factor(cheptel$Stage)
summary(cheptel)


# Prepare data: count individuals by Year, Population, Sex, and Stage
arrival_counts_detailed <- cheptel %>%
  group_by(Year, Population, Sex, Stage) %>%
  summarise(n = n(), .groups = "drop")

# Define colors for facet strip backgrounds
pop_colors <- c("Puyjalon" = "#DC143C", "Romaine" = "#000080")

# Define modified strip labels with N
pop_labels <- c("Puyjalon" = "Puyjalon (N = 447)", "Romaine" = "Romaine (N = 269)")

# Plot for Supp Mat 1
ggplot(arrival_counts_detailed, aes(x = as.factor(Year), y = n, fill = interaction(Sex, Stage))) +
  geom_col() +
  ggh4x::facet_wrap2(
    ~ Population,
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(fill = pop_colors),
      text_x = ggh4x::elem_list_text(color = "white", face = "bold", size = 12)
    ),
    labeller = labeller(Population = pop_labels)
  ) +
  scale_fill_manual(values = c(
    "F.Smolt" = "#e78ac3",   # former adult F color
    "M.Smolt" = "#80b1d3",   # former adult M color
    "F.Adult" = "grey",   # former parr F color
    "M.Adult" = "black",   # former parr M color
    "F.Parr"  = "#b6d7a8",   # new olive green
    "M.Parr"  = "#93c47d"    # darker olive green
  )) +
  labs(x = "Year", y = "Number of Individuals", fill = "Sex & Stage") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    plot.title = element_blank(),  # Remove main title
  )
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Supp Mat/BroodstockArrival.png",width = 10, height = 7, dpi = 900)


setwd("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier")

###3.1.4 Effective number of breeders in the broodstock
#Import crosses database to calculate Nbpot/Nbreal/Nbvar/Nbstocked

cross <- read.delim("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 1 - Broodstock/cross.txt")
cross$Croisement <- as.factor(cross$Croisement)
cross$F_ID <- as.factor(cross$F_ID)
cross$OrigineF <- as.factor(cross$OrigineF)
cross$Cohorte_F <- as.factor(cross$Cohorte_F)
cross$M_ID <- as.factor(cross$M_ID)
cross$Origine_M <- as.factor(cross$Origine_M)
cross$Cohorte_M <- as.factor(cross$Cohorte_M)
cross$Year <- as.factor(cross$Year)
cross$Stocking_Year <- as.factor(cross$Stocking_Year)
cross$Type <- as.factor(cross$Type)
cross$Survival <- as.numeric(cross$Survival)
summary(cross)

#All calculations for Figure 2
#Calculate potential Nbpot (meaning with individuals that could have matured)____________________________________________________

# Optional for reproducibility
set.seed(123)

# Adults: reproduce the year they arrive
adults <- cheptel %>%
  filter(Stage == "Adult") %>%
  mutate(StartYear = as.integer(as.character(Year)))

# Non-adults: 50% mature at 2 years, 50% at 3 years,
# separately for males and females within each arrival year
smolts <- cheptel %>%
  filter(Stage == "Smolt") %>%
  group_by(Year, Sex) %>%
  mutate(
    Year_num = as.integer(as.character(Year)),  # convert factor -> integer year
    r = rank(runif(n()), ties.method = "random"),  # randomize within Year × Sex
    half = floor(n() / 2),
    StartYear = if_else(r <= half, Year_num + 2L, Year_num + 3L)
  ) %>%
  ungroup() %>%
  dplyr::select(-r, -half, -Year_num)
summary(smolts)

parr <- cheptel %>%
  filter(Stage == "Parr") %>%
  group_by(Year, Sex) %>%
  mutate(
    Year_num = as.integer(as.character(Year)),  # convert factor -> integer year
    r = rank(runif(n()), ties.method = "random"),
    half = floor(n() / 2),
    StartYear = if_else(r <= half, Year_num + 3L, Year_num + 4L)
  ) %>%
  ungroup() %>%
  dplyr::select(-r, -half, -Year_num)
summary(parr)


# Combine and expand to max 4 breeding years (consecutive once started)
breeding_df <- bind_rows(adults, smolts,parr) %>%
  rowwise() %>%
  mutate(Year = list(StartYear:(StartYear + 3L))) %>%
  unnest(Year) %>%
  ungroup() %>%
  transmute(
    Individual = ID,
    Sex,
    Population,
    Year = as.integer(Year)
  )


summary(breeding_df)

CheptelPot <- subset(breeding_df, breeding_df$Year < 2024)


# Group by Year and Population, count males and females, and calculate Ne with equation 1
Nbpot <- CheptelPot %>%
  group_by(Year, Population) %>%
  summarise(
    Nmpot = sum(Sex == "M"),
    Nfpot = sum(Sex == "F"),
    Nbpot = ifelse(Nmpot + Nfpot > 0, 4 * Nmpot * Nfpot / (Nmpot + Nfpot), NA_real_),
    .groups = "drop"
  )

# View results for Npot
print(Nbpot)

##Calculate realized Nbreal (meaning with individual that actually reproduced)____________________________________________________


# Count unique reproducing females by their population
females_by_pop <- cross %>%
  group_by(Population = OrigineF) %>%
  summarise(Nf_total = n_distinct(F_ID), .groups = "drop")

# Count unique reproducing males by their population
males_by_pop <- cross %>%
  group_by(Population = Origine_M) %>%
  summarise(Nm_total = n_distinct(M_ID), .groups = "drop")

# Merge the two summaries by population
total_reproducers_by_pop <- full_join(females_by_pop, males_by_pop, by = "Population") %>%
  replace_na(list(Nf_total = 0, Nm_total = 0))

print(total_reproducers_by_pop)

#Nbreal calculation
Nbreal <- cross %>%
  # Keep only crosses where male and female are from the same population (should not exist anyway)
  filter(OrigineF == Origine_M) %>%
  group_by(Year, Population = OrigineF) %>%
  summarise(
    Nmreal = n_distinct(M_ID),
    Nfreal = n_distinct(F_ID),
    Crosses = n(),  # Number of crosses made for this population and year
    .groups = "drop"
  ) %>%
  mutate(
    Nbreal = ifelse(Nfreal + Nmreal > 0, 4 * Nmreal * Nfreal / (Nmreal + Nfreal), NA)
  )

print(Nbreal)

## calculate Nbvarall with the variance of offspring per male and female_________________________________________________________
#Using number of eggs produce per individuals to account for variance

# ---- Calculate Nbvarall for females ----
nb_females_varall <- cross %>%
  group_by(Year, Population = OrigineF, F_ID) %>%
  summarise(total_offspring = sum(Offspring, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year, Population) %>%
  summarise(
    N_F_varall = n(),
    k_F_varall = mean(total_offspring),
    Vk_F_varall = var(total_offspring),
    Nb_F_varall = (k_F_varall * N_F_varall - 2) /
      (k_F_varall - 1 + (Vk_F_varall / k_F_varall)),
    .groups = "drop"
  )

# ---- Calculate Nbvarall for males ----
nb_males_varall <- cross %>%
  group_by(Year, Population = Origine_M, M_ID) %>%
  summarise(total_offspring = sum(Offspring, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year, Population) %>%
  summarise(
    N_M_varall = n(),
    k_M_varall = mean(total_offspring),
    Vk_M_varall = var(total_offspring),
    Nb_M_varall = (k_M_varall * N_M_varall - 2) /
      (k_M_varall - 1 + (Vk_M_varall / k_M_varall)),
    .groups = "drop"
  )

# ---- Merge both and compute total Nbvarall ----
Nbvarall <- full_join(nb_females_varall, nb_males_varall, by = c("Year", "Population")) %>%
  mutate(
    Nbvarall = ifelse(
      !is.na(Nb_F_varall) & !is.na(Nb_M_varall),
      (4 * Nb_F_varall * Nb_M_varall) / (Nb_F_varall + Nb_M_varall),
      NA
    )
  )

Nbvarall

## calculate Nbvarstock with the variance of offspring per male and female just before stocking_________________________________________________________
#Take into account the  column offspring survival only (offspring that survived from reproduction to stocking)
# ---- Calculate Nbvarstock for females ----
nb_females_varstock <- cross %>%
  group_by(Year, Population = OrigineF, F_ID) %>%
  summarise(total_offspring_surv = sum(Offspring_Surv, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year, Population) %>%
  summarise(
    N_F_varstock = n(),
    k_F_varstock = mean(total_offspring_surv),
    Vk_F_varstock = var(total_offspring_surv),
    Nb_F_varstock = (k_F_varstock * N_F_varstock - 2) /
      (k_F_varstock - 1 + (Vk_F_varstock / k_F_varstock)),
    .groups = "drop"
  )

# ---- Calculate Nbvarstock for males ----
nb_males_varstock <- cross %>%
  group_by(Year, Population = Origine_M, M_ID) %>%
  summarise(total_offspring_surv = sum(Offspring_Surv, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year, Population) %>%
  summarise(
    N_M_varstock = n(),
    k_M_varstock = mean(total_offspring_surv),
    Vk_M_varstock = var(total_offspring_surv),
    Nb_M_varstock = (k_M_varstock * N_M_varstock - 2) /
      (k_M_varstock - 1 + (Vk_M_varstock / k_M_varstock)),
    .groups = "drop"
  )

# ---- Merge both and compute total Nbvarstock ----
Nbvarstock <- full_join(nb_females_varstock, nb_males_varstock, by = c("Year", "Population")) %>%
  mutate(
    Nbvarstock = ifelse(
      !is.na(Nb_F_varstock) & !is.na(Nb_M_varstock),
      (4 * Nb_F_varstock * Nb_M_varstock) / (Nb_F_varstock + Nb_M_varstock),
      NA
    )
  )

Nbvarstock

#Merge

Nbpot <- Nbpot %>% filter(!(Year == 2014 & Population == "Romaine")) #No reproduction for Romaine that year)

# ---- Ensure Year is numeric in all datasets ----
Nbpot <- Nbpot %>% mutate(Year = as.numeric(as.character(Year)))
Nbreal <- Nbreal %>% mutate(Year = as.numeric(as.character(Year)))
Nbvarall <- Nbvarall %>% mutate(Year = as.numeric(as.character(Year)))
Nbvarstock <- Nbvarstock %>% mutate(Year = as.numeric(as.character(Year)))

# ---- Select Year, Population and Nb columns from each ----
Nbpot_sel <- Nbpot %>% dplyr::select(Year, Population, Nbpot)
Nbreal_sel <- Nbreal %>% dplyr::select(Year, Population, Nbreal)
Nbvarall_sel <- Nbvarall %>% dplyr::select(Year, Population, Nbvarall)
Nbvarstock_sel <- Nbvarstock %>% dplyr::select(Year, Population, Nbvarstock)

# ---- Join all together by Year and Population ----
Nball <- Nbpot_sel %>%
  full_join(Nbreal_sel, by = c("Year", "Population")) %>%
  full_join(Nbvarall_sel, by = c("Year", "Population")) %>%
  full_join(Nbvarstock_sel, by = c("Year", "Population")) %>%
  arrange(Population, Year)



##Value summary
summary(Nball)
Nball_summary <- Nball %>%
  group_by(Population) %>%
  summarise(
    Nbpot_mean = mean(Nbpot, na.rm = TRUE),
    Nbpot_SE = sd(Nbpot, na.rm = TRUE) / sqrt(sum(!is.na(Nbpot))),
    
    Nbreal_mean = mean(Nbreal, na.rm = TRUE),
    Nbreal_SE = sd(Nbreal, na.rm = TRUE) / sqrt(sum(!is.na(Nbreal))),
    
    Nbvarall_mean = mean(Nbvarall, na.rm = TRUE),
    Nbvarall_SE = sd(Nbvarall, na.rm = TRUE) / sqrt(sum(!is.na(Nbvarall))),
    
    Nbvarstock_mean = mean(Nbvarstock, na.rm = TRUE),
    Nbvarstock_SE = sd(Nbvarstock, na.rm = TRUE) / sqrt(sum(!is.na(Nbvarstock)))
  )

# Optionally, round for neat display
Nball_summary <- Nball_summary %>%
  mutate(across(-Population, ~round(., 1)))

Nball_summary

#Checking correlation
# Select only the Nb columns
Nb_values <- Nball %>% dplyr::select(Nbpot, Nbreal, Nbvarall, Nbvarstock)
cor_matrix <- cor(Nb_values, use = "pairwise.complete.obs", method = "pearson")
cor_matrix

Nball <- Nball %>%
  mutate(
    Nb_red1 = ((Nbpot-Nbreal) / Nbpot)*100,
    Nb_red1 = ifelse(Nb_red1 < 0, NA, Nb_red1)
  )

Nball <- Nball %>%
  mutate(
    Nb_red2 = ((Nbreal-Nbvarall) / Nbreal)*100,
    Nb_red2 = ifelse(Nb_red2 < 0, NA, Nb_red2)
  )

Nball <- Nball %>%
  mutate(
    Nb_red3 = ((Nbvarall-Nbvarstock) / Nbvarall)*100,
    Nb_red3 = ifelse(Nb_red3 < 0, NA, Nb_red3)
  )

# ---- Compute mean loss per Population ----
mean_losses <- Nball %>%
  group_by(Population) %>%
  summarise(
    mean_Nb_red1 = mean(Nb_red1, na.rm = TRUE),
    mean_Nb_red2 = mean(Nb_red2, na.rm = TRUE),
    mean_Nb_red3 = mean(Nb_red3, na.rm = TRUE)
  )

mean_losses



# ---- Filter for Puyjalon and pivot to long format ----
Nb_long_puy <- Nball %>%
  filter(Population == "Puyjalon") %>%
  dplyr::select(Year, Nbpot, Nbreal, Nbvarall, Nbvarstock) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Nb_type",
    values_to = "Nb_value"
  )

# ---- Define harmonious colors for the 4 Nb types ----
library(scales)
nb_colors <- c(
  "Nbpot" = alpha("grey", 0.4),      
  "Nbreal" = "#a6cee3",     
  "Nbvarall" = "#1f78b4",  
  "Nbvarstock" = "#9467bd" 
)
# Extract the Puyjalon means for convenience
puy_mean <- mean_losses %>% filter(Population == "Puyjalon")


# ---- Plot ---- (Figure 2A)
pu <- ggplot(Nb_long_puy, aes(x = factor(Year), y = Nb_value, fill = Nb_type)) +
  geom_col(position = "dodge") +
  # Mimic a facet label
  annotate(
    "text",
    x = 10,
    y = 185,  # 95% of max y
    label = "Puyjalon",
    hjust = 1,
    vjust = 0,
    size = 8,
    fontface = "bold",
    color = "#DC143C" # optional: dark blue to match Puyjalon
  ) +
  # Mean losses displayed below the title
  annotate(
    "text",
    x = 0.6, y = 192,
    label = paste0("Mean loss: ",
                   "\nPotential to realized = - ", round(puy_mean$mean_Nb_red1, 1), "%",
                   "\nRealized to offspring variance = - ", round(puy_mean$mean_Nb_red2, 1), "%",
                   "\nOffspring to stocking = - ", round(puy_mean$mean_Nb_red3, 1), "%"),
    hjust = 0, vjust = 1,
    size = 6,
    color = "black"
  ) +
  labs(
    x = "Year of reproduction",
    y = expression("Effective number of breeders (N"[b]*")"),
    fill = "Nb type"
  ) +
  scale_fill_manual(
    values = nb_colors, 
    labels = c(
      expression("N"[b]*"pot"),
      expression("N"[b]*"real"),
      expression("N"[b]*"var"),
      expression("N"[b]*"stock")
    )
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(0, 185))
pu

# ---- Filter for Romaine and pivot to long format ----
Nb_long_ro <- Nball %>%
  filter(Population == "Romaine") %>%
  dplyr::select(Year, Nbpot, Nbreal, Nbvarall, Nbvarstock) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Nb_type",
    values_to = "Nb_value"
  )
# Extract the Puyjalon means for convenience
ro_mean <- mean_losses %>% filter(Population == "Romaine")

# ---- Plot ---- (Figure 2B)
ro <- ggplot(Nb_long_ro, aes(x = factor(Year), y = Nb_value, fill = Nb_type)) +
  geom_col(position = "dodge") +
  # Mimic a facet label
  annotate(
    "text",
    x = 9,
    y = 185,  # 95% of max y
    label = "Romaine",
    hjust = 1,
    vjust = 0,
    size = 8,
    fontface = "bold",
    color = "#000080" # optional: dark blue to match Puyjalon
  ) +
  # Mean losses displayed below the title
  annotate(
    "text",
    x = 0.6, y = 192,
    label = paste0("Mean loss: ",
                   "\nPotential to realized = - ", round(ro_mean$mean_Nb_red1, 1), "%",
                   "\nRealized to offspring variance = - ", round(ro_mean$mean_Nb_red2, 1), "%",
                   "\nOffspring to stocking = - ", round(ro_mean$mean_Nb_red3, 1), "%"),
    hjust = 0, vjust = 1,
    size = 6,
    color = "black"
  ) +
  labs(
    x = "Year of reproduction",
    y = expression("Effective number of breeders (N"[b]*")"),
    fill = "Nb type"
  ) +
  scale_fill_manual(
    values = nb_colors, 
    labels = c(
      expression("N"[b]*"pot"),
      expression("N"[b]*"real"),
      expression("N"[b]*"var"),
      expression("N"[b]*"stock")
    )
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  coord_cartesian(ylim = c(0, 185))
ro

#Figure 2
Nb <-ggdraw()+
  draw_plot(pu, x = 0, y = 0.5, width = 1, height = 0.5)+
  draw_plot(ro, x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0,0), y = c(0.99,0.50))
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Nb.png",width = 11, height = 14, dpi = 900)



#Summary
Nb_summary_SE <- Nball %>%
  group_by(Population) %>%
  summarise(
    mean_Nbpot = mean(Nbpot, na.rm = TRUE),
    se_Nbpot   = sd(Nbpot, na.rm = TRUE) / sqrt(sum(!is.na(Nbpot))),
    
    mean_Nbreal = mean(Nbreal, na.rm = TRUE),
    se_Nbreal   = sd(Nbreal, na.rm = TRUE) / sqrt(sum(!is.na(Nbreal))),
    
    mean_Nbvarall = mean(Nbvarall, na.rm = TRUE),
    se_Nbvarall   = sd(Nbvarall, na.rm = TRUE) / sqrt(sum(!is.na(Nbvarall))),
    
    mean_Nbvarstock = mean(Nbvarstock, na.rm = TRUE),
    se_Nbvarstock   = sd(Nbvarstock, na.rm = TRUE) / sqrt(sum(!is.na(Nbvarstock))),
    .groups = "drop"
  ) %>%
  mutate(across(-Population, ~ round(., 1)))  # exclude Population from rounding

Nb_summary_SE


#Supplementary S6 (K_M, K_F, N, V_F, V_M) to visualize importance of offspring survival

summary(cross)

# 1. Expand offspring per parent
female_offspring <- cross %>%
  dplyr::select(Year, Population = OrigineF, ID = F_ID, Offspring) %>%
  group_by(Year, Population, ID) %>%
  summarise(Offspring = sum(Offspring), .groups = "drop") %>%
  mutate(Sex = "F")

male_offspring <- cross %>%
  dplyr::select(Year, Population = Origine_M, ID = M_ID, Offspring) %>%
  group_by(Year, Population, ID) %>%
  summarise(Offspring = sum(Offspring), .groups = "drop") %>%
  mutate(Sex = "M")

offspring_all <- bind_rows(female_offspring, male_offspring)
summary(offspring_all)
pop_colors <- c("Puyjalon" = "#DC143C", "Romaine" = "#000080")

#Density of offspring per individual (SuppMat6B)
curve <- ggplot(offspring_all, aes(x = Offspring, color = as.factor(Sex))) +
  geom_density(aes(y = after_stat(count / sum(count) * 100)),
               linewidth = 1.2, alpha = 0.7) +
  facet_wrap2(
    ~ Population, scales = "free",
    strip = strip_themed(
      background_x = elem_list_rect(fill = unname(pop_colors)),
      text_x = elem_list_text(color = "white", face = "bold", size = 12)
    )
  ) +
  labs(
    x = "Offspring per individual",
    y = "Density curve",
    color = "Sex"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),
    axis.text.x = element_text(color = "black",angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  )+
  scale_x_log10(
    breaks = c(1000, 5000, 10000, 35000),
    labels = scales::comma
  )
curve #Figure S6B

#Code for Figure S6A
# STEP 1: Compute Km, Kf, Vm, Vf from offspring_all
offspring_stats <- offspring_all %>%
  group_by(Population, Year, Sex) %>%
  summarise(
    K = mean(Offspring),
    V = var(Offspring),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Sex,
    values_from = c(K, V),
    names_glue = "{.value}_{Sex}"
  )

# STEP 2: Get N from cross (sum of offspring by pop and year)
offspring_total <- cross %>%
  group_by(OrigineF, Year) %>%
  summarise(N = sum(Offspring, na.rm = TRUE), .groups = "drop")
offspring_total <- offspring_total %>%
  rename(Population = OrigineF)

# STEP 3: Join and calculate Ne using the formula
ne_df <- left_join(offspring_stats, offspring_total, by = c("Population", "Year")) %>%
  mutate(
    Ne = (4 * N * (K_M - 1) * (K_F - 1)) /
      (V_M * (K_F - 1) + V_F * (K_M - 1) + 4 * (K_M - 1) * (K_F - 1))
  )

# 1. Pivot your ne_df into long format for the metrics
metrics_long <- ne_df %>%
  dplyr::select(Population, Year, K_M, K_F, V_M, V_F, N) %>%
  pivot_longer(
    cols = c(K_M, K_F, V_M, V_F, N),
    names_to  = "Metric",
    values_to = "Value"
  )
# Make sure Metric is a factor with correct order
metrics_long$Metric <- factor(metrics_long$Metric, levels = c("K_M", "K_F", "N", "V_M", "V_F"))

# Define colors for lines (metrics)
metric_colors <- c(
  "K_M" = "#0072B2",   # Blue
  "K_F" = "#D55E00",   # Orange
  "V_M" = "#56B4E9",   # Light blue
  "V_F" = "#E69F00",   # Yellow-orange
  "N"   = "#009E73"    # Green
)


# 2. Define facet‐strip colors
pop_colors <- c("Puyjalon" = "#DC143C", "Romaine" = "#000080")


# Filter for Km, Kf, N
metrics_k <- metrics_long %>%
  filter(Metric %in% c("K_F", "K_M", "N"))

metrics <- ggplot(metrics_k, aes(x = Year, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  ggh4x::facet_wrap2(
    ~ Population,
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(fill = pop_colors),
      text_x       = ggh4x::elem_list_text(color = "white", face = "bold", size = 14)
    )
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Value", color = "Metric") +
  theme_classic() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 15, color = "black"),
    axis.text.y      = element_text(color = "black", size = 15),
    axis.title       = element_text(size = 18),
    legend.position  = "bottom",
    legend.text      = element_text(size = 15),
    legend.title     = element_text(size = 15),
    strip.background = element_blank()
  )+
  scale_y_log10(labels = scales::label_scientific())
metrics #Figure S6A

# Filter for Vf and Vm
metrics_v <- metrics_long %>%
  filter(Metric %in% c("V_M", "V_F"))
metrics_v$Metric <- factor(metrics_v$Metric, levels = c("V_F", "V_M"))
v <- ggplot(metrics_v, aes(x = Year, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  ggh4x::facet_wrap2(
    ~ Population,
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(fill = pop_colors),
      text_x       = ggh4x::elem_list_text(color = "white", face = "bold", size = 14)
    )
  ) +
  #scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Value", color = "Metric") +
  theme_classic() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 15, color = "black"),
    axis.text.y      = element_text(size = 15, color = "black"),
    axis.title       = element_text(size = 18),
    legend.position  = "bottom",
    legend.text      = element_text(size = 15),
    legend.title     = element_text(size = 15),
    strip.background = element_blank()
  )
v #Figure S6C


#Survival rates between 2020 and 2023
summary(cross)
cross_filtered <- subset(cross, Year %in% 2020:2023 & !is.na(Survival))
cross_filtered$Survival_percent <- cross_filtered$Survival * 100
summary(cross_filtered)
# Function to calculate SE
se <- function(x) sd(x) / sqrt(length(x))

# Summary per population
surv_summary <- cross_filtered %>%
  group_by(OrigineF) %>%   # or use Population if that’s the right column
  summarise(
    mean_survival = mean(Survival_percent, na.rm = TRUE),
    se_survival   = se(Survival_percent),
    min_survival  = min(Survival_percent, na.rm = TRUE),
    max_survival  = max(Survival_percent, na.rm = TRUE),
    .groups = "drop"
  )
surv_summary
# Plot
eggfry2023 <- ggplot(cross_filtered, aes(x = Survival_percent)) +
  geom_histogram(binwidth = 5, fill = "grey70", color = "black") +
  facet_wrap2(
    ~OrigineF,
    strip = strip_themed(
      background_x = elem_list_rect(fill = pop_colors)
    )
  ) +
  labs(x = "Survival rate (%)", y = "Frequency in family egg-to-fry survival\n(2020-2023)") +
  theme_classic() +
  theme(axis.text = element_text(size = 15, color = "black"), 
        axis.title = element_text(size = 18),
        strip.text = element_text(color = "white", size = 14, face = "bold"),
        legend.position = "none")
eggfry2023 #Figure S6D

#Figure S6
Nevar<-ggdraw()+
  draw_plot(metrics, x = 0, y = 0.5, width = 0.5, height = 0.5)+
  draw_plot(curve, x = 0.5, y = 0.5, width = 0.5, height = 0.5)+
  draw_plot(v, x = 0, y = 0, width = 0.5, height = 0.5)+
  draw_plot(eggfry2023, x = 0.5, y = 0, width = 0.5, height = 0.5)+
  draw_plot_label(label = c("A", "B","C","D"), size = 18,
                  x = c(0,0.5,0,0.5), y = c(0.99,0.99,0.5,0.5))

ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Supp Mat/Nevar.png",width = 13, height = 13, dpi = 900)


###3.1.5 Broodstock genetic diversity

#Compute female per year
females <- data.frame(
  ID = cross$F_ID,
  Pop = cross$OrigineF,
  Year = cross$Year
)

#Compute males per year
males <- data.frame(
  ID = cross$M_ID,
  Pop = cross$Origine_M,
  Year = cross$Year
)
# Combine females and males
all_parents <- rbind(females, males)

# Remove rows with NA IDs or Pop
all_parents <- all_parents[!is.na(all_parents$ID) & !is.na(all_parents$Pop), ]

# Keep only unique combinations of ID x Year
all_parents_unique <- unique(all_parents)

# Check
head(all_parents_unique)
nrow(all_parents_unique)

#Import LARSA individual genotype and information on assignation
LARSAgen <- read.delim("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 1 - Broodstock/LARSAgen.txt")

summary(all_parents_unique)
summary(LARSAgen)

# Merge genotype info into the parents list
all_parents_geno <- merge(
  all_parents_unique,  # parent list with ID, Pop, Year
  LARSAgen,            # genotype table
  by = "ID",           # merge by ID
  all.x = TRUE         # keep all parents even if no genotype info
)

summary(all_parents_geno)

# Write as tab-delimited text file
write.table(all_parents_geno,
            file = "all_parents_geno.txt",  # output file name
            sep = "\t",                     # tab-separated
            quote = FALSE,                  # do not quote strings
            row.names = FALSE)              # do not include row numbers


##How many individuals were assigned in the cheptel and how many reproduce
LARSAgen$Population<- as.factor(LARSAgen$Population)
LARSAgen$Origine <- as.factor(LARSAgen$Origine)
LARSAgen$Sex <- as.factor(LARSAgen$Sex)
LARSAgen$Assigned <- as.factor(LARSAgen$Assigned)
summary(LARSAgen)

table(LARSAgen$Assigned,LARSAgen$Origine, LARSAgen$Population)

#How many reproduce
summary(all_parents_geno)

# 1️⃣ Count assigned status per Year & Pop

assigned_summary <- all_parents_geno %>%
  filter(!is.na(Assigned) & Assigned != "") %>%   # keep only valid entries
  group_by(Pop, Year, Assigned) %>%
  summarise(
    n_reproducers = n_distinct(ID),   # number of unique individuals
    .groups = "drop"
  )

print(assigned_summary)

# 2️⃣ Plot: histogram of "oui" vs "non" reproducers per year Mat supp(Figure S7A)

reproAssigned <- ggplot(assigned_summary, aes(x = factor(Year), y = n_reproducers, fill = Assigned)) +
  geom_col(position = "stack") +
  
  # facet with colored strips AND white text
  facet_wrap2(
    ~Pop,
    strip = strip_themed(
      background_x = elem_list_rect(fill = pop_colors, color = NA),
      text_x = elem_list_text(color = "white", face = "bold", size = 14)
    )
  ) +
  
  theme_classic() +
  labs(
    x = "Year",
    y = "Number of reproducers"
  ) +
  
  scale_fill_manual(
    values = c("oui" = "#D55E00", "non" = "#FFD700"),
    labels = c("oui" = "Hatchery-born", "non" = "Wild-born")
  ) +
  
  theme(
    axis.text = element_text(size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 14, color = "white", face = "bold"),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

reproAssigned


#Calculation of genetic diversity indices
setwd("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 1 - Broodstock")

LARSA_Gen <- read.delim("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 1 - Broodstock/LARSA_Gen.txt")

#Using Structure file prepared with the R script prep_structure.R
LARSA_Gen_Stru  <- read.structure("STRUCTURE_Filtered_Individuals823_Loci41.stru", 
                                  n.ind = 823, n.loc = 41, col.lab = 1, col.pop = 2,
                                  row.marknames = 1, ask = FALSE, onerowperind = TRUE)
LARSA_Gen_Hier <- genind2hierfstat(LARSA_Gen_Stru)
LARSA_Gen_Hier<-bind_cols(LARSA_Gen,LARSA_Gen_Hier)
LARSA_Gen_Hier$NumUnique <- as.factor(LARSA_Gen_Hier$NumUnique)
LARSA_Gen_Hier$PopUniq <- as.factor(LARSA_Gen_Hier$PopUniq)
LARSA_Gen_Hier$Nom_Pop <- as.factor(LARSA_Gen_Hier$Nom_Pop)
LARSA_Gen_Hier$Assigned <- as.factor(LARSA_Gen_Hier$Assigned)
LARSA_Gen_Hier$Sex <- as.factor(LARSA_Gen_Hier$Sex)
LARSA_Gen_Hier$Year<- as.factor(LARSA_Gen_Hier$Year)
summary(LARSA_Gen_Hier)


##Expected heterozyosity
# calculate He (Hs) from bootstraps
bootstrap_he <- function(data, nboot = 10) {
  replicate(nboot, {
    sampled <- data[sample(1:nrow(data), replace = TRUE), ]
    bs <- basic.stats(sampled)$Hs
    mean(bs, na.rm = TRUE)
  })
}

# Prepare genetic data
gen_cols <- grep("^Ssa|^NGS", names(LARSA_Gen_Hier), value = TRUE)
meta_cols <- c("Year", "Nom_Pop")
filtered_data <- LARSA_Gen_Hier %>% filter(!is.na(Year)) %>%
  dplyr::select(all_of(meta_cols), all_of(gen_cols))

# Remove rows with too much missing data (optional but recommended)
filtered_data <- filtered_data[rowMeans(is.na(filtered_data[gen_cols])) < 0.2, ]

# Split by group
grouped <- split(filtered_data, interaction(filtered_data$Year, 
                                            filtered_data$Nom_Pop,
                                            drop = TRUE))


# Bootstrap and store results
results <- lapply(names(grouped), function(grp_name) {
  df <- grouped[[grp_name]]
  if (nrow(df) < 6) return(NULL)
  
  # Remove metadata before computing stats
  geno <- df[, gen_cols]
  
  # Convert to hierfstat format
  geno_hierfstat <- df %>%
    mutate(pop = 1) %>% # fake single population
    dplyr::select(pop, all_of(gen_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  # Bootstrap He
  he_boot <- bootstrap_he(geno_hierfstat, nboot = 100)
  
  # Return results
  data.frame(
    Year = unique(df$Year),
    Nom_Pop = unique(df$Nom_Pop),
    He_mean = mean(he_boot, na.rm = TRUE),
    He_sd = sd(he_boot, na.rm = TRUE),
    N = nrow(df)
  )
})

# Combine all results
he_df <- do.call(rbind, results)
summary(he_df)

# Code for Figure S7B
pop_colors <- c("Romaine" = "#000080", "Puyjalon" = "#DC143C")

heLARSA <- ggplot(he_df, aes(x = Year, y = He_mean, color = Nom_Pop, group = Nom_Pop)) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(
    aes(ymin = He_mean - He_sd, ymax = He_mean + He_sd),
    width = 0.2,
    position = position_dodge(width = 0.3)
  ) +
  scale_color_manual(values = pop_colors, name = "Population", guide = "none") +
  labs(x = "Year", y = expression("Expected Heterozygosity (H"[E]*")")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
heLARSA

he_summary <- he_df %>%
  group_by(Nom_Pop) %>%
  summarise(
    He_mean_pop = mean(He_mean, na.rm = TRUE),
    He_se_pop   = mean(He_sd, na.rm = TRUE),
    N_total     = sum(N),
    .groups = "drop"
  )

he_summary

##Observed heterozyosity
# calculate Ho from bootstraps
bootstrap_ho <- function(data, nboot = 10) {
  replicate(nboot, {
    sampled <- data[sample(1:nrow(data), replace = TRUE), ]
    bs <- basic.stats(sampled)$Ho
    mean(bs, na.rm = TRUE)
  })
}


# Bootstrap and store results
resultsHo <- lapply(names(grouped), function(grp_name) {
  df <- grouped[[grp_name]]
  if (nrow(df) < 6) return(NULL)
  
  # Remove metadata before computing stats
  geno <- df[, gen_cols]
  
  # Convert to hierfstat format
  geno_hierfstat <- df %>%
    mutate(pop = 1) %>% # fake single population
    dplyr::select(pop, all_of(gen_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  # Bootstrap He
  ho_boot <- bootstrap_ho(geno_hierfstat, nboot = 100)
  
  # Return results
  data.frame(
    Year = unique(df$Year),
    Nom_Pop = unique(df$Nom_Pop),
    Ho_mean = mean(ho_boot, na.rm = TRUE),
    Ho_sd = sd(ho_boot, na.rm = TRUE),
    N = nrow(df)
  )
})

# Combine all results
ho_df <- do.call(rbind, resultsHo)

ho_summary <- ho_df %>%
  group_by(Nom_Pop) %>%
  summarise(
    Ho_mean_pop = mean(Ho_mean, na.rm = TRUE),
    Ho_se_pop   = mean(Ho_sd, na.rm = TRUE),
    N_total     = sum(N),
    .groups = "drop"
  )

ho_summary
# Code for Figure S7C
pop_colors <- c("Romaine" = "#000080", "Puyjalon" = "#DC143C")

hoLARSA <- ggplot(ho_df, aes(x = Year, y = Ho_mean, color = Nom_Pop, group = Nom_Pop)) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(
    aes(ymin = Ho_mean - Ho_sd, ymax = Ho_mean + Ho_sd),
    width = 0.2,
    position = position_dodge(width = 0.3)
  ) +
  scale_color_manual(values = pop_colors, name = "Population") +
  labs(x = "Year", y = expression("Observed Heterozygosity (H"[O]*")")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
hoLARSA

## Allelic richness

# -------------------------
# Locus columns
gen_cols <- grep("^Ssa|^NGS", names(LARSA_Gen_Hier), value = TRUE)

# Drop rows with missing metadata
LARSA_clean <- LARSA_Gen_Hier %>%
  filter(!is.na(Year) & !is.na(Nom_Pop))

# -------------------------
# Bootstrap function
# -------------------------
bootstrap_ar <- function(data, nboot = 100) {
  if (nrow(data) < 10) return(NULL)
  
  # hierfstat format: first column = population
  data_hierf <- data.frame(pop = rep(1, nrow(data)), data[, gen_cols])
  data_hierf <- droplevels(data_hierf)
  
  replicate(nboot, {
    sampled <- data_hierf[sample(1:nrow(data_hierf), replace = TRUE), ]
    ar <- tryCatch(
      allelic.richness(sampled),
      error = function(e) NA
    )
    if (is.list(ar)) {
      mean(ar$Ar[1, ], na.rm = TRUE)  # mean across loci
    } else {
      NA
    }
  })
}

# =========================
# 1️⃣ Bootstrap by population (per Year × Nom_Pop)
# =========================
grouped <- LARSA_clean %>%
  group_by(Year, Nom_Pop) %>%
  group_split()

ar_results <- lapply(grouped, function(group) {
  if (nrow(group) < 10) return(NULL)
  bs <- bootstrap_ar(group)
  data.frame(
    Year   = unique(group$Year),
    Nom_Pop = unique(group$Nom_Pop),
    Ar_mean = mean(bs, na.rm = TRUE),
    Ar_sd   = sd(bs, na.rm = TRUE),
    N       = nrow(group)
  )
}) |> bind_rows()


ar_summary <- ar_results %>%
  group_by(Nom_Pop) %>%
  summarise(
    Ar_mean_pop = mean(Ar_mean, na.rm = TRUE),
    Ar_se_pop   = mean(Ar_sd, na.rm = TRUE),
    N_total     = sum(N),
    .groups = "drop"
  )

ar_summary


# =========================
# Plot code for Figure 3A
# =========================
pop_colors <- c("Romaine" = "#000080", "Puyjalon" = "#DC143C")

arLARSA <- ggplot(ar_results, aes(x = Year, y = Ar_mean, color = Nom_Pop, group = Nom_Pop)) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(
    aes(ymin = Ar_mean - Ar_sd, ymax = Ar_mean + Ar_sd),
    width = 0.2,
    position = position_dodge(width = 0.3)
  ) +
  scale_color_manual(values = pop_colors, name = "Population", guide = "none") +
  labs(x = "Year", y = expression("Allelic richness (A"[R]*")")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

arLARSA


##FIS
# calculate Fis from bootstraps
bootstrap_Fis <- function(data, nboot = 100) {
  replicate(nboot, {
    sampled <- data[sample(1:nrow(data), replace = TRUE), ]
    bs <- basic.stats(sampled)$Fis
    mean(bs, na.rm = TRUE)
  })
}

# Prepare genetic data
gen_cols <- grep("^Ssa|^NGS", names(LARSA_Gen_Hier), value = TRUE)
meta_cols <- c("Year", "Nom_Pop")
filtered_data <- LARSA_Gen_Hier %>% filter(!is.na(Year)) %>%
  dplyr::select(all_of(meta_cols), all_of(gen_cols))

# Remove rows with too much missing data (optional but recommended)
filtered_data <- filtered_data[rowMeans(is.na(filtered_data[gen_cols])) < 0.2, ]

# Split by group
grouped <- split(filtered_data, interaction(filtered_data$Year, 
                                            filtered_data$Nom_Pop,
                                            drop = TRUE))

# Bootstrap and store results
resultsFis <- lapply(names(grouped), function(grp_name) {
  df <- grouped[[grp_name]]
  if (nrow(df) < 6) return(NULL)
  
  # Remove metadata before computing stats
  geno <- df[, gen_cols]
  
  # Convert to hierfstat format
  geno_hierfstat <- df %>%
    mutate(pop = 1) %>% # fake single population
    dplyr::select(pop, all_of(gen_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  # Bootstrap He
  Fis_boot <- bootstrap_Fis(geno_hierfstat, nboot = 100)
  
  # Return results
  data.frame(
    Year = unique(df$Year),
    Nom_Pop = unique(df$Nom_Pop),
    Fis_mean = mean(Fis_boot, na.rm = TRUE),
    Fis_sd = sd(Fis_boot, na.rm = TRUE),
    N = nrow(df)
  )
})


# Combine all results
Fis_df <- do.call(rbind, resultsFis)
summary(Fis_df)

Fis_summary <- Fis_df %>%
  group_by(Nom_Pop) %>%
  summarise(
    Fis_mean_pop = mean(Fis_mean, na.rm = TRUE),
    Fis_se_pop   = mean(Fis_sd, na.rm = TRUE),
    N_total     = sum(N),
    .groups = "drop"
  )

Fis_summary

Fis_df <- Fis_df %>%
  mutate(Significance = case_when(
    Fis_mean + Fis_sd < 0 ~ "Below zero",
    Fis_mean - Fis_sd > 0 ~ "Above zero",
    TRUE ~ "Not significant"
  ))

# Code for Figure 3B
pop_colors <- c("Romaine" = "#000080", "Puyjalon" = "#DC143C")

FisLARSA <- ggplot(Fis_df, aes(x = Year, y = Fis_mean, color = Nom_Pop, group = Nom_Pop, shape = Significance)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_point(position = position_dodge(width = 0.3), size = 6) +
  geom_errorbar(
    aes(ymin = Fis_mean - Fis_sd, ymax = Fis_mean + Fis_sd),
    width = 0.2,
    position = position_dodge(width = 0.3)
  ) +
  scale_color_manual(values = pop_colors, name = "Population", guide = "none") +
  scale_shape_manual(values = c("Below zero" = 25, "Not significant" = 16, "Above zero" = 24)) +
  labs(x = "Year", y = expression("Inbreeding coefficient (F"[IS]*")")) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
FisLARSA

#Mean kinship

# read genotype file, convert to genind friendly genotypes
x <- read.delim("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 1 - Broodstock/MK_LARSEM.txt")

usats <- colnames(x)[seq(2, ncol(x), 2)]
y <- lapply(seq(2, ncol(x), 2), function(i) {
  paste(x[,i], x[,i+1], sep = "")
})
names(y) <- usats
x <- data.frame(x[,1], y, stringsAsFactors = FALSE)
x[x == "00"] <- "0000"
x <- x %>% 
  rename(id = x...1.)
# read in individuals for current reproduction year and extract cohort year
z <- read.delim("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 1 - Broodstock/MK_Repro_LARSEM.txt")

#z$cohort <- as.integer(paste0(20, str_extract(z$ind, "^[0-9][0-9]")))

# perform left join to link individual metadata and genotypes
#data <- left_join(z,x, by = c("id"))
data <- bind_cols(z, x[ , -1])
no_genos <- data$ind[rowSums(is.na(data[,usats])) == length(usats)]
data[rowSums(is.na(data[,usats])) == length(usats),]

# split to population specific datasets

#Exemple for 2014
pu_data_2014 <- data %>% filter(Pop == "Puyjalon") %>% filter(Year == "2014")
pu_geno_2014 <- df2genind(pu_data_2014 %>% dplyr::select(usats), 
                     ncode = 2L, 
                     ind.names = as.character(pu_data_2014 %>% pull(id)), 
                     ploidy = 2)



calcMK <- function(genind, ...) {
  rel <- coancestry(data.frame(ind = indNames(genind), genind2df(genind, oneColPerAll = TRUE), stringsAsFactors = FALSE), 
                    error.rates = 0.01,
                    trioml = 1,
                    allow.inbreeding = TRUE)
  mat <- spread(rel$relatedness[,c(2,3,5)], ind2.id, -ind1.id)
  toadd <- setdiff(mat$ind1.id, names(mat))
  mat[,toadd] <- unlist(mat[mat$ind1.id == toadd,-1])
  MK <- colMeans(mat[,-1], na.rm = TRUE)
  return(list(rel = rel, MK = MK))
}
#Exemple for 2014
pu_2014_MK <- calcMK(pu_geno_2014)

# Make sure 'usats' contains all loci column names
loci_cols <- usats

# Unique populations and years
pops <- unique(data$Pop)
years <- sort(unique(data$Year))

# Prepare empty result table
MK_summary <- data.frame(Pop = character(),
                         Year = integer(),
                         MK_mean = numeric(),
                         MK_SE = numeric(),
                         stringsAsFactors = FALSE)

# Loop over populations and years
for (pop in pops) {
  for (yr in years) {
    # Filter data for current Pop and Year
    df_sub <- data %>% filter(Pop == pop, Year == yr)
    
    # Skip if no individuals
    if(nrow(df_sub) == 0) next
    
    # Remove individuals with missing alleles (coded as "00")
    df_sub_clean <- df_sub %>% filter(rowSums(dplyr::select(., all_of(loci_cols)) == "00") == 0)
    
    # Skip if none left
    if(nrow(df_sub_clean) == 0) {
      MK_summary <- rbind(MK_summary,
                          data.frame(Pop = pop,
                                     Year = yr,
                                     MK_mean = NA_real_,
                                     MK_SE = NA_real_))
      next
    }
    
    # Convert to genind
    geno_sub <- df2genind(df_sub_clean %>% dplyr::select(all_of(loci_cols)),
                          ncode = 2L,
                          ind.names = df_sub_clean$id,
                          ploidy = 2)
    
    # Calculate MK
    mk_res <- calcMK(geno_sub)
    
    # Average MK and SE
    MK_mean <- mean(mk_res$MK, na.rm = TRUE)
    MK_SE <- sd(mk_res$MK, na.rm = TRUE) / sqrt(length(mk_res$MK))
    
    # Store
    MK_summary <- rbind(MK_summary,
                        data.frame(Pop = pop,
                                   Year = yr,
                                   MK_mean = MK_mean,
                                   MK_SE = MK_SE))
  }
}

# View results
MK_summary
MK_summary$Pop <- as.factor(MK_summary$Pop)
summary(MK_summary)

MKmoy_summary <- MK_summary %>%
  group_by(Pop) %>%
  summarise(
    MK_mean_pop = mean(MK_mean, na.rm = TRUE),
    MK_se_pop   = mean(MK_SE, na.rm = TRUE),
    .groups = "drop"
  )
MKmoy_summary

#Plot for Figure 3C
MK <- ggplot(MK_summary, aes(x = Year, y = MK_mean, color = Pop, group = Pop)) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    aes(ymin = MK_mean - MK_SE, ymax = MK_mean + MK_SE),
    width = 0.2,
    position = position_dodge(width = 0.3)
  ) +
  scale_x_continuous(
    breaks = seq(2014, max(MK_summary$Year), 1)  # show every year from 2014
  ) +
  labs(x = "Year", y = "Mean Kinship (MK)", color = "Population") +
  scale_color_manual(values = c("Puyjalon" = "#DC143C", "Romaine" = "#000080")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

MK

##Figure3
BroodstockCons<-ggdraw()+
  draw_plot(arLARSA, x = 0, y = 0.66, width = 1, height = 0.33)+
  draw_plot(FisLARSA, x = 0, y = 0.33, width = 1, height = 0.33)+
  draw_plot(MK, x = 0, y = 0, width = 1, height = 0.33)+
  draw_plot_label(label = c("A","B","C"), size = 18,
                  x = c(0,0,0), y = c(0.99,0.66,0.33))
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/BroodstockCons.png",width = 11, height = 16, dpi = 900)

#Figure S7
BroodstockCons2<-ggdraw()+
  draw_plot(reproAssigned, x = 0, y = 0.66, width = 1, height = 0.33)+
  draw_plot(heLARSA, x = 0, y = 0.33, width = 1, height = 0.33)+
  draw_plot(hoLARSA, x = 0, y = 0, width = 1, height = 0.33)+
  draw_plot_label(label = c("A","B","C"), size = 18,
                  x = c(0,0,0), y = c(0.99,0.66,0.33))
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Supp Mat/BroodstockCons2.png",width = 11, height = 16, dpi = 900)

##Meankinship per individuals
# Prepare empty dataframe for all individual MKs
MK_individuals <- data.frame(Pop = character(),
                             Year = integer(),
                             id = character(),
                             MK = numeric(),
                             stringsAsFactors = FALSE)

# Loop over populations and years
for (pop in pops) {
  for (yr in years) {
    # Filter data
    df_sub <- data %>% filter(Pop == pop, Year == yr)
    if(nrow(df_sub) == 0) next
    
    # Remove individuals with missing alleles
    df_sub_clean <- df_sub %>% filter(rowSums(dplyr::select(., all_of(loci_cols)) == "00") == 0)
    if(nrow(df_sub_clean) == 0) next
    
    # Convert to genind
    geno_sub <- df2genind(df_sub_clean %>% dplyr::select(all_of(loci_cols)),
                          ncode = 2L,
                          ind.names = df_sub_clean$id,
                          ploidy = 2)
    
    # Calculate MK
    mk_res <- calcMK(geno_sub)
    
    # Store individual MKs
    MK_individuals <- rbind(MK_individuals,
                            data.frame(Pop = pop,
                                       Year = yr,
                                       id = names(mk_res$MK),
                                       MK = mk_res$MK))
  }
}

# Check result
head(MK_individuals)

# Fit the linear mixed model
library(lmerTest)
mod <- lmer(MK ~ Pop + (1 | Year), data = MK_individuals)
summary(mod)
mod0 <- lmer(MK ~ (1 | Year), data = MK_individuals)
anova(mod0, mod)




####Objective_2_ Stocking effect on demography and genetic diversity ____________________________________________________________________________________

###3.2.2 Demography in the watershed
##3.2.2.1 Relative contribution of Romaine and Puyjalon populations and stocked fish in smolts

# Import data set of all individual used in the project
assign <- read.delim("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/assign.txt")
assign[] <- lapply(assign, function(x) {
  if (is.character(x) || is.numeric(x)) as.factor(x) else x
})
summary(assign)

#Subset between stages 
assignWSP <- subset(assign, assign$Origin == "SmoltWSP")
summary(assignWSP)
table(assignWSP$Population, assignWSP$Assigned)
assignFry <- subset(assign, assign$Origin == "Alevins")
summary(assignFry)
table(assignFry$Population, assignFry$Assigned)
assignParr <- subset(assign, assign$Origin == "Tacons")
summary(assignParr)
table(assignParr$Population, assignParr$Assigned)

#Population and sex ratio in the watershed using outstream migrating smolts

# Population Proportion summary
prop_summary <- assignWSP %>%
  filter(Population %in% c("Puyjalon", "Romaine")) %>%   # keep only the two populations
  group_by(Year, Population) %>%
  summarise(N = n(), .groups = "drop_last") %>%          # count individuals per year/pop
  mutate(Total = sum(N),
         Prop = N / Total) %>%                           # proportion per year
  group_by(Population) %>%
  summarise(
    Mean = mean(Prop, na.rm = TRUE),
    Min  = min(Prop, na.rm = TRUE),
    Max  = max(Prop, na.rm = TRUE),
    SE   = sd(Prop, na.rm = TRUE) / sqrt(n()),           # standard error across years
    .groups = "drop"
  )

prop_summary

# Sex ratio: proportion of males per year/population
sex_ratio_summary <- assignWSP %>%
  filter(Population %in% c("Puyjalon", "Romaine"),
         Sex %in% c("M","F")) %>%
  group_by(Year, Population) %>%
  summarise(
    N_total = n(),
    N_male  = sum(Sex == "M"),
    PropMale = N_male / N_total,
    .groups = "drop"
  ) %>%
  group_by(Population) %>%
  summarise(
    Mean = mean(PropMale, na.rm = TRUE),
    Min  = min(PropMale, na.rm = TRUE),
    Max  = max(PropMale, na.rm = TRUE),
    SE   = sd(PropMale, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
sex_ratio_summary

#Model to determine influence of population and stage on sex ratio

# Filter only the desired stages and sexes
dat_stage <- assign %>%
  filter(Origin %in% c("Alevins", "SmoltWSP", "Tacons"),
         Sex %in% c("M", "F"),
         Population %in% c("Puyjalon", "Romaine")) %>%
  mutate(Sex_bin = ifelse(Sex == "M", 1, 0))

# Fit GLMM with Population, Stage, and interaction
mod_stage <- glmer(Sex_bin ~ Population * Stage + (1 | Year),
                   family = binomial,
                   data = dat_stage)

summary(mod_stage)

mod_stage_noInt <- glmer(Sex_bin ~ Population + Stage + (1 | Year),
                         family = binomial, data = dat_stage)
summary(mod_stage_noInt)
anova(mod_stage, mod_stage_noInt)

#Selecting the model with interactions
library(ggeffects)
preds <- ggpredict(mod_stage, terms = c("Stage", "Population"))
plot(preds)  # nice grouped plot
preds

#Proportion of assigned fish in all outsream migration year (Figure 4A)

StockedProp <- assignWSP %>%
  filter(Population %in% names(pop_colors), Assigned %in% c("oui", "non")) %>%
  group_by(Year, Population, Assigned) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year, Population) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = factor(Year), y = proportion, fill = Assigned)) +
  geom_bar(stat = "identity") +
  labs(x = "Year of downstream migration", y = "Proportion of smolts")+
  scale_fill_manual(
    values = c("oui" = "#D55E00", "non" = "#FFD700"),
    labels = c("oui" = "Hatchery-born", "non" = "Wild-born")
  ) +
  scale_y_continuous(labels = percent_format()) +
  ggh4x::facet_wrap2(
    ~ Population,
    strip = ggh4x::strip_themed(
      background_x = lapply(pop_colors, function(col) element_rect(fill = col, color = NA)),
      text_x = lapply(names(pop_colors), function(x) element_text(
        color = "white", face = "bold", size = 14,
        margin = margin(t = 6, b = 6)  # add top/bottom margin
      ))
    )
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

StockedProp

# Summary of assigned fish proportion per year and population

assignWSP$Year <- as.numeric(assignWSP$Year)+2013
assign_WSP_2019 <- assignWSP %>%
  filter(Year >= 2019)
assigned_summary_2019 <- assign_WSP_2019 %>%
  filter(Population %in% c("Puyjalon", "Romaine"),
         Year >= 2019,# post-2019
         Assigned %in% c("oui", "non")) %>%
  group_by(Year, Population) %>%
  summarise(
    N_total = n(),
    N_assigned = sum(Assigned == "oui"),
    PropAssigned = N_assigned / N_total,
    .groups = "drop"
  ) %>%
  group_by(Population) %>%
  summarise(
    Mean = mean(PropAssigned, na.rm = TRUE),
    Min  = min(PropAssigned, na.rm = TRUE),
    Max  = max(PropAssigned, na.rm = TRUE),
    SE   = sd(PropAssigned, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

assigned_summary_2019


# Prepare data: only years >= 2020 (after first stocking in 2017)
dat_assigned_post2019 <- assignWSP %>%
  filter(Population %in% c("Puyjalon", "Romaine"),
         Year >= 2019,
         Assigned %in% c("oui", "non")) %>%
  mutate(Assigned_bin = ifelse(Assigned == "oui", 1, 0))

# GLMM: Probability of being assigned, post-2019
mod_assigned_post2019 <- glmer(Assigned_bin ~ Population + (1 | Year),
                               data = dat_assigned_post2019,
                               family = binomial)

summary(mod_assigned_post2019)

##3.2.2.2 Smolt abundance estimate
## CMR per year (Figure 4B)

CMR_counts <- read.delim2("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/CMR_counts.txt")
CMR_counts$Type <- as.factor(CMR_counts$Type)
summary(CMR_counts)

CMR_summary <- CMR_counts %>%
  mutate(Population = case_when(
    grepl("Puyjalon", Type) ~ "Puyjalon",
    grepl("Romaine", Type) ~ "Romaine"
  )) %>%
  group_by(Year, Population) %>%
  summarise(
    Estimate = sum(Estimate, na.rm = TRUE),
    Lower = sum(Lower, na.rm = TRUE),
    Upper = sum(Upper, na.rm = TRUE),
    .groups = "drop"
  )

# Check result
CMR_summary

CMR <- ggplot(CMR_summary, aes(x = as.factor(Year), y = Estimate, color = Population, group = Population)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Population), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("Puyjalon" = "#DC143C", "Romaine" = "#000080")) +
  scale_fill_manual(values = c("Puyjalon" = "#DC143C", "Romaine" = "#000080")) +
  labs(x = "Year", y = "Estimated abundance (CMR)") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
CMR


trend_results <- CMR_summary %>%
  group_by(Population) %>%
  do({
    model <- lm(Estimate ~ Year, data = .)
    tidy <- broom::tidy(model)
    glance <- broom::glance(model)
    data.frame(
      Slope = tidy$estimate[2],
      p_value = tidy$p.value[2],
      R2 = glance$r.squared
    )
  })

trend_results

##3.2.2.3 Nest counting
#Nest counts
Nids <- read.delim("~/Disque dur/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Nids.txt")
Nids$Population <- as.factor(Nids$Population)
summary(Nids)

Nids_sum <- Nids %>%
  filter(Population %in% c("Puyjalon", "Romaine", "Tributaries")) %>%
  group_by(Fraie, Population) %>%
  summarise(N = sum(N, na.rm = TRUE), .groups = "drop") %>%
  # Combine Puyjalon + Tributaries, then rename to Puyjalon
  mutate(Population = ifelse(Population %in% c("Puyjalon", "Tributaries"),
                             "Puyjalon", "Romaine")) %>%
  group_by(Fraie, Population) %>%
  summarise(N = sum(N), .groups = "drop")

Nids_summary <- Nids_sum %>%
  group_by(Population) %>%
  summarise(
    mean_N = mean(N, na.rm = TRUE),
    se_N   = sd(N, na.rm = TRUE) / sqrt(n()),
    n_years = n(),
    .groups = "drop"
  )

Nids_summary


#glmnb for counts

mod_nb <- glm.nb(N ~ Fraie * Population, data = Nids_sum)
summary(mod_nb)

##3.2.2.4 Adults return

Adults <- read.delim("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Adults.txt")
summary(Adults)

# 1. Rename columns for clarity
Adults_renamed <- Adults %>%
  rename(
    `1SW` = X1SW,
    MSW = MSW,       # stays the same
    Total = Total    # stays the same
  )

# 2. Reshape to long format
Adults_long <- Adults_renamed %>%
  pivot_longer(
    cols = c(`1SW`, MSW),
    names_to = "Sea_age",
    values_to = "Count"
  )


#Figure 4C
NidAdulte <- ggplot() +
  # Line + points for nests
  geom_line(data = Nids_sum, aes(x = Fraie, y = N, color = Population, group = Population), size = 1) +
  geom_point(data = Nids_sum, aes(x = Fraie, y = N, color = Population), size = 3) +
  
  # Bars for adults
  geom_col(data = Adults_long, aes(x = Year, y = Count, fill = Sea_age), color = "black", width = 0.8, alpha = 0.6) +
  
  # Labels and scales
  labs(x = "Year", y = "Number of nests and adults", color = "Population", fill = "Sea-age") +
  scale_x_continuous(breaks = 2003:2024, limits = c(2003, 2025)) +
  scale_color_manual(values = pop_colors) +
  scale_fill_manual(values = c("1SW" = "#492050", "MSW" = "#2C792D"),labels = c("1SW" = "1 Sea Winter", "MSW" = "Multi Sea Winter")) +
  
  # Theme
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    legend.position = "bottom"
  )

NidAdulte

#Figure 4A
Demography <-ggdraw()+
  draw_plot(StockedProp, x = 0, y = 0.66, width = 1, height = 0.33)+
  draw_plot(CMR, x = 0, y = 0.33, width = 1, height = 0.33)+
  draw_plot(NidAdulte, x = 0, y = 0, width = 1, height = 0.33)+
  draw_plot_label(label = c("A","B","C"), size = 18,
                  x = c(0,0,0), y = c(0.99,0.67,0.34))
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Demography.png",width = 13, height = 16, dpi = 900)




# 1. Calculate proportion/ratio
Adults_stats <- Adults %>%
  mutate(
    SW_ratio = X1SW / Total
  )

# 2. Summarize overall (across all years)
summary_overall <- Adults_stats %>%
  summarise(
    Mean_Adults = mean(Total),
    SE_Adults = sd(Total)/sqrt(n()),
    Mean_SW_ratio = mean(SW_ratio),
    SE_SW_ratio = sd(SW_ratio)/sqrt(n())
  )

summary_overall


# 1. Prepare data (exclude 2010)
Adults_trend <- Adults %>%
  filter(Year != 2010) %>%
  mutate(
    SW_ratio = X1SW / MSW,
    Total = Total
  )

# 2. Linear model: trend in number of adults
lm_adults <- lm(Total ~ Year, data = Adults_trend)
summary(lm_adults)


# 3. Binomial model: sea-age ratio (1SW vs MSW)
# Using cbind(1SW, MSW) for binomial response
glm_sea_age <- glm(cbind(X1SW, MSW) ~ Year, 
                   data = Adults, 
                   family = binomial)
summary(glm_sea_age)



##3.2.2.5 Reproduction of stocked fish
# Using RSS file that summarize findings from Colony (available in Parentage assigment/Reproductive success)
setwd("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse")
RSS <- read.delim("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/RSS.txt")
RSS[] <- lapply(RSS, as.factor)
summary(RSS)

RSS <- RSS %>%
  mutate(
    InferredDad = na_if(as.character(InferredDad), ""),
    InferredMom = na_if(as.character(InferredMom), ""),
    PopDad      = na_if(as.character(PopDad), ""),
    PopMom      = na_if(as.character(PopMom), ""),
    YearDad     = na_if(as.character(YearDad), ""),
    YearMom     = na_if(as.character(YearMom), ""),
    StatusDad   = na_if(as.character(StatusDad), ""),
    StatusMom   = na_if(as.character(StatusMom), "")
  ) %>%
  mutate(
    ParentID     = coalesce(InferredDad, InferredMom),
    ParentPop    = coalesce(PopDad, PopMom),
    ParentYear   = coalesce(YearDad, YearMom),
    ParentStatus = coalesce(StatusDad, StatusMom),
    ParentSex    = case_when(
      !is.na(InferredDad) ~ "Male",
      !is.na(InferredMom) ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(-InferredDad, -InferredMom,
         -PopDad, -PopMom,
         -YearDad, -YearMom,
         -StatusDad, -StatusMom)
RSS[] <- lapply(RSS, as.factor)
summary(RSS)

Adults_Gen <- read.delim("E:/Thèse/Chapitre I et II/Chapitre II/Soumission/Analyse/Adults_Gen.txt")
Adults_Gen[] <- lapply(Adults_Gen, as.factor)
summary(Adults_Gen)

#Adult parents
adult_parents <- RSS %>%
  filter(Parent_Stage == "Adult")
summary(adult_parents)

adult_summary <- RSS %>%
  filter(Parent_Stage == "Adult") %>%
  group_by(ParentPop, ParentStatus) %>%
  summarise(
    n_offspring = n(),
    n_unique_parents = n_distinct(ParentID)
  ) %>%
  arrange(ParentPop, ParentStatus)
adult_summary

#ggplot(adult_summary, aes(x = ParentPop, y = n_offspring, fill = ParentSex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ParentStatus) +
  labs(y = "Number of offspring", x = "Parent population") +
  theme_classic()

# Proportion of offspring with hatchery parent by population
hatchery_by_pop <- adult_parents %>%
  group_by(ParentPop) %>%
  summarise(
    total_offspring     = n(),                           # all assigned offspring
    hatchery_offspring  = sum(ParentStatus == "oui"),    # offspring from hatchery parents
    prop_hatchery       = hatchery_offspring / total_offspring
  ) %>%
  arrange(ParentPop)

hatchery_by_pop


# Table for all genotyped adults
tab_all <- table(Adults_Gen$Pop, Adults_Gen$ParentStatus)

# Table for adults that produced offspring
tab_parents <- table(adult_parents$ParentPop, adult_parents$ParentStatus)

tab_all
tab_parents

# Add a column indicating dataset
Adults_Gen$Group <- "AllAdults"
adult_parents$Group <- "Parents"

# Bind together
combined <- rbind(
  Adults_Gen %>% dplyr::select(Pop, ParentStatus, Group),
  adult_parents %>% dplyr::select(Pop = ParentPop, ParentStatus, Group)
)

# Contingency table: (Pop × Status) by Group
tab_combined <- table(combined$Group, combined$ParentStatus, combined$Pop)
tab_combined
# Example: check Romaine only
chisq.test(tab_combined[,, "Romaine"])

# Example: check Puyjalon only
fisher.test(tab_combined[,, "Puyjalon"])


#Smolt parents
smolt_parents <- RSS %>%
  filter(Parent_Stage == "Smolt")

smolt_summary <- RSS %>%
  filter(Parent_Stage == "Smolt") %>%
  group_by(ParentPop, ParentStatus) %>%
  summarise(
    n_offspring = n(),
    n_unique_parents = n_distinct(ParentID)
  ) %>%
  arrange(ParentPop, ParentStatus)
smolt_summary

ggplot(smolt_summary, aes(x = ParentPop, y = n_offspring, fill = ParentSex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ParentStatus) +
  labs(y = "Number of offspring", x = "Parent population") +
  theme_classic()

# Proportion of offspring with hatchery parent by population
hatchery_by_pop_smolt <- smolt_parents %>%
  group_by(ParentPop) %>%
  summarise(
    total_offspring     = n(),                           # all assigned offspring
    hatchery_offspring  = sum(ParentStatus == "oui"),    # offspring from hatchery parents
    prop_hatchery       = hatchery_offspring / total_offspring
  ) %>%
  arrange(ParentPop)

hatchery_by_pop_smolt

###3.2.3 Estimation of Ryman-Laikre effect

##This part calculate Ncped using pedigree data for assigned individuals (with parents identified in the broodstock)
##Nw and NcLD were estimated using NeEstimator. Output are available in the NeEstimator file.
##Overall results (Table S8) are available in excel file Ryman-Laikre
#Mom
females_by_year <- cross %>%
  filter(!is.na(F_ID), !is.na(Stocking_Year)) %>%
  group_by(Stocking_Year, F_ID) %>%
  summarise(
    Population = unique(OrigineF),
    .groups = "drop"
  ) %>%
  arrange(Stocking_Year, F_ID)


offspring_counts_mom <- assignWSP %>%
  filter(Assigned == "oui", Population != "Ind", !is.na(Mom)) %>%
  mutate(
    Stocking_Year = as.numeric(as.character(Stocking_Year))
  ) %>%
  group_by(Population, Stocking_Year, Mom) %>%
  summarise(offspring_counts_mom = n(), .groups = "drop")

mom_all <- females_by_year %>%
  mutate(
    Stocking_Year = as.numeric(as.character(Stocking_Year))  # convert factor → numeric
  ) %>%
  left_join(
    offspring_counts_mom %>%
      mutate(Stocking_Year = as.numeric(Stocking_Year)),     # ensure same type
    by = c("Population", "Stocking_Year", "F_ID" = "Mom")
  ) %>%
  mutate(
    offspring_counts_mom = replace_na(offspring_counts_mom, 0)
  )

mom_summary <- mom_all %>%
  group_by(Population, Stocking_Year) %>%
  summarise(
    Nf = n(),
    total_offspring = sum(offspring_counts_mom),
    k_f = mean(offspring_counts_mom),
    V_kf = var(offspring_counts_mom),
    .groups = "drop"
  )

#Dad
males_by_year <- cross %>%
  filter(!is.na(M_ID), !is.na(Stocking_Year)) %>%
  group_by(Stocking_Year, M_ID) %>%
  summarise(
    Population = unique(Origine_M),
    .groups = "drop"
  ) %>%
  arrange(Stocking_Year, M_ID)


offspring_counts_dad <- assignWSP %>%
  filter(Assigned == "oui", Population != "Ind", !is.na(Dad)) %>%
  mutate(
    Stocking_Year = as.numeric(as.character(Stocking_Year))
  ) %>%
  group_by(Population, Stocking_Year, Dad) %>%
  summarise(offspring_counts_dad = n(), .groups = "drop")

dad_all <- males_by_year %>%
  mutate(
    Stocking_Year = as.numeric(as.character(Stocking_Year))  # convert factor → numeric
  ) %>%
  left_join(
    offspring_counts_dad %>%
      mutate(Stocking_Year = as.numeric(Stocking_Year)),     # ensure same type
    by = c("Population", "Stocking_Year", "M_ID" = "Dad")
  ) %>%
  mutate(
    offspring_counts_dad = replace_na(offspring_counts_dad, 0)
  )

dad_summary <- dad_all %>%
  group_by(Population, Stocking_Year) %>%
  summarise(
    Nm = n(),
    total_offspring = sum(offspring_counts_dad),
    k_m = mean(offspring_counts_dad),
    V_km = var(offspring_counts_dad),
    .groups = "drop"
  )

# ---- Assemble ----
Ne_summary <- full_join(
  mom_summary %>%
    rename(
      Nf = Nf,
      total_offspring_f = total_offspring,
      kf_mom = k_f,
      Vkf_mom = V_kf
    ),
  dad_summary %>%
    rename(
      Nm = Nm,
      total_offspring_m = total_offspring,
      kf_dad = k_m,
      Vkf_dad = V_km
    ),
  by = c("Population", "Stocking_Year")
) %>%
  mutate(
    # Combine offspring counts from both sexes
    total_offspring = coalesce(total_offspring_f, 0) + coalesce(total_offspring_m, 0),
    
    # Compute Nb_f (mothers)
    Nb_f = ifelse(
      is.na(Vkf_mom) | is.na(kf_mom) | Nf == 0 | kf_mom == 0, NA,
      (kf_mom * Nf - 2) / (kf_mom - 1 + Vkf_mom / kf_mom)
    ),
    
    # Compute Nb_m (fathers)
    Nb_m = ifelse(
      is.na(Vkf_dad) | is.na(kf_dad) | Nm == 0 | kf_dad == 0, NA,
      (kf_dad * Nm - 2) / (kf_dad - 1 + Vkf_dad / kf_dad)
    ),
    
    # Combine both sexes to get total Ne (breeding effective size)
    Ne_total = ifelse(
      is.na(Nb_f) | is.na(Nb_m) | (Nb_f + Nb_m) == 0, NA,
      4 * Nb_f * Nb_m / (Nb_f + Nb_m)
    ),
    
    # Optional: Adjust year to align with Ne_pop_by_year
    Year = Stocking_Year - 1
  ) %>%
  dplyr::select(
    Population, Year, Stocking_Year,
    Nf, Nm, total_offspring,
    kf_mom, Vkf_mom, kf_dad, Vkf_dad,
    Nb_f, Nb_m, Ne_total
  )

# ---- Display final result ----
Ne_summary




### 3.2.4 Genetic diversity in smolts from oustream migration

setwd("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier")

#Import info on those smolts
WSP_Gen <- read.delim("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 2 - Stocking effect on demography and genetic diversity/WSP_Gen.txt")
WSP_Gen_1 <- WSP_Gen[,2:8]
#Import their genotype
WSP_Gen_Stru  <- read.structure("STRUCTURE_Filtered_Individuals2261_Loci41.stru", 
                                n.ind = 2261, n.loc = 41, col.lab = 1, col.pop = 2,
                                row.marknames = 1, ask = FALSE, onerowperind = TRUE)
WSP_Gen_Hier <- genind2hierfstat(WSP_Gen_Stru)
WSP_Gen_Hier<-bind_cols(WSP_Gen_1,WSP_Gen_Hier)
WSP_Gen_Hier$NumUnique <- as.factor(WSP_Gen_Hier$NumUnique)
WSP_Gen_Hier$PopUniq <- as.factor(WSP_Gen_Hier$PopUniq)
WSP_Gen_Hier$Nom_Pop <- as.factor(WSP_Gen_Hier$Nom_Pop)
WSP_Gen_Hier$Assigned <- as.factor(WSP_Gen_Hier$Assigned)
WSP_Gen_Hier$Sexe <- as.factor(WSP_Gen_Hier$Sexe)
WSP_Gen_Hier$Year<- as.factor(WSP_Gen_Hier$Year)
WSP_Gen_Hier$Cohorte<- as.factor(WSP_Gen_Hier$Cohorte)
summary(WSP_Gen_Hier)


#Same function as for broodstock diversity were used.

##Expected heterozyosity
# calculate He (Hs) from bootstraps
bootstrap_he <- function(data, nboot = 10) {
  replicate(nboot, {
    sampled <- data[sample(1:nrow(data), replace = TRUE), ]
    bs <- basic.stats(sampled)$Hs
    mean(bs, na.rm = TRUE)
  })
}

# Prepare genetic data
gen_cols <- grep("^Ssa|^NGS", names(WSP_Gen_Hier), value = TRUE)
meta_cols <- c("Cohorte", "Nom_Pop", "Assigned")
filtered_data <- WSP_Gen_Hier %>% filter(!is.na(Cohorte)) %>%
  dplyr::select(all_of(meta_cols), all_of(gen_cols))

# Remove rows with too much missing data (optional but recommended)
filtered_data <- filtered_data[rowMeans(is.na(filtered_data[gen_cols])) < 0.2, ]

# Split by group
grouped <- split(filtered_data, interaction(filtered_data$Cohorte, 
                                            filtered_data$Nom_Pop, 
                                            filtered_data$Assigned, drop = TRUE))


# Bootstrap and store results
results <- lapply(names(grouped), function(grp_name) {
  df <- grouped[[grp_name]]
  if (nrow(df) < 6) return(NULL)
  
  # Remove metadata before computing stats
  geno <- df[, gen_cols]
  
  # Convert to hierfstat format
  geno_hierfstat <- df %>%
    mutate(pop = 1) %>% # fake single population
    dplyr::select(pop, all_of(gen_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  # Bootstrap He
  he_boot <- bootstrap_he(geno_hierfstat, nboot = 100)
  
  # Return results
  data.frame(
    Cohorte = unique(df$Cohorte),
    Nom_Pop = unique(df$Nom_Pop),
    Assigned = unique(df$Assigned),
    He_mean = mean(he_boot, na.rm = TRUE),
    He_sd = sd(he_boot, na.rm = TRUE),
    N = nrow(df)
  )
})

# Combine all results
he_df <- do.call(rbind, results)

# Clean Cohorte (in case of bad formatting)
he_df <- subset(he_df, he_df$Cohorte!="#VALEUR!")

# Assigned status color and label
assigned_colors <- c("oui" = "#D55E00", 
                     "non" = "#FFD700")
assigned_labels <- c("oui" = "Hatchery-born", 
                     "non" = "Wild-born")

# Facet strip background colors per Nom_Pop
pop_colors <- c("Romaine" ="#DC143C" , "Puyjalon" = "#000080")

he <- ggplot(
  he_df %>% filter(Assigned != "all"),
  aes(x = Cohorte, y = He_mean, color = Assigned)
) +
  geom_line(aes(group = interaction(Nom_Pop, Assigned))) +
  geom_point() +
  geom_errorbar(aes(ymin = He_mean - He_sd, ymax = He_mean + He_sd), width = 0.2) +
  facet_wrap2(
    ~Nom_Pop,
    strip = strip_themed(
      background_x = lapply(pop_colors, function(col) element_rect(fill = col)),
      text_x = lapply(names(pop_colors), function(name)
        element_text(face = "bold", color = "white", size = 14)
      ) |> `names<-`(names(pop_colors))
    )
  ) +
  scale_color_manual(values = assigned_colors, labels = assigned_labels, name = "Origin",guide = "none") +
  labs(x = "Cohort", y = expression("Expected Heterozygosity (H"[E]*")")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

he
he_summary <- he_df %>%
  group_by(Nom_Pop, Assigned) %>%
  summarise(
    mean_He = mean(He_mean, na.rm = TRUE),
    mean_sd = mean(He_sd, na.rm = TRUE),
    n_groups = n(),  # optional: number of cohorts per group
    .groups = "drop"
  )

he_summary

lm_res <- lm(He_mean ~ Nom_Pop * Assigned, data = he_df)
summary(lm_res)


# Summary of fixed effects
summary(lmm)

#Observed heterozygosity________________________

# calculate Ho (Hs) from bootstraps
bootstrap_ho <- function(data, nboot = 10) {
  replicate(nboot, {
    sampled <- data[sample(1:nrow(data), replace = TRUE), ]
    bs <- basic.stats(sampled)$Ho
    mean(bs, na.rm = TRUE)
  })
}


# Bootstrap and store results
resultsHo <- lapply(names(grouped), function(grp_name) {
  df <- grouped[[grp_name]]
  if (nrow(df) < 6) return(NULL)
  
  # Remove metadata before computing stats
  geno <- df[, gen_cols]
  
  # Convert to hierfstat format
  geno_hierfstat <- df %>%
    mutate(pop = 1) %>% # fake single population
    dplyr::select(pop, all_of(gen_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  # Bootstrap He
  ho_boot <- bootstrap_ho(geno_hierfstat, nboot = 100)
  
  # Return results
  data.frame(
    Cohorte = unique(df$Cohorte),
    Nom_Pop = unique(df$Nom_Pop),
    Assigned = unique(df$Assigned),
    Ho_mean = mean(ho_boot, na.rm = TRUE),
    Ho_sd = sd(ho_boot, na.rm = TRUE),
    N = nrow(df)
  )
})

# Combine all results
ho_df <- do.call(rbind, resultsHo)

# Clean Cohorte (in case of bad formatting)
ho_df <- subset(ho_df, ho_df$Cohorte!="#VALEUR!")

ho <- ggplot(
  ho_df %>% filter(Assigned != "all"),
  aes(x = Cohorte, y = Ho_mean, color = Assigned)
) +
  geom_line(aes(group = interaction(Nom_Pop, Assigned))) +
  geom_point() +
  geom_errorbar(aes(ymin = Ho_mean - Ho_sd, ymax = Ho_mean + Ho_sd), width = 0.2) +
  facet_wrap2(
    ~Nom_Pop,
    strip = strip_themed(
      background_x = lapply(pop_colors, function(col) element_rect(fill = col)),
      text_x = lapply(names(pop_colors), function(name) element_text(face = "bold", color = "white", size = 14)) |> 
        `names<-`(names(pop_colors))
    )
  ) +
  scale_color_manual(values = assigned_colors, labels = assigned_labels, name = "Origin") +
  labs(x = "Cohort", y = expression("Observed Heterozygosity (H"[O]*")")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ho
ho_summary <- ho_df %>%
  group_by(Nom_Pop, Assigned) %>%
  summarise(
    mean_Ho = mean(Ho_mean, na.rm = TRUE),
    mean_sd = mean(Ho_sd, na.rm = TRUE),
    n_groups = n(),  # optional: number of cohorts per group
    .groups = "drop"
  )

ho_summary

lm_resHo <- lm(Ho_mean ~ Nom_Pop + Assigned, data = ho_df)
summary(lm_resHo)


#Inbreeding coefficient________________________

# calculate fis (Hs) from bootstraps
bootstrap_fis <- function(data, nboot = 10) {
  replicate(nboot, {
    sampled <- data[sample(1:nrow(data), replace = TRUE), ]
    bs <- basic.stats(sampled)$Fis
    mean(bs, na.rm = TRUE)
  })
}


# Bootstrap and store results
resultsfis <- lapply(names(grouped), function(grp_name) {
  df <- grouped[[grp_name]]
  if (nrow(df) < 6) return(NULL)
  
  # Remove metadata before computing stats
  geno <- df[, gen_cols]
  
  # Convert to hierfstat format
  geno_hierfstat <- df %>%
    mutate(pop = 1) %>% # fake single population
    dplyr::select(pop, all_of(gen_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  # Bootstrap He
  fis_boot <- bootstrap_fis(geno_hierfstat, nboot = 100)
  
  # Return results
  data.frame(
    Cohorte = unique(df$Cohorte),
    Nom_Pop = unique(df$Nom_Pop),
    Assigned = unique(df$Assigned),
    fis_mean = mean(fis_boot, na.rm = TRUE),
    fis_sd = sd(fis_boot, na.rm = TRUE),
    N = nrow(df)
  )
})

# Combine all results
fis_df <- do.call(rbind, resultsfis)

# Clean Cohorte (in case of bad formatting)
fis_df <- subset(fis_df, fis_df$Cohorte!="#VALEUR!")


fis_df <- fis_df %>%
  mutate(Significance = case_when(
    fis_mean - fis_sd > 0 ~ "Above zero",
    fis_mean + fis_sd < 0 ~ "Below zero",
    TRUE ~ "Not significant"
  ))


fis <- ggplot(
  fis_df %>% filter(Assigned != "all"),
  aes(x = Cohorte, y = fis_mean, color = Assigned, shape = Significance)
)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_line(aes(group = interaction(Nom_Pop, Assigned))) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = fis_mean - fis_sd, ymax = fis_mean + fis_sd), width = 0.2) +
  facet_wrap2(
    ~Nom_Pop,
    strip = strip_themed(
      background_x = lapply(pop_colors, function(col) element_rect(fill = col)),
      text_x = lapply(names(pop_colors), function(name) element_text(face = "bold", color = "white", size = 14)) |> 
        `names<-`(names(pop_colors))
    )
  ) +
  scale_color_manual(values = assigned_colors, labels = assigned_labels, name = "Origin") +
  scale_shape_manual(values = c(24, 25, 16)) +  # filled circle, triangle, open circle
  labs(x = "Cohort", y = expression("Inbreeding coefficient (F"[IS]*")"), shape = "Significance") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom",
    #legend.box = "vertical",         # Stack legends vertically
    legend.box.just = "center"       # Center align them
  )
fis

fis_summary <- fis_df %>%
  group_by(Nom_Pop, Assigned) %>%
  summarise(
    mean_fis = mean(fis_mean, na.rm = TRUE),
    mean_sd = mean(fis_sd, na.rm = TRUE),
    n_groups = n(),  # optional: number of cohorts per group
    .groups = "drop"
  )

fis_summary

lm_resFis <- lm(fis_mean ~ Nom_Pop + Assigned, data = fis_df)
summary(lm_resFis)


##Ar__________________________________________________

# -------------------------
gen_cols <- grep("^Ssa|^NGS", names(WSP_Gen_Hier), value = TRUE)

# Drop rows with missing Cohorte/Nom_Pop/Assigned
WSP_clean <- WSP_Gen_Hier %>%
  filter(!is.na(Cohorte) & !is.na(Nom_Pop) & !is.na(Assigned))

# -------------------------
# Bootstrap function
# -------------------------
bootstrap_ar <- function(data, nboot = 100) {
  if (nrow(data) < 10) return(NULL)
  
  # Prepare hierfstat format (first column = population)
  data_hierf <- data.frame(pop = rep(1, nrow(data)), data[, gen_cols])
  data_hierf <- droplevels(data_hierf)
  
  replicate(nboot, {
    sampled <- data_hierf[sample(1:nrow(data_hierf), replace = TRUE), ]
    ar <- tryCatch(
      allelic.richness(sampled),
      error = function(e) NA
    )
    if (is.list(ar)) {
      mean(ar$Ar[1, ], na.rm = TRUE)  # average across loci
    } else {
      NA
    }
  })
}

# =========================
# 1️⃣ Bootstrap by Assigned
# =========================
grouped_assigned <- WSP_clean %>%
  group_by(Cohorte, Nom_Pop, Assigned) %>%
  group_split()

ar_results_assigned <- lapply(grouped_assigned, function(group) {
  if (nrow(group) < 10) return(NULL)
  bs <- bootstrap_ar(group)
  data.frame(
    Cohorte = unique(group$Cohorte),
    Nom_Pop = unique(group$Nom_Pop),
    Assigned = unique(group$Assigned),
    Ar_mean = mean(bs, na.rm = TRUE),
    Ar_sd = sd(bs, na.rm = TRUE),
    N = nrow(group)
  )
}) |> bind_rows()


ar_df_combined <- ar_results_assigned

# Ensure factor levels for proper plotting
ar_df_combined$Cohorte <- factor(ar_df_combined$Cohorte, levels = sort(unique(ar_df_combined$Cohorte)))
ar_df_combined$Nom_Pop <- factor(ar_df_combined$Nom_Pop, levels = c("Puyjalon", "Romaine"))
ar_df_combined$Assigned <- factor(ar_df_combined$Assigned, levels = c("non", "oui", "all"))
# Clean Cohorte (in case of bad formatting)
ar_df_combined <- subset(ar_df_combined, ar_df_combined$Cohorte!="#VALEUR!")


# =========================
# Plot
# =========================

ar <- ggplot(
  ar_df_combined %>%filter(Assigned != "all"), 
  aes(x = Cohorte, y = Ar_mean, color = Assigned)
) +
  geom_line(aes(group = interaction(Nom_Pop, Assigned))) +
  geom_point() +
  geom_errorbar(aes(ymin = Ar_mean - Ar_sd, ymax = Ar_mean + Ar_sd), width = 0.2) +
  facet_wrap2(
    ~Nom_Pop,
    strip = strip_themed(
      background_x = lapply(pop_colors, function(col) element_rect(fill = col)),
      text_x = lapply(names(pop_colors), function(name) element_text(face = "bold", color = "white", size = 14)) |>
        `names<-`(names(pop_colors))
    )
  ) +
  scale_color_manual(values = assigned_colors, labels = assigned_labels, name = "Origin", guide = "none") +
  labs(x = "Cohort", y = expression("Allelic Richness (A"[R]*")")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ar

ar_summary <- ar_df_combined %>%
  group_by(Nom_Pop, Assigned) %>%
  summarise(
    mean_ar = mean(Ar_mean, na.rm = TRUE),
    mean_sd = mean(Ar_sd, na.rm = TRUE),
    n_groups = n(),  # optional: number of cohorts per group
    .groups = "drop"
  )

ar_summary

lm_resAr <- lm(Ar_mean ~ Nom_Pop + Assigned, data = ar_df_combined)
summary(lm_resAr)


## FST

WSP_Gen_Hier <- genind2hierfstat(WSP_Gen_Stru)
WSP_Gen_Hier<-bind_cols(WSP_Gen_1,WSP_Gen_Hier)
WSP_Gen_Hier$NumUnique <- as.factor(WSP_Gen_Hier$NumUnique)
WSP_Gen_Hier$PopUniq <- as.factor(WSP_Gen_Hier$PopUniq)
WSP_Gen_Hier$Nom_Pop <- as.factor(WSP_Gen_Hier$Nom_Pop)
WSP_Gen_Hier$Assigned <- as.factor(WSP_Gen_Hier$Assigned)
WSP_Gen_Hier$Sexe <- as.factor(WSP_Gen_Hier$Sexe)
WSP_Gen_Hier$Year<- as.factor(WSP_Gen_Hier$Year)
WSP_Gen_Hier$Cohorte<- as.factor(WSP_Gen_Hier$Cohorte)
summary(WSP_Gen_Hier)


bootstrap_fst <- function(data, n_boot = 1000) {
  boot_fst <- numeric(n_boot)
  
  # S'assurer que la colonne 'pop' est un facteur
  data$pop <- as.factor(data$pop)
  pops <- levels(data$pop)
  
  for (i in seq_len(n_boot)) {
    # Resample individuellement dans chaque population
    resampled <- lapply(pops, function(p) {
      data[data$pop == p, ] %>%
        slice_sample(n = sum(data$pop == p), replace = TRUE)
    }) %>% bind_rows()
    
    # Calcul du FST pairwise
    fst <- suppressWarnings(pairwise.neifst(resampled))
    boot_fst[i] <- fst[1, 2]  # On suppose deux populations
  }
  
  # Retourne moyenne et IC
  data.frame(
    Fst = mean(boot_fst, na.rm = TRUE),
    CI_low = quantile(boot_fst, 0.025, na.rm = TRUE),
    CI_high = quantile(boot_fst, 0.975, na.rm = TRUE)
  )
}
# Colonnes génétiques = tous les loci (à adapter si besoin)
gen_cols <- grep("^Ssa|^NGS", colnames(WSP_Gen_Hier), value = TRUE)

fst_results <- list()

# Nettoyer les noms de cohortes
WSP_Gen_Hier <- WSP_Gen_Hier %>%
  filter(!is.na(Cohorte), !is.na(pop)) %>%
  mutate(Cohorte = as.numeric(as.character(Cohorte)))

cohorte_list <- sort(unique(WSP_Gen_Hier$Cohorte))

for (coh in cohorte_list) {
  sub_data <- WSP_Gen_Hier %>%
    filter(Cohorte == coh) %>%
    dplyr::select(pop, all_of(gen_cols))
  
  # Vérifie qu’il y a bien deux pops et au moins 10 individus chacune
  if (length(unique(sub_data$pop)) == 2 &&
      all(table(sub_data$pop) >= 10)) {
    
    cat("Bootstrapping cohorte:", coh, "\n")
    res <- bootstrap_fst(sub_data, n_boot = 1000)
    res$Cohorte <- coh
    fst_results[[as.character(coh)]] <- res
  }
}

# Combine tous les résultats
fst_df <- bind_rows(fst_results)

# Define all years you want on the x-axis
all_years <- c(2015, 2017, 2018, 2019, 2020, 2021, 2022)

# Ensure Cohorte is numeric for merging
fst_df$Cohorte <- as.numeric(as.character(fst_df$Cohorte))

# Expand the dataset to include all years
fst_df_complete <- fst_df %>%
  tidyr::complete(Cohorte = all_years, fill = list(Fst = NA, CI_low = NA, CI_high = NA))

# Convert Cohorte to factor with the desired order
fst_df$Cohorte <- factor(fst_df$Cohorte, levels = c(2015, 2017, 2018, 2019, 2020, 2021, 2022))

# Convert Cohorte to numeric (for lines/ribbon)
fst_df$Cohorte_num <- as.numeric(as.character(fst_df$Cohorte))

# Plot
fst <- ggplot(fst_df, aes(x = Cohorte_num, y = Fst)) +
  geom_line(linewidth = 1.2, color = "#2c3e50") +
  geom_point(size = 3, color = "#2c3e50") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "#2c3e50") +
  scale_x_continuous(
    breaks = c(2015, 2017, 2018, 2019, 2020, 2021, 2022),
    labels = c(2015, 2017, 2018, 2019, 2020, 2021, 2022)
  ) +
  labs(
    x = "Cohort", y = expression("F"[ST]*" pairwise (Puyjalon vs Romaine)"
  )) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 18),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(0.05, 0.15))
fst



#Number of alleles and loess regression. All individuals were used for this test
All_Gen_Stru  <- read.structure("STRUCTURE_Filtered_Individuals3899_Loci41.stru", 
                                n.ind = 3899, n.loc = 41, col.lab = 1, col.pop = 2,
                                row.marknames = 1, ask = FALSE, onerowperind = TRUE)

All_Gen_Hier <- genind2hierfstat(All_Gen_Stru)
All_Gen_Info <- read.delim("~/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/Objective 2 - Stocking effect on demography and genetic diversity/All_Gen_Info.txt")
All_Gen_Hier<-bind_cols(All_Gen_Info,All_Gen_Hier)
All_Gen_Hier <- All_Gen_Hier %>%
  mutate(across(where(is.character), as.factor))
summary(All_Gen_Hier)


puy_data <- All_Gen_Hier %>%
  filter(Population == "Puyjalon") %>%
  dplyr::select(ID, Assigned, starts_with("Ssa"))  # Only keep genotype columns + ID + Assigned
rom_data <- All_Gen_Hier %>%
  filter(Population == "Romaine") %>%
  dplyr::select(ID, Assigned, starts_with("Ssa"))  # Only keep genotype columns + ID + Assigned

wild_only <- puy_data %>% filter(Assigned == "non")
ro_wild_only <- rom_data %>% filter(Assigned == "non")

count_alleles <- function(df_sample) {
  loci <- df_sample %>% dplyr::select(starts_with("Ssa"))
  # Flatten all alleles into a vector, remove NAs
  alleles <- unlist(loci)
  alleles <- alleles[!is.na(alleles)]
  length(unique(alleles))
}

# Rarefaction function
allele_rarefaction <- function(df, nmin = 50, nmax = 1500, step = 50, nboot = 1000) {
  
  sample_sizes <- seq(nmin, min(nmax, nrow(df)), by = step)
  
  results <- map_dfr(sample_sizes, function(n) {
    boot_values <- replicate(nboot, {
      sampled <- df[sample(1:nrow(df), n, replace = TRUE), ]
      count_alleles(sampled)
    })
    
    tibble(
      n_fry = n,
      mean_alleles = mean(boot_values),
      CI_low = quantile(boot_values, 0.025),
      CI_high = quantile(boot_values, 0.975)
    )
  })
  
  return(results)
}


wild_raref <- allele_rarefaction(wild_only)
raref <- allele_rarefaction(puy_data)

wild_raref$group <- "Wild only"
raref$group <- "Wild + Captive"

wild_raref$pop <- "Puyjalon"
raref$pop <- "Puyjalon"

ro_wild_raref <- allele_rarefaction(ro_wild_only)
ro_raref <- allele_rarefaction(rom_data)

ro_wild_raref$group <- "Wild only"
ro_raref$group <- "Wild + Captive"

ro_wild_raref$pop <- "Romaine"
ro_raref$pop <- "Romaine"


final_raref_df <- bind_rows(wild_raref, raref, ro_wild_raref,ro_raref)
summary(final_raref_df)
na <- ggplot(final_raref_df, aes(x = n_fry, y = mean_alleles, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.3, color = NA) +
  facet_wrap2(~pop, scales = "free_x", strip = strip_themed(
    background_x = elem_list_rect(fill = pop_colors),
    text_x = elem_list_text(color = "white", size = 14, face = "bold")
  )) +
  scale_color_manual(values = c("Wild only" = "#F7D700", "Wild + Captive" = "#2c3e50")) +
  scale_fill_manual(values = c("Wild only" = "#F7D700", "Wild + Captive" = "#2c3e50")) +
  labs(x = "Number of individuals sampled", y = "Total alleles") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(angle =45, hjust =1),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
na

#Figure 5
DivGen <-ggdraw()+
  draw_plot(fis, x = 0, y = 0.33, width = 1, height = 0.33)+
  draw_plot(ar, x = 0, y = 0.66, width = 1, height = 0.33)+
  draw_plot(na, x = 0, y = 0, width = 1, height = 0.33)+
  draw_plot_label(label = c("A","B","C"), size = 18,
                  x = c(0,0,0), y = c(0.99,0.67,0.34))
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/DivGen.png",width = 13, height = 16, dpi = 900)

#Figure S9
MatSuppDivGen <-ggdraw()+
  draw_plot(he, x = 0, y = 0.66, width = 1, height = 0.33)+
  draw_plot(ho, x = 0, y = 0.33, width = 1, height = 0.33)+
  draw_plot(fst, x = 0, y = 0, width = 1, height = 0.33)+
  draw_plot_label(label = c("A","B","C"), size = 18,
                  x = c(0,0,0), y = c(0.99,0.67,0.34))
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Supp Mat/DivGen2.png",width = 13, height = 16, dpi = 900)

#### 3.3 Objectif 3 _ Stocking treatment comparison ____________________________________________________________________________________

#Only taking 2020-2023 crosses (survival available and different incubation treatment)
cross_filtered <- subset(cross, Year %in% 2020:2023 & !is.na(Survival))
cross_filtered$Survival_percent <- cross_filtered$Survival * 100
summary(cross_filtered)

### 3.3.2 Egg to fry survival
summary(cross_filtered)
cross_filtered$Offspring_Surv <- round(cross_filtered$Offspring_Surv)
cross_filtered$Offspring <- round(cross_filtered$Offspring)


mod <- glmer(
  cbind(Offspring_Surv, Offspring - Offspring_Surv) ~ Type * OrigineF + (1 | Year),
  data = cross_filtered,
  family = binomial
)
summary(mod)
# Estimated marginal means on the response (probability) scale
emm <- emmeans(mod, ~ Type | OrigineF, type = "response")

# Show estimated survival by treatment within each origin
emm

# Pairwise contrasts (LARSA vs SSRR) within each origin
contrast(emm, method = "pairwise", by = "OrigineF", adjust = "none")


# Prédictions marginales (sur échelle de probabilité)
emm <- emmeans(mod, ~ Type * OrigineF, type = "response")

# Convertir en data.frame pour ggplot
emm_df <- as.data.frame(emm)

# Rename treatment types
emm_df$Type <- recode(emm_df$Type, "LARSA" = "OPT", "SSRR" = "MPT")


# Define colors for treatment types
type_colors <- c("OPT" = "#492050", "MPT" = "#2C792D")

eggtofry <- ggplot(emm_df, aes(x = OrigineF, y = prob, color = Type, group = Type)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.3), width = 0.2) +
  
  # Use treatment colors for legend
  scale_color_manual(values = type_colors, labels = c("OPT" = "OPT", "MPT" = "NMT")) +
  
  labs(y = "Predicted egg to fry survival probability",
       x = "Population",  # Now x-axis is population
       color = "Treatment") +
  
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"), 
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.position = "bottom"
  ) +
  
  # Brackets: positions need to be adjusted to new x-axis (OrigineF)
  geom_bracket(
    xmin = 0.9, xmax = 1.1,
    y.position = 0.81,
    label = "***",
    inherit.aes = FALSE,
    tip.length = 0.02
  ) +
  geom_bracket(
    xmin = 1.9, xmax = 2.1,
    y.position = 0.81,
    label = "***",
    inherit.aes = FALSE,
    tip.length = 0.02
  )

eggtofry #Figure 6A

### 3.3.3 Treatment survival 

#Import number of individuals stocked
stocked <- read.delim("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/stocked.txt")

stocked[] <- lapply(stocked, function(x) if (is.character(x)) as.factor(x) else x)
stocked$Year <- as.factor(stocked$Year)
summary(stocked)

#Importing dataset of all sampled individuals for all stage

assign <- read.delim("F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Code et fichier/assign.txt")
assign[] <- lapply(assign, function(x) {
  if (is.character(x) || is.numeric(x)) as.factor(x) else x
})
summary(assign)


#Only assigned fish
assigned_fish <- assign %>%
  filter(Assigned=="oui")
summary(assigned_fish)

#Remove cohort < 2018 and > 2023
assigned_fish_filtered <- assigned_fish %>%
  filter(as.numeric(as.character(Stocking_Year)) > 2018)
summary(assigned_fish_filtered)


#Only fry


fry_fish_filtered <- assigned_fish_filtered %>%
  filter(Stage == "Fry")
summary(fry_fish_filtered)


# Step 1: Count survivors in 'assign' by Year, Population, and Type
survival_counts_fry <- fry_fish_filtered  %>%
  group_by(Stocking_Year, Population, Type) %>%
  summarise(survival = n(), .groups = "drop")
survival_counts_fry <- survival_counts_fry %>%
  filter(Stocking_Year!="2022")
survival_counts_fry <- survival_counts_fry %>%
  filter(Stocking_Year!="2019")

stocked_after_2019 <- stocked %>%
  filter(as.numeric(as.character(Year)) > 2018)
stocked_after_2019 <- stocked_after_2019 %>%
  filter(as.numeric(as.character(Year)) < 2024)
# Step 2: Merge '
stocked_fry <- stocked_after_2019 %>%
  filter(as.numeric(as.character(Year))<2024, !Year %in% c(2019,2022))


stocked_fry  <- stocked_fry  %>%
  left_join(survival_counts_fry, by = c("Year" = "Stocking_Year",
                                        "Population" = "Population",
                                        "Type" = "Type"))

# Create the variable for binomial analysis
stocked_fry <- stocked_fry %>%
  mutate(failed = Stocked - survival)
summary(stocked_fry)
#Fit the model

model_fry <- glmer(cbind(survival,failed) ~ Type * Population + (1|Year),
                   data = stocked_fry, family = binomial)
performance::check_overdispersion(model_fry)
summary(model_fry)



#Odds ratio comparison
# Step 1: Get estimated marginal means and pairwise contrasts
emm_fry <- emmeans(model_fry, ~ Type | Population, type = "response")
pairwise_fry <- pairs(emm_fry, type = "response")

# Step 2: Extract odds ratios and p-values
df_or_fry <- as.data.frame(pairwise_fry) %>%
  rename(odds.ratio = odds.ratio) %>%
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    favored = ifelse(odds.ratio > 1, "LARSA favored", "SSRR favored")
  )

# Step 3: Extract confidence intervals from the same source
df_ci_fry <- confint(pairwise_fry) %>%
  as.data.frame() %>%
  rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)

# Step 4: Merge CI info into df_or
df_plot_fry <- df_or_fry %>%
  left_join(df_ci_fry %>% dplyr::select(Population, contrast, lower.CL, upper.CL),
            by = c("Population", "contrast"))


#Only parr

parr_fish_filtered <- assigned_fish_filtered %>%
  filter(Stage == "Parr")
summary(parr_fish_filtered)


# Step 1: Count survivors in 'assign' by Year, Population, and Type
survival_counts_parr <- parr_fish_filtered  %>%
  group_by(Stocking_Year, Population, Type) %>%
  summarise(survival = n(), .groups = "drop")
survival_counts_parr <- survival_counts_parr %>%
  filter(Stocking_Year!="2021")


# Step 2: Merge '
stocked_parr <- stocked_after_2019 %>%
  filter(as.numeric(as.character(Year))<2024, !Year %in% c(2021))


stocked_parr  <- stocked_parr  %>%
  left_join(survival_counts_parr, by = c("Year" = "Stocking_Year",
                                         "Population" = "Population",
                                         "Type" = "Type"))%>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))


# Create the variable for binomial analysis
stocked_parr <- stocked_parr %>%
  mutate(failed = Stocked - survival)

#Fit the model

model_parr <- glmer(cbind(survival,failed) ~ Type * Population + (1|Year),
                    data = stocked_parr, family = binomial)
performance::check_overdispersion(model_parr)
summary(model_parr)


#Odds ratio comparison
# Step 1: Get estimated marginal means and pairwise contrasts
emm_parr <- emmeans(model_parr, ~ Type | Population, type = "response")
pairwise_parr <- pairs(emm_parr, type = "response")

# Step 2: Extract odds ratios and p-values
df_or_parr <- as.data.frame(pairwise_parr) %>%
  rename(odds.ratio = odds.ratio) %>%
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    favored = ifelse(odds.ratio > 1, "LARSA favored", "SSRR favored")
  )

# Step 3: Extract confidence intervals from the same source
df_ci_parr <- confint(pairwise_parr) %>%
  as.data.frame() %>%
  rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)

# Step 4: Merge CI info into df_or
df_plot_parr <- df_or_parr %>%
  left_join(df_ci_parr %>% dplyr::select(Population, contrast, lower.CL, upper.CL),
            by = c("Population", "contrast"))


#Only smolts

smolts_fish_filtered <- assigned_fish_filtered %>%
  filter(Stage == "Smolt")
summary(smolts_fish_filtered)


# Step 1: Count survivors in 'assign' by Year, Population, and Type
survival_counts_smolts <- smolts_fish_filtered  %>%
  group_by(Stocking_Year, Population, Type) %>%
  summarise(survival = n(), .groups = "drop")
#survival_counts_smolts$Type <- recode(survival_counts_smolts$Type,
                              # "LARSA" = "OPT",
                               #"SSRR"  = "MPT")

# Step 2: Merge '
stocked_smolts <- stocked_after_2019 %>%
  filter(as.numeric(as.character(Year))<2023)
#stocked_smolts <- stocked_smolts  %>%
  #dplyr::select(-survival,-failed)

stocked_smolts <- stocked_smolts %>%
  left_join(survival_counts_smolts, by = c("Year" = "Stocking_Year",
                                    "Population" = "Population",
                                    "Type" = "Type"))%>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Create the variable for binomial analysis
stocked_smolts <- stocked_smolts %>%
  mutate(failed = Stocked - survival)

#Fit the model

model_smolt <- glmer(cbind(survival,failed) ~ Type * Population + (1|Year),
               data = stocked_smolts, family = binomial)
performance::check_overdispersion(model_smolt)
summary(model_smolt)


#Odds ratio comparison
# Step 1: Get estimated marginal means and pairwise contrasts
emm_smolt <- emmeans(model_smolt, ~ Type | Population, type = "response")
pairwise_smolt <- pairs(emm_smolt, type = "response")

# Step 2: Extract odds ratios and p-values
df_or_smolt <- as.data.frame(pairwise_smolt) %>%
  rename(odds.ratio = odds.ratio) %>%
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    favored = ifelse(odds.ratio > 1, "LARSA favored", "SSRR favored")
  )

# Step 3: Extract confidence intervals from the same source
df_ci_smolt <- confint(pairwise_smolt) %>%
  as.data.frame() %>%
  rename(lower.CL = asymp.LCL, upper.CL = asymp.UCL)

# Step 4: Merge CI info into df_or
df_plot_smolt <- df_or_smolt %>%
  left_join(df_ci_smolt %>% dplyr::select(Population, contrast, lower.CL, upper.CL),
            by = c("Population", "contrast"))


#Figure 6
# 1️⃣ Add a Stage column to each dataset
df_plot_fry   <- df_plot_fry   %>% mutate(Stage = "Fry")
df_plot_parr  <- df_plot_parr  %>% mutate(Stage = "Parr")
df_plot_smolt <- df_plot_smolt %>% mutate(Stage = "Smolt")

# 2️⃣ Combine all datasets
df_plot_all <- bind_rows(df_plot_fry, df_plot_parr, df_plot_smolt)

# 3️⃣ Plot
treatment <- ggplot(df_plot_all, aes(x = Population, y = odds.ratio, 
                        color = Population, shape = Stage)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_text(aes(y = upper.CL * 1.05, label = signif), 
            size = 5, vjust = 0, position = position_dodge(width = 0.5)) +
  labs(
    y = "Odds ratio of survival (OPT vs NMT treatment)",
    x = "Population",
    shape = "Stage"
  ) +
  scale_color_manual(values = c("Puyjalon" = "#DC143C", "Romaine" = "#000080")) +
  scale_shape_manual(values = c("Fry" = 16, "Parr" = 17, "Smolt" = 15)) + # pch: circle, triangle, square
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, color = "black"), 
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  )+
  guides(color = "none")+
  annotate("text", x = 1.4, y = 2, label = "OPT favored", 
           fontface = "italic", size = 4.5) +
  annotate("text", x = 2.4, y = 1.1, label = "NMT favored", 
           fontface = "italic", size = 4.5)# remove Population legend

treatment

#Figure 6
Treatment <-ggdraw()+
  draw_plot(eggtofry, x = 0, y = 0.5, width = 1, height = 0.5)+
  draw_plot(treatment, x = 0, y = 0, width = 1, height = 0.5)+
  draw_plot_label(label = c("A","B"), size = 18,
                  x = c(0,0), y = c(0.995,0.51))
ggsave(filename = "F:/Thèse/Projet Romaine - Contrat/Chapitres/Chapitre II - Impact des ensemencements/Figure/Treatment.png",width = 10, height = 12, dpi = 900)
