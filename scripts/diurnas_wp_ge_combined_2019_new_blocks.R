#Combine diurnals new blocks 

#Aug20_r1

library(tidyverse)

table1_r1_aug20 <- read.csv("data/2019-08-20-0500_BH_R1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_aug20<- read.csv("data/2019-08-20-0502_logdata_BH_Round1.csv", header= TRUE, sep =",", skip = 15)

table1_r1_aug20_sel <-table1_r1_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =1)
table1_r1_aug20_sel <- table1_r1_aug20_sel [-1, ]

table2_r1_aug20_sel <-table2_r1_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =1)
table2_r1_aug20_sel <- table2_r1_aug20_sel [-1, ]


names(table1_r1_aug20_sel) <- names(table2_r1_aug20_sel )
identical(names(table1_r1_aug20_sel), names (table2_r1_aug20_sel ))

table_comb_r1_aug20 <-rbind(table1_r1_aug20_sel, table2_r1_aug20_sel)

#aug20_r2

table1_r2_aug20 <- read.csv("data/2019-08-20-0804_logdata_BH_Round2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_aug20<- read.csv("data/2019-08-20-0805_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_aug20_sel <-table1_r2_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm , Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =2)
table1_r2_aug20_sel <- table1_r2_aug20_sel [-1, ]

table2_r2_aug20_sel <-table2_r2_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =2)
table2_r2_aug20_sel <- table2_r2_aug20_sel [-1, ]


names(table1_r2_aug20_sel) <- names(table2_r2_aug20_sel )
identical(names(table1_r2_aug20_sel), names (table2_r2_aug20_sel ))

table_comb_r2_aug20 <-rbind(table1_r2_aug20_sel, table2_r2_aug20_sel)

#Aug20_r3

table1_r3_aug20 <- read.csv("data/2019-08-20-1026_BH_R3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_aug20<- read.csv("data/2019-08-20-1027_logdata_BH_Round3.csv", header= TRUE, sep =",", skip = 15)

table1_r3_aug20_sel <-table1_r3_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =3)
table1_r3_aug20_sel <- table1_r3_aug20_sel [-1, ]

table2_r3_aug20_sel <-table2_r3_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =3)
table2_r3_aug20_sel <- table2_r3_aug20_sel [-1, ]


names(table1_r3_aug20_sel) <- names(table2_r3_aug20_sel )
identical(names(table1_r3_aug20_sel), names (table2_r3_aug20_sel ))

table_comb_r3_aug20 <-rbind(table1_r3_aug20_sel, table2_r3_aug20_sel)

#Aug20_r4

table1_r4_aug20 <- read.csv("data/2019-08-20-1232_BH_R4.csv", header = TRUE, sep =",", skip = 13)
table2_r4_aug20<- read.csv("data/2019-08-20-1236_logdata_BH_Round4.csv", header= TRUE, sep =",", skip = 15)

table1_r4_aug20_sel <-table1_r4_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =4)
table1_r4_aug20_sel <- table1_r4_aug20_sel [-1, ]

table2_r4_aug20_sel <-table2_r4_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =4)
table2_r4_aug20_sel <- table2_r4_aug20_sel [-1, ]


names(table1_r4_aug20_sel) <- names(table2_r4_aug20_sel )
identical(names(table1_r4_aug20_sel), names (table2_r4_aug20_sel ))

table_comb_r4_aug20 <-rbind(table1_r4_aug20_sel, table2_r4_aug20_sel)

#Aug20_r5

table1_r5_aug20 <- read.csv("data/2019-08-20-1555_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_aug20<- read.csv("data/2019-08-20-1601_logdata_BH_Round5.csv", header= TRUE, sep =",", skip = 15)

table1_r5_aug20_sel <-table1_r5_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =5)
table1_r5_aug20_sel <- table1_r5_aug20_sel [-1, ]

table2_r5_aug20_sel <-table2_r5_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =5)
table2_r5_aug20_sel <- table2_r5_aug20_sel [-1, ]


names(table1_r5_aug20_sel) <- names(table2_r5_aug20_sel )
identical(names(table1_r5_aug20_sel), names (table2_r5_aug20_sel ))

table_comb_r5_aug20 <-rbind(table1_r5_aug20_sel, table2_r5_aug20_sel)

#Combine all rounds diurnals aug 20

names(table_comb_r1_aug20) <- names(table_comb_r2_aug20) 
names(table_comb_r2_aug20) <- names(table_comb_r3_aug20)
names(table_comb_r3_aug20) <- names(table_comb_r4_aug20)
names(table_comb_r4_aug20) <- names(table_comb_r5_aug20)

identical(names(table_comb_r1_aug20), names (table_comb_r2_aug20))
identical(names(table_comb_r2_aug20), names (table_comb_r3_aug20))
identical(names(table_comb_r3_aug20), names (table_comb_r4_aug20))
identical(names(table_comb_r4_aug20), names (table_comb_r5_aug20))

table_diurnals_aug20 <-rbind(table_comb_r1_aug20, table_comb_r2_aug20, table_comb_r3_aug20, table_comb_r4_aug20, table_comb_r5_aug20)

write.csv(table_diurnals_aug20,"data_output/table_diurnals_aug20")

#Sep5_r1

table1_r1_sep5 <- read.csv("data/2019-09-05-0505_BH_Round1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_sep5<- read.csv("data/2019-09-05-0509_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_sep5_sel <-table1_r1_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =1)
table1_r1_sep5_sel <- table1_r1_sep5_sel [-1, ]

table2_r1_sep5_sel <-table2_r1_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =1) %>%
  filter(!is.na(Fv.Fm))



names(table1_r1_sep5_sel) <- names(table2_r1_sep5_sel )
identical(names(table1_r1_sep5_sel), names (table2_r1_sep5_sel ))

table_comb_r1_sep5 <-rbind(table1_r1_sep5_sel, table2_r1_sep5_sel)

#Sep5_r2

table1_r2_sep5 <- read.csv("data/2019-09-05-0752_BH_Round2 (with all data).csv", header = TRUE, sep =",", skip = 53)
table2_r2_sep5<- read.csv("data/2019-09-05-0750_BH_R2.csv", header= TRUE, sep =",", skip = 53)
table3_r2_sep5<- read.csv("data/2019-09-05-0938_BH_R2_#2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_sep5_sel <-table1_r2_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =2)
table1_r2_sep5_sel <- table1_r2_sep5_sel [-1, ]

table2_r2_sep5_sel <-table2_r2_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =2) %>%
  filter(!is.na(Fv.Fm))

table3_r2_sep5_sel <-table3_r2_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =2) %>%
  filter(!is.na(Fv.Fm))


names(table1_r2_sep5_sel) <- names(table2_r2_sep5_sel)
names(table2_r2_sep5_sel) <- names(table3_r2_sep5_sel)
identical(names(table1_r2_sep5_sel), names (table2_r2_sep5_sel))
identical(names(table2_r2_sep5_sel), names (table3_r2_sep5_sel))

table_comb_r2_sep5 <-rbind(table1_r2_sep5_sel, table2_r2_sep5_sel, table3_r2_sep5_sel)

#Sep5_r3

table1_r3_sep5 <- read.csv("data/2019-09-05-1028_BH_R3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_sep5<- read.csv("data/2019-09-05-1035_BH_Round3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_sep5_sel <-table1_r3_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =3)
table1_r3_sep5_sel <- table1_r3_sep5_sel [-1, ]

table2_r3_sep5_sel <-table2_r3_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =3) %>%
  filter(!is.na(Fv.Fm))

names(table1_r3_sep5_sel) <- names(table2_r3_sep5_sel )
identical(names(table1_r1_sep5_sel), names (table2_r1_sep5_sel ))


table_comb_r3_sep5 <-rbind(table1_r3_sep5_sel, table2_r3_sep5_sel)

#Sep5_r4

table1_r4_sep5 <- read.csv("data/2019-09-05-1247_BH_R4.csv", header = TRUE, sep =",", skip = 13)
table2_r4_sep5<- read.csv("data/2019-09-05-1250_BH_Round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_sep5_sel <-table1_r4_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =4)
table1_r4_sep5_sel <- table1_r4_sep5_sel [-1, ]

table2_r4_sep5_sel <-table2_r4_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =4)
  table2_r4_sep5_sel <- table2_r4_sep5_sel [-1, ]

names(table1_r4_sep5_sel) <- names(table2_r4_sep5_sel )
identical(names(table1_r4_sep5_sel), names (table2_r4_sep5_sel ))


table_comb_r4_sep5 <-rbind(table1_r4_sep5_sel, table2_r4_sep5_sel)

#Sep5_r5

table1_r5_sep5 <- read.csv("data/2019-09-05-1549_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_sep5<- read.csv("data/2019-09-05-1552_BH_Round5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_sep5_sel <-table1_r5_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =5)
table1_r5_sep5_sel <- table1_r5_sep5_sel [-1, ]

table2_r5_sep5_sel <-table2_r5_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =5)
table2_r5_sep5_sel <- table2_r5_sep5_sel [-1, ]

names(table1_r5_sep5_sel) <- names(table2_r5_sep5_sel )
identical(names(table1_r5_sep5_sel), names (table2_r5_sep5_sel ))


table_comb_r5_sep5 <-rbind(table1_r5_sep5_sel, table2_r5_sep5_sel)


#Combine all rounds diurnals sep 5

names(table_comb_r1_sep5) <- names(table_comb_r2_sep5)
names(table_comb_r2_sep5) <- names(table_comb_r3_sep5)
names(table_comb_r3_sep5) <- names(table_comb_r4_sep5)
names(table_comb_r4_sep5) <- names(table_comb_r5_sep5)

identical(names(table_comb_r1_sep5), names (table_comb_r2_sep5))
identical(names(table_comb_r2_sep5), names (table_comb_r3_sep5))
identical(names(table_comb_r3_sep5), names (table_comb_r4_sep5))
identical(names(table_comb_r4_sep5), names (table_comb_r5_sep5))

names(table_comb_r1_sep5) <- names(table_comb_r2_sep5)
names(table_comb_r2_sep5) <- names(table_comb_r3_sep5)
names(table_comb_r3_sep5) <- names(table_comb_r4_sep5)
names(table_comb_r4_sep5) <- names(table_comb_r5_sep5)

identical(names(table_comb_r1_sep5), names (table_comb_r2_sep5))
identical(names(table_comb_r2_sep5), names (table_comb_r3_sep5))
identical(names(table_comb_r3_sep5), names (table_comb_r4_sep5))
identical(names(table_comb_r4_sep5), names (table_comb_r5_sep5))

table_diurnals_sep5 <-rbind(table_comb_r1_sep5, table_comb_r2_sep5, table_comb_r3_sep5, table_comb_r4_sep5, table_comb_r5_sep5)

write.csv(table_diurnals_sep5,"data_output/table_diurnals_sep5")

#Combine all diurnals new blocks

diurnals_Jul25 <-read.csv("data_output/table_diurnals_jul25.csv", header = TRUE, sep = ",")

diurnals_Jul28 <-read.csv("data_output/table_diurnal_jul28", header = TRUE, sep = ",")

diurnals_aug1 <-read.csv("data_output/table_diurnals_aug1", header = TRUE, sep = ",")

diurnals_aug15 <-read.csv("data_output/table_diurnals_aug15", header = TRUE, sep = ",")

diurnals_aug20 <-read.csv("data_output/table_diurnals_aug20", header = TRUE, sep = ",")

diurnals_sep5 <-read.csv("data_output/table_diurnals_sep5", header = TRUE, sep = ",")


#Repeat a few times 

names(diurnals_Jul25) <- names(diurnals_Jul28) 
names(diurnals_Jul28) <- names(diurnals_aug1)
names(diurnals_aug1) <- names(diurnals_aug15)
names(diurnals_aug15) <- names(diurnals_aug20)
names(diurnals_aug20) <- names(diurnals_sep5)

identical(names(diurnals_Jul25), names (diurnals_Jul28))
identical(names(diurnals_Jul28), names (diurnals_aug1))
identical(names(diurnals_aug1), names (diurnals_aug15))
identical(names(diurnals_aug15), names (diurnals_aug20))
identical(names(diurnals_aug20), names (diurnals_sep5))




diurnals_after_blockid_change <- rbind(diurnals_Jul25, diurnals_Jul28, diurnals_aug1, diurnals_aug15, diurnals_aug20, diurnals_sep5)


diurnals_after_blockid_change <- diurnals_after_blockid_change%>%
  mutate(pixel_number = case_when(
    BH_Block == "B1_R2" ~ 34,
    BH_Block == "B1_R3" ~ 55,
    BH_Block == "B1_R4" ~ 67, 
    BH_Block == "B2_R1" ~ 23,
    BH_Block == "B2_R2" ~ 35,
    BH_Block == "B2_R3" ~ 48,
    BH_Block == "B3_R1" ~ 27,
    BH_Block == "B3_R2" ~ 45,
    BH_Block == "B3_R3" ~ 54
  )) %>%
  mutate( treatment= case_when(
    BH_Block == "B1_R2" ~ 1,
    BH_Block == "B1_R3" ~ 1,
    BH_Block == "B1_R4" ~ 1, 
    BH_Block == "B2_R1" ~ 2,
    BH_Block == "B2_R2" ~ 2,
    BH_Block == "B2_R3" ~ 2,
    BH_Block == "B3_R1" ~ 3,
    BH_Block == "B3_R2" ~ 3,
    BH_Block == "B3_R3" ~ 3))%>%
  mutate(Rep= case_when(
    BH_Block == "B1_R2" ~ 2,
    BH_Block == "B1_R3" ~ 3,
    BH_Block == "B1_R4" ~ 1, 
    BH_Block == "B2_R1" ~ 1,
    BH_Block == "B2_R2" ~ 2,
    BH_Block == "B2_R3" ~ 3,
    BH_Block == "B3_R1" ~ 1,
    BH_Block == "B3_R2" ~ 2,
    BH_Block == "B3_R3" ~ 3))


write.csv(diurnals_after_blockid_change,"data_output/diurnals_after_blockid_change.csv")


# First trial to merge water potentials and LI-COR dataframes

water_potentials_after_blockid_change <- read.csv("data/Water_potentials_2019.csv", header = TRUE, sep = ",")

diurnals_after_blockid_change_w_pixels <- read.csv("data_output/diurnals_after_blockid_change.csv", header = TRUE, sep =",")

common_col_names <- intersect(names(diurnals_after_blockid_change_w_pixels), names(water_potentials_after_blockid_change))

diurnals_wp_ge_data_new_blocks_2019 <- merge(diurnals_after_blockid_change_w_pixels, water_potentials_after_blockid_change, by=common_col_names, all =TRUE)

write.csv(diurnals_wp_ge_data_new_blocks_2019, "data_output/diurnals_wp_ge_data_new_blocks_2019")

diurnals_wp_ge_data_new_blocks_2019_final <-diurnals_wp_ge_data_new_blocks_2019 %>%
  select(date, year, day, hhmmss, pixel_number, BH_Block, BH_Vine, BH_Leaf, round, E, A, Ci, gsw, Fv.Fm,Tleaf, Tair, VPDleaf, RHcham, Leaf_wp_bar, leaf_temp_C, Stem_wp_bar, Leak, wrong_vine, treatment, Rep )

write.csv(diurnals_wp_ge_data_new_blocks_2019_final,"data_output/diurnals_wp_ge_data_new_blocks_2019_final.csv")

str(diurnals_wp_ge_data_new_blocks_2019_final$hhmmss)

#Combine gas exchange and water potential data frames old blocks

water_potentials_before_blockid_change <- read.csv("data/Water_potentials_2019_old_blocks.csv", header = TRUE, sep = ",")

diurnals_before_blockid_change_w_pixels <- read.csv("data_output/diurnals_before_blockid_change.csv", header = TRUE, sep =",")

common_col_names <- intersect(names(diurnals_before_blockid_change_w_pixels), names(water_potentials_before_blockid_change))

diurnals_wp_ge_data_old_blocks_2019 <- merge(diurnals_before_blockid_change_w_pixels, water_potentials_before_blockid_change, by=common_col_names, all =TRUE)

write.csv(diurnals_wp_ge_data_old_blocks_2019, "data_output/diurnals_wp_ge_data_old_blocks_2019")


diurnals_wp_ge_data_old_blocks_2019_final <-diurnals_wp_ge_data_old_blocks_2019 %>%
  select(date, year, day, hhmmss, pixel_number, BH_Block, BH_Vine, BH_Leaf, round, E, A, Ci, gsw, Fv.Fm,Tleaf, Tair, VPDleaf, RHcham, Leaf_wp_bar, Stem_wp_bar, Leak, wrong_vine,treatment, Rep)



write.csv(diurnals_wp_ge_data_old_blocks_2019_final,"data_output/diurnals_wp_ge_data_old_blocks_2019_final.csv")

str(diurnals_wp_ge_data_old_blocks_2019$hhmmss)

#Merge old blocks and new blocks diurnals 

diurnals_wp_ge_data_old_blocks_2019_to_combine <- read.csv("data_output/diurnals_wp_ge_data_old_blocks_2019_final.csv", header = TRUE, sep = ",")

diurnals_wp_ge_data_new_blocks_2019_to_combine <- read.csv("data_output/diurnals_wp_ge_data_new_blocks_2019_final.csv", header = TRUE, sep =",")

common_col_names <- intersect(names(diurnals_wp_ge_data_old_blocks_2019_to_combine ), names(diurnals_wp_ge_data_new_blocks_2019_to_combine ))

diurnals_2019_old_and_new_blocks<- merge(diurnals_wp_ge_data_old_blocks_2019_to_combine , diurnals_wp_ge_data_new_blocks_2019_to_combine , by=common_col_names, all =TRUE)

write.csv(diurnals_2019_old_and_new_blocks, "data_output/diurnals_2019_old_and_new_blocks.csv")

library(tidyverse)
diurnals_2019_old_and_new_blocks <-read.csv("data_output/diurnals_2019_old_and_new_blocks.csv", header = TRUE)

diurnals_2019_old_and_new_blocks_cleaned <- diurnals_2019_old_and_new_blocks%>%
  filter(!is.na(A))

write.csv(diurnals_2019_old_and_new_blocks_cleaned, "data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv")




