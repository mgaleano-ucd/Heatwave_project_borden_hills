#combinig diurnals and water potentials data

library(tidyverse)

#jUL1_R1

table1_r1_jul2 <- read.csv("data/2019-07-02-0607_logdata_BH445_run1 missing.csv", header = TRUE, sep =",", skip = 54)
table2_r1_jul2<- read.csv("data/2019-07-02-0608_logdata_BH1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_jul2_sel <-table1_r1_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =1)
table1_r1_jul2_sel <- table1_r1_jul2_sel [-1, ]

table2_r1_jul2_sel <-table2_r1_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =1)
table2_r1_jul2_sel <- table2_r1_jul2_sel [-1, ]

names(table1_r1_jul2_sel) <- names(table2_r1_jul2_sel ) 
identical(names(table1_r1_jul2_sel), names (table2_r1_jul2_sel ))
          
table_comb__r1_Jul2 <- rbind (table1_r1_jul2_sel, table2_r1_jul2_sel)

#jUL1_R2

table1_r2_jul2 <- read.csv("data/2019-07-02-0942_BH455_round2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_jul2<- read.csv("data/2019-07-02-0942_BH455_round2_0.csv", header= TRUE, sep =",", skip = 15)
table3_r2_jul2<- read.csv("data/2019-07-02-1007_BH445_round2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul2_sel <-table1_r2_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =2)
table1_r2_jul2_sel <- table1_r2_jul2_sel [-1, ]

table2_r2_jul2_sel <-table2_r2_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =2)
table2_r2_jul2_sel <- table2_r2_jul2_sel [-1, ]

table3_r2_jul2_sel <-table3_r2_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =2)
table3_r2_jul2_sel <- table3_r2_jul2_sel [-1, ]

names(table1_r2_jul2_sel) <- names(table2_r2_jul2_sel ) 
identical(names(table1_r2_jul2_sel), names (table2_r2_jul2_sel ))
names(table2_r2_jul2_sel) <- names(table3_r2_jul2_sel )
identical(names(table2_r2_jul2_sel), names (table3_r2_jul2_sel ))

table_comb_r2_Jul2 <- rbind (table1_r2_jul2_sel, table2_r2_jul2_sel, table3_r2_jul2_sel)

#jUL1_R3

table1_r3_jul2 <- read.csv("data/2019-07-02-1338_BH455_diurnals_round3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_jul2<- read.csv("data/2019-07-02-1339_BH455_round3.csv", header= TRUE, sep =",", skip = 15)

table1_r3_jul2_sel <-table1_r3_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =3)
table1_r3_jul2_sel <- table1_r3_jul2_sel [-1, ]

table2_r3_jul2_sel <-table2_r3_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =3)
table2_r3_jul2_sel <- table2_r3_jul2_sel [-1, ]


names(table1_r3_jul2_sel) <- names(table2_r3_jul2_sel ) 
identical(names(table1_r3_jul2_sel), names (table2_r3_jul2_sel ))

table_comb_r3_Jul2 <-rbind(table2_r3_jul2_sel, table1_r3_jul2_sel)

#jUL1_R4


table1_r4_jul2 <- read.csv("data/2019-07-02-1617_BH445_round4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_jul2<- read.csv("data/2019-07-02-1625_BH455_round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul2_sel <-table1_r4_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =4)
table1_r4_jul2_sel <- table1_r4_jul2_sel [-1, ]

table2_r4_jul2_sel <-table2_r4_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =4)
table2_r4_jul2_sel <- table2_r4_jul2_sel [-1, ]


names(table1_r4_jul2_sel) <- names(table2_r4_jul2_sel ) 
identical(names(table1_r4_jul2_sel), names (table2_r4_jul2_sel ))

table_comb_r4_Jul2 <-rbind(table1_r4_jul2_sel, table2_r4_jul2_sel)


#jUL1_R5

table1_r5_jul2 <- read.csv("data/2019-07-02-1845_BH445_round5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_jul2<- read.csv("data/2019-07-02-1855_logdata_BH455_Round5.csv", header= TRUE, sep =",", skip = 13)
table3_r5_jul2<- read.csv("data/2019-07-02-1855_logdata_BH455_Round5_0.csv", header= TRUE, sep =",", skip = 13)

table1_r5_jul2_sel <-table1_r5_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =5)
table1_r5_jul2_sel <- table1_r5_jul2_sel [-1, ]

table2_r5_jul2_sel <-table2_r5_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =5)
table2_r5_jul2_sel <- table2_r5_jul2_sel [-1, ]

table3_r5_jul2_sel <-table3_r5_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =5)
table3_r5_jul2_sel <- table3_r5_jul2_sel [-1, ]

names(table1_r5_jul2_sel) <- names(table2_r5_jul2_sel ) 
names(table2_r5_jul2_sel) <- names(table3_r5_jul2_sel )
identical(names(table1_r5_jul2_sel), names (table2_r5_jul2_sel ))
identical(names(table2_r5_jul2_sel), names (table3_r5_jul2_sel ))

table_comb_r5_Jul2 <-rbind(table1_r5_jul2_sel, table2_r5_jul2_sel, table3_r5_jul2_sel)

#Combining all rounds fro diurnal jul 1

names(table_comb__r1_Jul2) <- names(table_comb_r2_Jul2)
names(table_comb_r2_Jul2) <- names(table_comb_r3_Jul2)
names(table_comb_r3_Jul2) <- names(table_comb_r4_Jul2)
names(table_comb_r4_Jul2) <- names(table_comb_r5_Jul2)

identical(names(table_comb__r1_Jul2), names (table_comb_r2_Jul2))
identical(names(table_comb_r2_Jul2), names (table_comb_r3_Jul2))
identical(names(table_comb_r3_Jul2), names (table_comb_r4_Jul2))
identical(names(table_comb_r4_Jul2), names (table_comb_r5_Jul2))

table_diurnals_Jul2 <-rbind(table_comb__r1_Jul2, table_comb_r2_Jul2, table_comb_r3_Jul2, table_comb_r4_Jul2, table_comb_r5_Jul2)

write.csv(table_diurnals_Jul2, "data_output/table_diurnals_Jul1")

#jUL5_Midday

library(tidyverse)

table1_midday_jul5 <- read.csv("data/2019-07-05-1200_BH_midday.csv", header = TRUE, sep =",", skip = 13)

table1_midday_jul5_sel <-table1_midday_jul5 %>%
  select (date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-05-2019", day = 186, year =2019, round = "midday")
table1_midday_jul5_sel  <- table1_midday_jul5_sel  [-1, ]

write.csv(table1_midday_jul5_sel,"data_output/table_midday_jul5")

#jUL12_r1 

table1_r1_jul12 <- read.csv("data/2019-07-12-0536_logdata_BH_R1.csv", header = TRUE, sep =",", skip = 15)
table2_r1_jul12<- read.csv("data/2019-07-12-0551_logdata_BH_Round1.csv", header= TRUE, sep =",", skip = 53)

table1_r1_jul12_sel <-table1_r1_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =1)
table1_r1_jul12_sel <- table1_r1_jul12_sel [-1, ]

table2_r1_jul12_sel <-table2_r1_jul12 %>%
  select(date, hhmmss,BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =1)
table2_r1_jul12_sel <- table2_r1_jul12_sel [-1, ]


names(table1_r1_jul12_sel) <- names(table2_r1_jul12_sel ) 
identical(names(table1_r1_jul12_sel), names (table2_r1_jul12_sel ))

table_comb_r1_Jul12 <-rbind(table1_r1_jul12_sel, table2_r1_jul12_sel)

#Jul12_r2 

table1_r2_jul12 <- read.csv("data/2019-07-12-0758_logdata_BH_R2 missing.csv", header = TRUE, sep =",", skip = 54)
table2_r2_jul12<- read.csv("data/2019-07-12-0804_logdata_BH_Round2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul12_sel <-table1_r2_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =2)
table1_r2_jul12_sel <- table1_r2_jul12_sel [-1, ]

table2_r2_jul12_sel <-table2_r2_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =2)
table2_r2_jul12_sel <- table2_r2_jul12_sel [-1, ]


names(table1_r2_jul12_sel) <- names(table2_r2_jul12_sel ) 
identical(names(table1_r2_jul12_sel), names (table2_r2_jul12_sel ))

table_comb_r2_Jul12 <-rbind(table1_r2_jul12_sel, table2_r2_jul12_sel)

#jul12_r3

table1_r3_jul12 <- read.csv("data/2019-07-12-1046_logdata_BH_R3 missing.csv", header = TRUE, sep =",", skip = 54)
table2_r3_jul12<- read.csv("data/2019-07-12-1053_BH_R3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_jul12_sel <-table1_r3_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =3)
table1_r3_jul12_sel <- table1_r3_jul12_sel [-1, ]

table2_r3_jul12_sel <-table2_r3_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =3)
table2_r3_jul12_sel <- table2_r3_jul12_sel [-1, ]


names(table1_r3_jul12_sel) <- names(table2_r3_jul12_sel ) 
identical(names(table1_r3_jul12_sel), names (table2_r3_jul12_sel ))

table_comb_r3_Jul12 <-rbind(table1_r3_jul12_sel, table2_r3_jul12_sel)

#jul12_r4

table1_r4_jul12 <- read.csv("data/2019-07-12-1318_logdata_BH_R4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_jul12<- read.csv("data/2019-07-12-1812_logdata_BH_Round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul12_sel <-table1_r4_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =4)
table1_r4_jul12_sel <- table1_r4_jul12_sel [-1, ]

table2_r4_jul12_sel <-table2_r4_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =4)
table2_r4_jul12_sel <- table2_r4_jul12_sel [-1, ]


names(table1_r4_jul12_sel) <- names(table2_r4_jul12_sel ) 
identical(names(table1_r4_jul12_sel), names (table2_r4_jul12_sel ))

table_comb_r4_Jul12 <-rbind(table1_r4_jul12_sel, table2_r4_jul12_sel)

#jul12_r5

table1_r5_jul12 <- read.csv("data/2019-07-12-1618_logdata_BH_R5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_jul12<- read.csv("data/2019-07-12-2107_BH_R5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_jul12_sel <-table1_r5_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =5)
table1_r5_jul12_sel <- table1_r5_jul12_sel [-1, ]

table2_r5_jul12_sel <-table2_r5_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =5)
table2_r5_jul12_sel <- table2_r5_jul12_sel [-1, ]

names(table1_r5_jul12_sel) <- names(table2_r5_jul12_sel ) 
identical(names(table1_r5_jul12_sel), names (table2_r5_jul12_sel ))

table_comb_r5_Jul12 <-rbind(table1_r5_jul12_sel, table2_r5_jul12_sel)

#Jul12_r6

table1_r6_jul12 <- read.csv("data/2019-07-12-1840_logdata_BH_R6.csv", header = TRUE, sep =",", skip = 15)
table2_r6_jul12<- read.csv("data/2019-07-12-2349_logdata_BH_Round6-last log low CO2.csv", header= TRUE, sep =",", skip = 13)

table1_r6_jul12_sel <-table1_r6_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =6)
table1_r6_jul12_sel <- table1_r6_jul12_sel [-1, ]

table2_r6_jul12_sel <-table2_r6_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =6)
table2_r6_jul12_sel <- table2_r6_jul12_sel [-1, ]

names(table1_r6_jul12_sel) <- names(table2_r6_jul12_sel ) 
identical(names(table1_r6_jul12_sel), names (table2_r6_jul12_sel ))

table_comb_r6_Jul12 <-rbind(table1_r6_jul12_sel, table2_r6_jul12_sel)

#Combine all rounds diurnal jul 12

names(table_comb_r1_Jul12) <- names(table_comb_r2_Jul12) 
names(table_comb_r2_Jul12) <- names(table_comb_r3_Jul12)
names(table_comb_r3_Jul12) <- names(table_comb_r4_Jul12)
names(table_comb_r4_Jul12) <- names(table_comb_r5_Jul12)
names(table_comb_r5_Jul12) <- names(table_comb_r6_Jul12)

identical(names(table_comb_r1_Jul12), names (table_comb_r2_Jul12))
identical(names(table_comb_r2_Jul12), names (table_comb_r3_Jul12))
identical(names(table_comb_r3_Jul12), names (table_comb_r4_Jul12))
identical(names(table_comb_r4_Jul12), names (table_comb_r5_Jul12))
identical(names(table_comb_r5_Jul12), names (table_comb_r6_Jul12))

table_diurnals_Jul12 <-rbind(table_comb_r1_Jul12, table_comb_r2_Jul12, table_comb_r3_Jul12, table_comb_r4_Jul12, table_comb_r5_Jul12, table_comb_r6_Jul12)

write.csv(table_diurnals_Jul12, "data_output/table_diurnal_jul12")


# Diurnals before change in block's ID

diurnals_jul1 <-read.csv("data_output/table_diurnals_Jul1", header = TRUE, sep = ",")

Midday_jul5 <- read.csv("data_output/table_midday_jul5", header = TRUE, sep = ",")

diurnals_jul12 <- read.csv("data_output/table_diurnal_jul12", header = TRUE, sep = ",")

names(diurnals_jul1) <- names(Midday_jul5) 
names(Midday_jul5) <- names(diurnals_jul12)

identical(names(diurnals_jul1), names(Midday_jul5))
identical(names(Midday_jul5), names(diurnals_jul12))

names(diurnals_jul1) <- names(Midday_jul5) 
names(Midday_jul5) <- names(diurnals_jul12)

identical(names(diurnals_jul1), names(Midday_jul5))
identical(names(Midday_jul5), names(diurnals_jul12))

diurnals_before_blockid_change <- rbind(diurnals_jul1, Midday_jul5, diurnals_jul12)

diurnals_before_blockid_change <- diurnals_before_blockid_change%>%
  mutate(pixel_number = case_when(
    BH_Block == "B1_R2" ~ 45,
    BH_Block == "B1_R3" ~ 55,
    BH_Block == "B1_R1" ~ 35, 
    BH_Block == "B2_R1" ~ 13,
    BH_Block == "B2_R2" ~ 27,
    BH_Block == "B2_R3" ~ 75,
    BH_Block == "B3_R1" ~ 23,
    BH_Block == "B3_R2" ~ 78,
    BH_Block == "B3_R3" ~ 48,
  ))%>%
  mutate( treatment= case_when(
    pixel_number == 55 ~ 1,
    pixel_number == 13 ~ 1,
    pixel_number == 48 ~ 2,
    pixel_number == 35 ~ 2,
    pixel_number == 23 ~ 2,
    pixel_number == 78 ~ 2,
    pixel_number == 45 ~ 3,
    pixel_number == 27 ~ 3,
    pixel_number == 75 ~ 3,))%>%
  mutate(Rep= case_when(
    pixel_number == 55 ~ 1,
    pixel_number == 13 ~ 2,
    pixel_number == 48 ~ 1,
    pixel_number == 35 ~ 2,
    pixel_number == 23 ~ 3,
    pixel_number == 78 ~ 4,
    pixel_number == 45 ~ 1,
    pixel_number == 27 ~ 2,
    pixel_number == 75 ~ 3,))



write.csv(diurnals_before_blockid_change, "data_output/diurnals_before_blockid_change.csv")
