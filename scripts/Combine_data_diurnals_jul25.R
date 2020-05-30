

# Combining with csv diurnals Jul 25


#Jul_25 R1

table1_r1_jul25 <- read.csv("data/2019-07-25-0431_BH_R1.csv", header = TRUE, sep =",", skip = 15)
table2_r1_jul25<- read.csv("data/2019-07-25-0927_BH_R1.csv", header= TRUE, sep =",", skip = 13)
table3_r1_jul25<- read.csv("data/2019-07-25-0426_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_jul25_sel <-table1_r1_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =1)
table1_r1_jul25_sel <- table1_r1_jul25_sel [-1, ]

table2_r1_jul25_sel <-table2_r1_jul25 %>%
    select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =1)
table2_r1_jul25_sel <- table2_r1_jul25_sel [-1, ]

table3_r1_jul25_sel <-table3_r1_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =1)
table3_r1_jul25_sel <- table3_r1_jul25_sel [-1, ]

names(table1_r1_jul25_sel ) <- names(table2_r1_jul25_sel)
identical(names(table1_r1_jul25_sel), names (table2_r1_jul25_sel ))
names(table2_r1_jul25_sel ) <- names(table3_r1_jul25_sel)
identical(names(table2_r1_jul25_sel), names (table3_r1_jul25_sel ))

table_comb_r1_jul25<-rbind(table1_r1_jul25_sel, table2_r1_jul25_sel, table3_r1_jul25_sel)
table_comb_r1_jul25 <- table_comb_r1_jul25 %>%
  filter(!is.na (Fv.Fm))

#Jul25_r2

table1_r2_jul25 <- read.csv("data/2019-07-25-0743_BH_R2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_jul25<- read.csv("data/2019-07-25-0751_BH_R2.csv", header= TRUE, sep =",", skip = 13)
table3_r2_jul25<- read.csv("data/2019-07-25-1257_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul25_sel <-table1_r2_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =2)
table1_r2_jul25_sel <- table1_r2_jul25_sel [-1, ]

table2_r2_jul25_sel <-table2_r2_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =2)
table2_r2_jul25_sel <- table2_r2_jul25_sel [-1, ]

table3_r2_jul25_sel <-table3_r2_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =2)
table3_r2_jul25_sel <- table3_r2_jul25_sel [-1, ]

names(table1_r2_jul25_sel ) <- names(table2_r2_jul25_sel)
identical(names(table1_r2_jul25_sel), names (table2_r2_jul25_sel ))
names(table2_r2_jul25_sel ) <- names(table3_r2_jul25_sel)
identical(names(table2_r2_jul25_sel), names (table3_r2_jul25_sel ))

table_comb_r2_jul25<-rbind(table1_r2_jul25_sel, table2_r2_jul25_sel, table3_r2_jul25_sel)
table_comb_r2_jul25 <- table_comb_r2_jul25 %>%
  filter(!is.na (Fv.Fm))

#Jul25_r3

table1_r3_jul25 <- read.csv("data/2019-07-25-1007_BH_R3_andrew.csv", header = TRUE, sep =",", skip = 15)
table2_r3_jul25<- read.csv("data/2019-07-25-1007_BH_R3.csv", header= TRUE, sep =",", skip = 13)
table3_r3_jul25<- read.csv("data/2019-07-25-1008_BH_R3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_jul25_sel <-table1_r3_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =3)
table1_r3_jul25_sel <- table1_r3_jul25_sel [-1, ]

table2_r3_jul25_sel <-table2_r3_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =3)
table2_r3_jul25_sel <- table2_r3_jul25_sel [-1, ]

table3_r3_jul25_sel <-table3_r3_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =3)
table3_r2_jul25_sel <- table3_r2_jul25_sel [-1, ]

names(table1_r3_jul25_sel ) <- names(table2_r3_jul25_sel)
identical(names(table1_r3_jul25_sel), names (table2_r3_jul25_sel ))
names(table2_r3_jul25_sel ) <- names(table3_r3_jul25_sel)
identical(names(table2_r3_jul25_sel), names (table3_r3_jul25_sel ))

table_comb_r3_jul25<-rbind(table1_r3_jul25_sel, table2_r3_jul25_sel, table3_r3_jul25_sel)
table_comb_r3_jul25 <- table_comb_r3_jul25 %>%
  filter(!is.na (Fv.Fm))


#Jul25_r4

table1_r4_jul25 <- read.csv("data/2019-07-25-1216_logdata_BH_R4.csv", header = TRUE, sep =",", skip = 53)
table2_r4_jul25<- read.csv("data/2019-07-25-1203_BH_R4.csv", header= TRUE, sep =",", skip = 15)
table3_r4_jul25<- read.csv("data/2019-07-25-1212_BH_R4.csv", header= TRUE, sep =",", skip = 13)
table4_r4_jul25<- read.csv("data/2019-07-25-1247_logdata_BH_R4.2.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul25_sel <-table1_r4_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table1_r4_jul25_sel <- table1_r4_jul25_sel [-1, ]

table2_r4_jul25_sel <-table2_r4_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table2_r4_jul25_sel <- table2_r4_jul25_sel [-1, ]

table3_r4_jul25_sel <-table3_r4_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table3_r4_jul25_sel <- table3_r4_jul25_sel [-1, ]

table4_r4_jul25_sel <-table4_r4_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table4_r4_jul25_sel <- table4_r4_jul25_sel [-1, ]

names(table1_r4_jul25_sel ) <- names(table2_r4_jul25_sel)
identical(names(table1_r4_jul25_sel), names (table2_r4_jul25_sel ))
names(table2_r4_jul25_sel ) <- names(table3_r4_jul25_sel)
identical(names(table2_r4_jul25_sel), names (table3_r4_jul25_sel ))
names(table3_r4_jul25_sel ) <- names(table4_r4_jul25_sel)
identical(names(table3_r4_jul25_sel), names (table4_r4_jul25_sel ))


names(table1_r4_jul25_sel ) <- names(table2_r4_jul25_sel)
identical(names(table1_r4_jul25_sel), names (table2_r4_jul25_sel ))
names(table2_r4_jul25_sel ) <- names(table3_r4_jul25_sel)
identical(names(table2_r4_jul25_sel), names (table3_r4_jul25_sel ))
names(table3_r4_jul25_sel ) <- names(table4_r4_jul25_sel)
identical(names(table3_r4_jul25_sel), names (table4_r4_jul25_sel ))

table_comb_r4_jul25<-rbind(table1_r4_jul25_sel, table2_r4_jul25_sel, table3_r4_jul25_sel, table4_r4_jul25_sel )

table_comb_r4_jul25 <- table_comb_r4_jul25 %>%
  filter(!is.na (Fv.Fm))

#Jul25_r5

table1_r5_jul25 <- read.csv("data/2019-07-25-1603_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_jul25<- read.csv("data/2019-07-25-1604_BH_R5.csv", header= TRUE, sep =",", skip = 13)
table3_r5_jul25<- read.csv("data/2019-07-25-1604_logdata_BH_R5.csv", header= TRUE, sep =",", skip = 15)

table1_r5_jul25_sel <-table1_r5_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =5)
table1_r5_jul25_sel <- table1_r5_jul25_sel [-1, ]

table2_r5_jul25_sel <-table2_r5_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =5)
table2_r5_jul25_sel <- table2_r5_jul25_sel [-1, ]

table3_r5_jul25_sel <-table3_r5_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =5)
table3_r5_jul25_sel <- table3_r5_jul25_sel [-1, ]

names(table1_r5_jul25_sel ) <- names(table2_r5_jul25_sel)
identical(names(table1_r5_jul25_sel), names (table2_r5_jul25_sel ))
names(table2_r5_jul25_sel ) <- names(table3_r5_jul25_sel)
identical(names(table2_r5_jul25_sel), names (table3_r5_jul25_sel ))

names(table1_r5_jul25_sel ) <- names(table2_r5_jul25_sel)
identical(names(table1_r5_jul25_sel), names (table2_r5_jul25_sel ))
names(table2_r5_jul25_sel ) <- names(table3_r5_jul25_sel)
identical(names(table2_r5_jul25_sel), names (table3_r5_jul25_sel ))

table_comb_r5_jul25<-rbind(table1_r5_jul25_sel, table2_r5_jul25_sel, table3_r5_jul25_sel)
table_comb_r5_jul25 <- table_comb_r5_jul25 %>%
  filter(!is.na (Fv.Fm))

#All round combined diurnals Jul 25

names(table_comb_r1_jul25) <- names(table_comb_r2_jul25) 
names(table_comb_r2_jul25) <- names(table_comb_r3_jul25)
names(table_comb_r3_jul25) <- names(table_comb_r4_jul25)
names(table_comb_r4_jul25) <- names(table_comb_r5_jul25)


identical(names(table_comb_r1_jul25), names (table_comb_r2_jul25))
identical(names(table_comb_r2_jul25), names (table_comb_r3_jul25))
identical(names(table_comb_r3_jul25), names (table_comb_r4_jul25))
identical(names(table_comb_r4_jul25), names (table_comb_r5_jul25))


table_diurnals_jul25 <-rbind(table_comb_r1_jul25, table_comb_r2_jul25, table_comb_r3_jul25, table_comb_r4_jul25, table_comb_r5_jul25)

write.csv(table_diurnals_jul25, "data_output/table_diurnals_jul25")
