#Combine diurnals new blocks 

#jul28_r1

library(tidyverse)

table1_r1_jul28 <- read.csv("data/2019-07-28-0451_BH_R1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_jul28<- read.csv("data/2019-07-28-0514_BH_R1.csv", header= TRUE, sep =",", skip = 15)

table1_r1_jul28_sel <-table1_r1_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm,Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =1)
table1_r1_jul28_sel <- table1_r1_jul28_sel [-1, ]

table2_r1_jul28_sel <-table2_r1_jul28 %>%
  select(date, hhmmss, BH445_LEAF , BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =1)
table2_r1_jul28_sel <- table2_r1_jul28_sel [-1, ]


names(table1_r1_jul28_sel) <- names(table2_r1_jul28_sel )
identical(names(table1_r1_jul28_sel), names (table2_r1_jul28_sel ))

table_comb_r1_Jul28 <-rbind(table1_r1_jul28_sel, table2_r1_jul28_sel)

#Jul28_r2

table1_r2_jul28 <- read.csv("data/2019-07-28-0812_BH_R2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_jul28<- read.csv("data/2019-07-28-0820_logdata_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul28_sel <-table1_r2_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =2) %>%
  filter(!is.na(Fv.Fm))


table2_r2_jul28_sel <-table2_r2_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =2)
table2_r2_jul28_sel <- table2_r2_jul28_sel [-1, ]


names(table1_r2_jul28_sel) <- names(table2_r2_jul28_sel )
identical(names(table1_r2_jul28_sel), names (table2_r2_jul28_sel ))

table_comb_r2_Jul28 <-rbind(table1_r2_jul28_sel, table2_r2_jul28_sel)

#Jul28_r3

table1_r3_jul28 <- read.csv("data/2019-07-28-1105_BH_R3.csv", header = TRUE, sep =",", skip = 15)
table2_r3_jul28<- read.csv("data/2019-07-28-1107_logdata_BH_R3-low_RH_last_points.csv", header= TRUE, sep =",", skip = 13)

table1_r3_jul28_sel <-table1_r3_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =3)
  table1_r3_jul28_sel <- table1_r3_jul28_sel [-1, ]

table2_r3_jul28_sel <-table2_r3_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =3)
table2_r3_jul28_sel <- table2_r3_jul28_sel [-1, ]


names(table1_r3_jul28_sel) <- names(table2_r3_jul28_sel )
identical(names(table1_r3_jul28_sel), names (table2_r3_jul28_sel ))

table_comb_r3_Jul28 <-rbind(table1_r3_jul28_sel, table2_r3_jul28_sel)


#Jul28_r4

table1_r4_jul28 <- read.csv("data/2019-07-28-1310_BH_R4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_jul28<- read.csv("data/2019-07-28-1316_logdata_BH_R4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul28_sel <-table1_r4_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =4)
table1_r4_jul28_sel <- table1_r4_jul28_sel [-1, ]

table2_r4_jul28_sel <-table2_r4_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =4)
table2_r4_jul28_sel <- table2_r4_jul28_sel [-1, ]


names(table1_r4_jul28_sel) <- names(table2_r4_jul28_sel )
identical(names(table1_r4_jul28_sel), names (table2_r4_jul28_sel ))

table_comb_r4_Jul28 <-rbind(table1_r4_jul28_sel, table2_r4_jul28_sel)

#Jul28_r5

table1_r5_jul28 <- read.csv("data/2019-07-28-1606_BH_R5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_jul28<- read.csv("data/2019-07-28-1610_logdata_BH_R5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_jul28_sel <-table1_r5_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =5)
table1_r5_jul28_sel <- table1_r5_jul28_sel [-1, ]

table2_r5_jul28_sel <-table2_r5_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =5)
table2_r5_jul28_sel <- table2_r5_jul28_sel [-1, ]


names(table1_r5_jul28_sel) <- names(table2_r5_jul28_sel )
identical(names(table1_r5_jul28_sel), names (table2_r5_jul28_sel ))

table_comb_r5_Jul28 <-rbind(table1_r5_jul28_sel, table2_r5_jul28_sel)

#Combine all rounds diurnals jul 28

names(table_comb_r1_Jul28) <- names(table_comb_r2_Jul28) 
names(table_comb_r2_Jul28) <- names(table_comb_r3_Jul28)
names(table_comb_r3_Jul28) <- names(table_comb_r4_Jul28)
names(table_comb_r4_Jul28) <- names(table_comb_r5_Jul28)

identical(names(table_comb_r1_Jul28), names (table_comb_r2_Jul28))
identical(names(table_comb_r2_Jul28), names (table_comb_r3_Jul28))
identical(names(table_comb_r3_Jul28), names (table_comb_r4_Jul28))
identical(names(table_comb_r4_Jul28), names (table_comb_r5_Jul28))

table_diurnals_Jul28 <-rbind(table_comb_r1_Jul28, table_comb_r2_Jul28, table_comb_r3_Jul28, table_comb_r4_Jul28, table_comb_r5_Jul28)

write.csv(table_diurnals_Jul28, "data_output/table_diurnal_jul28")

#Aug01_r1

table1_r1_aug1 <- read.csv("data/2019-08-01-0456_BH_R1.csv", header = TRUE, sep =",", skip = 15)
table2_r1_aug1<- read.csv("data/2019-08-01-0500_logdata_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_aug1_sel <-table1_r1_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =1)
table1_r1_aug1_sel <- table1_r1_aug1_sel [-1, ]

table2_r1_aug1_sel <-table2_r1_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =1)
table2_r1_aug1_sel <- table2_r1_aug1_sel [-1, ]


names(table1_r1_aug1_sel) <- names(table2_r1_aug1_sel )
identical(names(table1_r1_aug1_sel), names (table2_r1_aug1_sel ))

table_comb_r1_aug1 <-rbind(table1_r1_aug1_sel, table2_r1_aug1_sel)

#Aug1_r2

table1_r2_aug1 <- read.csv("data/2019-08-01-0732_BH_R2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_aug1<- read.csv("data/2019-08-01-0714_logdata_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_aug1_sel <-table1_r2_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =2)
table1_r2_aug1_sel <- table1_r2_aug1_sel [-1, ]

table2_r2_aug1_sel <-table2_r2_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =2)
table2_r2_aug1_sel <- table2_r2_aug1_sel [-1, ]

names(table1_r2_aug1_sel) <- names(table2_r2_aug1_sel )
identical(names(table1_r2_aug1_sel), names (table2_r2_aug1_sel ))

table_comb_r2_aug1 <-rbind(table1_r2_aug1_sel, table2_r2_aug1_sel)

#Aug1_r3

table1_r3_aug1 <- read.csv("data/2019-08-01-0954_BH_R3.csv", header = TRUE, sep =",", skip = 15)
table2_r3_aug1<- read.csv("data/2019-08-01-0958_logdata_BH_R3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_aug1_sel <-table1_r3_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =3)
table1_r3_aug1_sel <- table1_r3_aug1_sel [-1, ]

table2_r3_aug1_sel <-table2_r3_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =3)
table2_r3_aug1_sel <- table2_r3_aug1_sel [-1, ]

names(table1_r3_aug1_sel) <- names(table2_r3_aug1_sel )
identical(names(table1_r3_aug1_sel), names (table2_r3_aug1_sel ))

table_comb_r3_aug1 <-rbind(table1_r3_aug1_sel, table2_r3_aug1_sel)

#Aug1_r4

table1_r4_aug1 <- read.csv("data/2019-08-01-1235_BH_R4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_aug1<- read.csv("data/2019-08-01-1236_logdata_BH_R4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_aug1_sel <-table1_r4_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =4)
table1_r4_aug1_sel <- table1_r4_aug1_sel [-1, ]

table2_r4_aug1_sel <-table2_r4_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =4)
table2_r4_aug1_sel <- table2_r4_aug1_sel [-1, ]

names(table1_r4_aug1_sel) <- names(table2_r4_aug1_sel )
identical(names(table1_r4_aug1_sel), names (table2_r4_aug1_sel ))

table_comb_r4_aug1 <-rbind(table1_r4_aug1_sel, table2_r4_aug1_sel)

#Aug1_r5

table1_r5_aug1 <- read.csv("data/2019-08-01-1601_BH_R5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_aug1<- read.csv("data/2019-08-01-1601_logdata_BH_R5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_aug1_sel <-table1_r5_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =5)
table1_r5_aug1_sel <- table1_r5_aug1_sel [-1, ]

table2_r5_aug1_sel <-table2_r5_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =5)
table2_r5_aug1_sel <- table2_r5_aug1_sel [-1, ]

names(table1_r5_aug1_sel) <- names(table2_r5_aug1_sel )
identical(names(table1_r5_aug1_sel), names (table2_r5_aug1_sel ))

table_comb_r5_aug1 <-rbind(table1_r5_aug1_sel, table2_r5_aug1_sel)

names(table_comb_r1_aug1) <- names(table_comb_r2_aug1)
names(table_comb_r2_aug1) <- names(table_comb_r3_aug1)
names(table_comb_r3_aug1) <- names(table_comb_r4_aug1)
names(table_comb_r4_aug1) <- names(table_comb_r5_aug1)


identical(names(table_comb_r1_aug1), names (table_comb_r2_aug1))
identical(names(table_comb_r2_aug1), names (table_comb_r3_aug1))
identical(names(table_comb_r3_aug1), names (table_comb_r4_aug1))
identical(names(table_comb_r4_aug1), names (table_comb_r5_aug1))


table_diurnals_aug1 <-rbind(table_comb_r1_aug1, table_comb_r2_aug1, table_comb_r3_aug1, table_comb_r4_aug1, table_comb_r5_aug1)

write.csv(table_diurnals_aug1, "data_output/table_diurnals_aug1")

#Aug_15

table1_r1_aug15 <- read.csv("data/2019-08-15-0524_Round1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_aug15<- read.csv("data/2019-08-15-0533_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_aug15_sel <-table1_r1_aug15 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =1)
table1_r1_aug15_sel <- table1_r1_aug15_sel [-1, ]

table2_r1_aug15_sel <-table2_r1_aug15 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =1)
table2_r1_aug15_sel <- table2_r1_aug15_sel [-1, ]


names(table1_r1_aug15_sel) <- names(table2_r1_aug15_sel )
identical(names(table1_r1_aug15_sel), names (table2_r1_aug15_sel ))

table_comb_r1_aug15 <-rbind(table1_r1_aug15_sel, table2_r1_aug15_sel)

#Aug15_r2

table1_r2_aug15 <- read.csv("data/2019-08-15-0759_BH_R2.csv", header = TRUE, sep =",", skip = 13)
table2_r2_aug15<- read.csv("data/2019-08-15-0804_round2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_aug15_sel <-table1_r2_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =2)
table1_r2_aug15_sel <- table1_r2_aug15_sel [-1, ]

table2_r2_aug15_sel <-table2_r2_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =2)
table2_r2_aug15_sel <- table2_r2_aug15_sel [-1, ]


names(table1_r2_aug15_sel) <- names(table2_r2_aug15_sel )
identical(names(table1_r2_aug15_sel), names (table2_r2_aug15_sel ))

table_comb_r2_aug15 <-rbind(table1_r2_aug15_sel, table2_r2_aug15_sel)

#Aug15_r3

table1_r3_aug15 <- read.csv("data/2019-08-15-1033_BH_R3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_aug15<- read.csv("data/2019-08-15-1042_Round 3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_aug15_sel <-table1_r3_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =3)
table1_r3_aug15_sel <- table1_r3_aug15_sel [-1, ]

table2_r3_aug15_sel <-table2_r3_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =3)
table2_r3_aug15_sel <- table2_r3_aug15_sel [-1, ]


names(table1_r3_aug15_sel) <- names(table2_r3_aug15_sel )
identical(names(table1_r3_aug15_sel), names (table2_r3_aug15_sel ))

table_comb_r3_aug15 <-rbind(table1_r3_aug15_sel, table2_r3_aug15_sel)

#Aug15_r4

table1_r4_aug15 <- read.csv("data/2019-08-15-1257_BH_R4.csv", header = TRUE, sep =",", skip = 13)
table2_r4_aug15<- read.csv("data/2019-08-15-1257_round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_aug15_sel <-table1_r4_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =4)
table1_r4_aug15_sel <- table1_r4_aug15_sel [-1, ]

table2_r4_aug15_sel <-table2_r4_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =4)
table2_r4_aug15_sel <- table2_r4_aug15_sel [-1, ]


names(table1_r4_aug15_sel) <- names(table2_r4_aug15_sel )
identical(names(table1_r4_aug15_sel), names (table2_r4_aug15_sel ))

table_comb_r4_aug15 <-rbind(table1_r4_aug15_sel, table2_r4_aug15_sel)

#Aug15_r5

table1_r5_aug15 <- read.csv("data/2019-08-15-1600_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_aug15<- read.csv("data/2019-08-15-1606_Round5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_aug15_sel <-table1_r5_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =5)
table1_r5_aug15_sel <- table1_r5_aug15_sel [-1, ]

table2_r5_aug15_sel <-table2_r5_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =5)
table2_r5_aug15_sel <- table2_r5_aug15_sel [-1, ]


names(table1_r5_aug15_sel) <- names(table2_r5_aug15_sel )
identical(names(table1_r5_aug15_sel), names (table2_r5_aug15_sel ))

table_comb_r5_aug15 <-rbind(table1_r5_aug15_sel, table2_r5_aug15_sel)

#All round combined diurnals aug 15


names(table_comb_r1_aug15) <- names(table_comb_r2_aug15) 
names(table_comb_r2_aug15) <- names(table_comb_r3_aug15)
names(table_comb_r3_aug15) <- names(table_comb_r4_aug15)
names(table_comb_r4_aug15) <- names(table_comb_r5_aug15)


identical(names(table_comb_r1_aug15), names (table_comb_r2_aug15))
identical(names(table_comb_r2_aug15), names (table_comb_r3_aug15))
identical(names(table_comb_r3_aug15), names (table_comb_r4_aug15))
identical(names(table_comb_r4_aug15), names (table_comb_r5_aug15))


table_diurnals_aug15 <-rbind(table_comb_r1_aug15, table_comb_r2_aug15, table_comb_r3_aug15, table_comb_r4_aug15, table_comb_r5_aug15)

write.csv(table_diurnals_aug15, "data_output/table_diurnals_aug15")
