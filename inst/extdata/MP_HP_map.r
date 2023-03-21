setwd("E:\\南方医科大学\\DOSE新paper\\mh_mapping_initiative-master\\mh_mapping_initiative-master")
library(data.table)
## HP2DO
file1 <- fread("mappings\\hp_doid_pistoia.sssom.tsv", sep = "\t")
class(file1) <- "data.frame"
file1 <- file1[, c(1, 3)]
colnames(file1) <- c("HP", "DOID")
# HP2OMIM 迂回到DO
phenotype <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\phenotype.hpoa", sep = "\t", header = TRUE)
class(phenotype) <- "data.frame"
phenotype <- phenotype[, c(4, 5)]
colnames(phenotype) <- c("HP", "OMIM")

# OMIM2DO
xrefs <- fread("E:\\南方医科大学\\DOSE新paper\\HumanDiseaseOntology-main\\src\\DOreports\\xrefs_in_DO.tsv", sep = "\t", header = TRUE)
class(xrefs) <- "data.frame"
xrefs <- unique(xrefs[grep("OMIM", xrefs[, 3]), c(1, 3)])
colnames(xrefs) <- c("DOID", "OMIM")

library(dplyr)
HP2DO <- inner_join(phenotype, xrefs, "OMIM")
HP2DO <- unique(HP2DO[, c(1, 3)])

# aa <- unique(paste(file1[, 1], file1[,2], sep = "_"))
# bb <- unique(paste(HP2DO[, 1], HP2DO[,2], sep = "_"))
# > length(aa)
# [1] 1291
# > length(bb)
# [1] 44931
# > length(intersect(aa,bb))
# [1] 195
HP2DO <- unique(rbind(file1, HP2DO))

## MP2HP
mp_hp_eye_impc <- fread("mappings\\mp_hp_eye_impc.sssom.tsv", sep = "\t", header = TRUE)
class(mp_hp_eye_impc) <- "data.frame"
mp_hp_eye_impc <- mp_hp_eye_impc[, c(1,4)]

mp_hp_hwt_impc <- fread("mappings\\mp_hp_hwt_impc.sssom.tsv", sep = "\t", header = TRUE)
class(mp_hp_hwt_impc) <- "data.frame"
mp_hp_hwt_impc <- mp_hp_hwt_impc[, c(1,4)]

mp_hp_mgi_all <- fread("mappings\\mp_hp_mgi_all.sssom.tsv", sep = "\t", header = TRUE)
class(mp_hp_mgi_all) <- "data.frame"
mp_hp_mgi_all <- mp_hp_mgi_all[, c(5,1)]

mp_hp_owt_impc <- fread("mappings\\mp_hp_owt_impc.sssom.tsv", sep = "\t", header = TRUE)
class(mp_hp_owt_impc) <- "data.frame"
mp_hp_owt_impc <- mp_hp_owt_impc[, c(1,4)]
mp_hp_owt_impc <- mp_hp_owt_impc[grep("HP:", mp_hp_owt_impc[, 2]), ]

mp_hp_pat_impc <- fread("mappings\\mp_hp_pat_impc.sssom.tsv", sep = "\t", header = TRUE)
class(mp_hp_pat_impc) <- "data.frame"
mp_hp_pat_impc <- mp_hp_pat_impc[, c(1,4)]
mp_hp_pat_impc <- mp_hp_pat_impc[grep("HP:", mp_hp_pat_impc[, 2]), ]

mp_hp_pistoia <- fread("mappings\\mp_hp_pistoia.sssom.tsv", sep = "\t", header = TRUE)
class(mp_hp_pistoia) <- "data.frame"
mp_hp_pistoia <- mp_hp_pistoia[, c(1,3)]
mp_hp_pistoia <- mp_hp_pistoia[grep("HP:", mp_hp_pistoia[, 2]), ]

colnames(mp_hp_eye_impc) <- colnames(mp_hp_hwt_impc) <- colnames(mp_hp_mgi_all) <- 
    colnames(mp_hp_owt_impc) <- colnames(mp_hp_pat_impc) <- colnames(mp_hp_pistoia) <- c("MP", "HP")
mp2hp <- do.call(rbind, list(mp_hp_eye_impc, mp_hp_hwt_impc, mp_hp_mgi_all, mp_hp_owt_impc, mp_hp_pat_impc, mp_hp_pistoia))
mp2hp <- mp2hp[grep("HP:", mp2hp[, 2]), ]
mp2hp <- unique(mp2hp)


## MP2DO
mp_doid_pistoia <- fread("mappings\\mp_doid_pistoia.sssom.tsv", sep = "\t")
class(mp_doid_pistoia) <- "data.frame"
mp_doid_pistoia <- mp_doid_pistoia[, c(1, 3)]
colnames(mp_doid_pistoia) <- c("MP", "DOID")

MP2DO <- inner_join(mp2hp, HP2DO, "HP")
MP2DO <- unique(MP2DO[, c(1, 3)])

MP2DO <- rbind(MP2DO, mp_doid_pistoia)








