setwd("E:\\南方医科大学\\DOSE新paper\\MGI")
packagedir <- "E:\\enrichplot_export\\MPO.db"
sqlite_path <- paste(packagedir, sep=.Platform$file.sep, "inst", "extdata")
if(!dir.exists(sqlite_path)){dir.create(sqlite_path,recursive = TRUE)}
dbfile <- file.path(sqlite_path, "MPO.sqlite")
unlink(dbfile)

###################################################
### create database
###################################################
## Create the database file
library(RSQLite)
drv <- dbDriver("SQLite")
db <- dbConnect(drv, dbname=dbfile)
## dbDisconnect(db)

source("E:\\enrichplot_export\\MPO.db\\inst\\extdata\\parse-obo.R")
library(dplyr)
## DOTERM
mpobo <- parse_mp("MPheno_OBO.ontology")
mpterm <- mpobo$mpinfo[, c(1,2)]
colnames(mpterm) <- c("mpid", "term")
MPOTERM <- mpterm
MPOTERM <- na.omit(MPOTERM)
MPOTERM <- MPOTERM[MPOTERM[, 1] != "", ]
MPOTERM <- MPOTERM[MPOTERM[, 2] != "", ]
dbWriteTable(conn = db, "mp_term", MPOTERM, row.names=FALSE, overwrite = TRUE)


## ALIAS
alias <- mpobo$alias
colnames(alias) <- c("mpid", "alias")
ALIAS <- alias
ALIAS <- na.omit(ALIAS)
ALIAS <- ALIAS[ALIAS[, 1] != "", ]
ALIAS <- ALIAS[ALIAS[, 2] != "", ]
dbWriteTable(conn = db, "mp_alias", ALIAS, row.names=FALSE, overwrite = TRUE)

## SYNONYM
synonym <- mpobo$synonym
colnames(synonym) <- c("mpid", "synonym")
SYNONYM <- synonym
SYNONYM <- na.omit(SYNONYM)
SYNONYM <- SYNONYM[SYNONYM[, 1] != "", ]
SYNONYM <- SYNONYM[SYNONYM[, 2] != "", ]
dbWriteTable(conn = db, "mp_synonym", SYNONYM, row.names=FALSE, overwrite = TRUE)


## Developmental Anatomy
library(data.table)
tissue <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MP_EMAPA.rpt", header = FALSE)
class(tissue) <- "data.frame"
tissue <- tissue[, c(1,4)]
colnames(tissue) <- c("mpid", "anatomy")
ANATOMY <- tissue
ANATOMY <- na.omit(ANATOMY)
ANATOMY <- ANATOMY[ANATOMY[, 1] != "", ]
ANATOMY <- ANATOMY[ANATOMY[, 2] != "", ]
dbWriteTable(conn = db, "mp_anatomy", ANATOMY, row.names=FALSE, overwrite = TRUE)


## DOPARENTS
MPOPARENTS <- mpobo$rel
colnames(MPOPARENTS) <- c("mpid", "parent")
MPOPARENTS <- na.omit(MPOPARENTS)
MPOPARENTS <- MPOPARENTS[MPOPARENTS[, 1] != "", ]
MPOPARENTS <- MPOPARENTS[MPOPARENTS[, 2] != "", ]
dbWriteTable(conn = db, "mp_parent", MPOPARENTS, row.names=FALSE)


## DOCHILDREN
MPOCHILDREN <- mpobo$rel[, c(2,1)]
MPOCHILDREN <- MPOCHILDREN[order(MPOCHILDREN[, 1]), ]
colnames(MPOCHILDREN) <- c("mpid", "children")
MPOCHILDREN <- na.omit(MPOCHILDREN)
MPOCHILDREN <- MPOCHILDREN[MPOCHILDREN[, 1] != "", ]
MPOCHILDREN <- MPOCHILDREN[MPOCHILDREN[, 2] != "", ]
dbWriteTable(conn = db, "mp_children", MPOCHILDREN, row.names=FALSE)

## DOANCESTOR
ancestor_list <- split(MPOPARENTS[, 2], MPOPARENTS[, 1])
getAncestor <- function(id) {
    ans_temp <- which(MPOPARENTS[, 1] %in% ancestor_list[[id]])
    ids <- MPOPARENTS[ans_temp, 2]
    content <- c(ancestor_list[[id]], ids)
    while(!all(is.na(ids))) {
        ans_temp <- which(MPOPARENTS[, 1] %in% ids)
        ids <- MPOPARENTS[ans_temp, 2]
        content <- c(content, ids)
    }
    content[!is.na(content)]
}

for (id in names(ancestor_list)) {
    ancestor_list[[id]] <- getAncestor(id)
}
ancestordf <- stack(ancestor_list)[, c(2, 1)]
ancestordf[, 1] <- as.character(ancestordf[, 1])
ancestordf <- unique(ancestordf)
MPOANCESTOR <- ancestordf
colnames(MPOANCESTOR) <- c("mpid", "ancestor")
MPOANCESTOR <- na.omit(MPOANCESTOR)
MPOANCESTOR <- MPOANCESTOR[MPOANCESTOR[, 1] != "", ]
MPOANCESTOR <- MPOANCESTOR[MPOANCESTOR[, 2] != "", ]
dbWriteTable(conn = db, "mp_ancestor", MPOANCESTOR, row.names=FALSE)


# DOOFFSPRING
MPOOFFSPRING <- ancestordf[, c(2, 1)]
MPOOFFSPRING <- MPOOFFSPRING[order(MPOOFFSPRING[, 1]), ]
colnames(MPOOFFSPRING) <- c("mpid", "offspring")
MPOOFFSPRING <- na.omit(MPOOFFSPRING)
MPOOFFSPRING <- MPOOFFSPRING[MPOOFFSPRING[, 1] != "", ]
MPOOFFSPRING <- MPOOFFSPRING[MPOOFFSPRING[, 2] != "", ]
dbWriteTable(conn = db, "mp_offspring", MPOOFFSPRING, row.names=FALSE)

# gene2MP
library(clusterProfiler)
library(org.Mm.eg.db)
HMD_HumanPhenotype <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\HMD_HumanPhenotype.rpt", sep = "\t")
class(HMD_HumanPhenotype) <- "data.frame"
gene_mp <- HMD_HumanPhenotype[, c(4, 5)]
gene_mp <- gene_mp[gene_mp[, 2] != "", ]
bb <- strsplit(gene_mp[, 2], ", ")
aa <- rep(gene_mp[, 1], times = unlist(lapply(bb, length)))
gene_mp <- data.frame(`MGI` = aa, MP = unlist(bb))
gene_mp <- unique(gene_mp)

MGI_GenePheno <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_GenePheno.rpt", sep = "\t")
class(MGI_GenePheno) <- "data.frame"
colnames(MGI_GenePheno) <- c("Allelic Composition", "Allelic Composition", "Allele ID(s)", 
    "Genetic Background", "Mammalian Phenotype ID", "PubMed ID", "MGI Marker Accession ID", "MGI Genotype Accession ID")
MGI_GenePheno <- MGI_GenePheno[, c("MGI Marker Accession ID", "Mammalian Phenotype ID")]
colnames(MGI_GenePheno) <- c("MGI", "MP")


MGI_Geno_DiseaseDO <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_Geno_DiseaseDO.rpt", sep = "\t")
class(MGI_Geno_DiseaseDO) <- "data.frame"
MGI_Geno_DiseaseDO <- MGI_Geno_DiseaseDO[, c(7, 5)]
colnames(MGI_Geno_DiseaseDO) <- c("MGI", "MP")


MGI_PhenotypicAllele <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_PhenotypicAllele.rpt", sep = "\t")
class(MGI_PhenotypicAllele) <- "data.frame"
MGI_PhenotypicAllele <- MGI_PhenotypicAllele[, c(7, 11)]
MGI_PhenotypicAllele <- MGI_PhenotypicAllele[MGI_PhenotypicAllele[, 2] != "", ]
bb <- strsplit(MGI_PhenotypicAllele[, 2], ",")
aa <- rep(MGI_PhenotypicAllele[, 1], times = unlist(lapply(bb, length)))
MGI_PhenotypicAllele <- data.frame(MGI = aa, MP = unlist(bb))
MGI_PhenotypicAllele <- unique(MGI_PhenotypicAllele)


MGI_Geno_NotDiseaseDO <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_Geno_NotDiseaseDO.rpt", sep = "\t")
class(MGI_Geno_NotDiseaseDO) <- "data.frame"
MGI_Geno_NotDiseaseDO <- MGI_Geno_NotDiseaseDO[, c(7, 5)]
colnames(MGI_Geno_NotDiseaseDO) <- c("MGI", "MP")


MGI_Pheno_Sex <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_Pheno_Sex.rpt", sep = "\t")
class(MGI_Pheno_Sex) <- "data.frame"
MGI_Pheno_Sex <- MGI_Pheno_Sex[, c(1, 3)]
colnames(MGI_Pheno_Sex) <- c("MGI", "MP")

## data from http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-18.0/results/
## (https://www.mousephenotype.org/)


impc <- read.csv("E:\\南方医科大学\\DOSE新paper\\IMPC\\genotype-phenotype-assertions-ALL.csv")
impc <- impc[,c("marker_accession_id", "mp_term_id")]
colnames(impc) <- c("MGI", "MP")



mp_mgi <- do.call(rbind,list(gene_mp, MGI_GenePheno, MGI_Geno_DiseaseDO, MGI_PhenotypicAllele,
    MGI_Geno_NotDiseaseDO, MGI_Pheno_Sex, impc))
mp_mgi <- unique(mp_mgi)

# convert MGI id
MGI_GenePheno <- fread("MGI_GenePheno.rpt", sep = "\t")
class(MGI_GenePheno) <- "data.frame"
colnames(MGI_GenePheno) <- c("Allelic Composition", "Allelic Composition", "Allele ID(s)", 
    "Genetic Background", "Mammalian Phenotype ID", "PubMed ID", "MGI Marker Accession ID", "MGI Genotype Accession ID")
aa <- MGI_GenePheno[, c("Allele ID(s)", "MGI Marker Accession ID", "MGI Genotype Accession ID")]

kk <- keys(org.Mm.eg.db, "MGI")
kk <- gsub("^\\w+:", "", kk)

# kk <- strsplit(kk, ":")
# kk <- lapply(kk, function(x) {
#     x <- x[-1]
#     x <- paste(x, collapse = ":")
#     })
# kk <- unique(unlist(kk))

mp_mgi1 <- mp_mgi[mp_mgi[, 1] %in% kk, ]
mp_mgi_fail <- mp_mgi[!(mp_mgi[, 1] %in% kk), ]
length(intersect(mp_mgi_fail[, 1], aa[, 1]))
length(intersect(mp_mgi_fail[, 1], aa[, 2]))
length(intersect(mp_mgi_fail[, 1], aa[, 3]))

mp_mgi2 <- mp_mgi_fail[mp_mgi_fail[,1] %in% aa[, 2], ]
mp_mgi_fail <- mp_mgi_fail[!(mp_mgi_fail[,1] %in% aa[, 2]), ]

mp_mgi3 <- mp_mgi_fail
mp_mgi3[, 1] <- aa[match(mp_mgi_fail[, 1], aa[, 3]), 2]
mp_mgi3 <- na.omit(mp_mgi3)
mp_mgi_new <- do.call(rbind, list(mp_mgi1, mp_mgi2, mp_mgi3))

bb <- bitr(paste0("MGI:", mp_mgi_new[, 1]), "MGI", "ENTREZID", org.Mm.eg.db)
# 11.74% of input gene IDs are fail to map...
bb[, 1] <-  gsub("^\\w+:", "", bb[,1])
mp_mgi_new[, 1] <- bb[match(mp_mgi_new[, 1], bb[, 1]), 2]

MPOMPMGI <- mp_mgi_new[, c(2,1)]
colnames(MPOMPMGI) <- c("mpid", "mgi")
MPOMPMGI <- na.omit(MPOMPMGI)
MPOMPMGI <- MPOMPMGI[MPOMPMGI[, 1] != "", ]
MPOMPMGI <- MPOMPMGI[MPOMPMGI[, 2] != "", ]
dbWriteTable(conn = db, "mp_mgi", MPOMPMGI, row.names=FALSE)

# MP2DO
###################
source("E:\\enrichplot_export\\MPO.db\\inst\\extdata\\MP_HP_map.r")
colnames(MP2DO) <- c("mpid", "doid")
# > head(MP2DO)
#           MP         DOID
# 1 MP:0001289 DOID:0111809
# 2 MP:0001289 DOID:0060282
# 3 MP:0001289 DOID:0111234
# 4 MP:0001293 DOID:0050691
# 5 MP:0001293 DOID:0060861
# 6 MP:0001293    DOID:2907
mgi2do <- inner_join(MPOMPMGI, MP2DO, "mpid")
mgi2do <- unique(mgi2do[, c(2, 3)])

# gene2DO
MGI_DO <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_DO.rpt", sep = "\t")
class(MGI_DO) <- "data.frame"
MGI_DO <- MGI_DO[MGI_DO$`Common Organism Name` == "mouse, laboratory", ]
MGI_DO <- MGI_DO[, c(8, 1)]
colnames(MGI_DO) <- c("MGI", "DO")


MGI_Geno_DiseaseDO <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_Geno_DiseaseDO.rpt", sep = "\t")
class(MGI_Geno_DiseaseDO) <- "data.frame"
MGI_Geno_DiseaseDO <- MGI_Geno_DiseaseDO[, c(7, 8)]
colnames(MGI_Geno_DiseaseDO) <- c("MGI", "DO")


MGI_Geno_NotDiseaseDO <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_Geno_NotDiseaseDO.rpt", sep = "\t")
class(MGI_Geno_NotDiseaseDO) <- "data.frame"
MGI_Geno_NotDiseaseDO <- MGI_Geno_NotDiseaseDO[, c(7, 8)]
colnames(MGI_Geno_NotDiseaseDO) <- c("MGI", "DO")


# do <- fread("E:\\南方医科大学\\DOSE新paper\\DISEASE-ALLIANCE_COMBINED.tsv", sep = "\t")
# class(do) <- "data.frame"
# do_m <- do[do$Source == "MGI", ]
# do_m <- do_m[, c("DBObjectID", "DOID")]
# colnames(do_m) <- c("MGI", "DO")

do <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\DISEASE-ALLIANCE_MGI.tsv", sep = "\t")
class(do) <- "data.frame"
do_m <- do[do$Source == "MGI", ]
do_m <- do_m[, c("DBObjectID", "DOID")]
colnames(do_m) <- c("MGI", "DO")




gene2DO <- unique(do.call(rbind, list(MGI_DO, MGI_Geno_DiseaseDO, MGI_Geno_NotDiseaseDO, do_m)))

# bb <- bitr(paste0("MGI:", gene2DO[, 1]), "MGI", "ENTREZID", org.Mm.eg.db)
# 84.9% of input gene IDs are fail to map...
kk <- keys(org.Mm.eg.db, "MGI")
kk <- gsub("^\\w+:", "", kk)
gene2DO1 <- gene2DO[gene2DO[, 1] %in% kk, ] 
gene2DO_fail <- gene2DO[!(gene2DO[, 1] %in% kk), ] 

MGI_GenePheno <- fread("E:\\南方医科大学\\DOSE新paper\\MGI\\MGI_GenePheno.rpt", sep = "\t")
class(MGI_GenePheno) <- "data.frame"
colnames(MGI_GenePheno) <- c("Allelic Composition", "Allelic Composition", "Allele ID(s)", 
    "Genetic Background", "Mammalian Phenotype ID", "PubMed ID", "MGI Marker Accession ID", "MGI Genotype Accession ID")
aa <- MGI_GenePheno[, c("Allele ID(s)", "MGI Marker Accession ID", "MGI Genotype Accession ID")]
length(intersect(gene2DO_fail[, 1], aa[, 1]))
length(intersect(gene2DO_fail[, 1], aa[, 2]))
length(intersect(gene2DO_fail[, 1], aa[, 3]))
# > length(intersect(gene2DO_fail[, 1], aa[, 1]))
# [1] 3341
# > length(intersect(gene2DO_fail[, 1], aa[, 2]))
# [1] 82
# > length(intersect(gene2DO_fail[, 1], aa[, 3]))
# [1] 3947

gene2DO2 <- gene2DO_fail[gene2DO_fail[,1] %in% aa[, 2], ]
gene2DO_fail <- gene2DO_fail[!(gene2DO_fail[,1] %in% aa[, 2]), ]

gene2DO3_1 <- gene2DO3_2 <- gene2DO_fail
gene2DO3_1[, 1] <- aa[match(gene2DO_fail[, 1], aa[, 3]), 2]
gene2DO3_1 <- na.omit(gene2DO3_1)

gene2DO3_2[, 1] <- aa[match(gene2DO_fail[, 1], aa[, 1]), 2]
gene2DO3_2 <- na.omit(gene2DO3_2)

gene2DO_new <- do.call(rbind, list(gene2DO1, gene2DO2, gene2DO3_1, gene2DO3_2))

bb <- bitr(paste0("MGI:", gene2DO_new[, 1]), "MGI", "ENTREZID", org.Mm.eg.db)
# 3.63% of input gene IDs are fail to map...
bb[, 1] <-  gsub("^\\w+:", "", bb[,1])
gene2DO_new[, 1] <- bb[match(gene2DO_new[, 1], bb[, 1]), 2]

MPOMGIDO <- gene2DO_new
colnames(MPOMGIDO) <- c("mgi", "doid")
MPOMGIDO <- unique(rbind(MPOMGIDO, mgi2do))
dbWriteTable(conn = db, "mgi_doid", MPOMGIDO, row.names=FALSE, overwrite = TRUE)

# mp2do
source("MP_HP_map.r")
colnames(MP2DO) <- c("mpid", "doid")
MPOMPDO <- unique(MP2DO)
MPOMPDO <- na.omit(MPOMPDO)
MPOMPDO <- MPOMPDO[MPOMPDO[, 1] != "", ]
MPOMPDO <- MPOMPDO[MPOMPDO[, 2] != "", ]
dbWriteTable(conn = db, "mp_doid", MPOMPDO, row.names=FALSE)

# # mgi2ENTREZID
# # need remove NA
# HMD_HumanPhenotype <- fread("HMD_HumanPhenotype.rpt", sep = "\t")
# class(HMD_HumanPhenotype) <- "data.frame"
# HMD_HumanPhenotype <- HMD_HumanPhenotype[, c(4, 2)]
# colnames(HMD_HumanPhenotype) <- c("mgi", "ENTREZID")

# MGI_Gene_Model_Coord <- fread("MGI_Gene_Model_Coord.rpt", sep = "\t")
# class(MGI_Gene_Model_Coord) <- "data.frame"
# MGI_Gene_Model_Coord <- MGI_Gene_Model_Coord[, c(1, 6)]
# colnames(MGI_Gene_Model_Coord) <- c("mgi", "ENTREZID")

# MGI_EntrezGene <- fread("MGI_EntrezGene.rpt", sep = "\t")
# class(MGI_EntrezGene) <- "data.frame"
# MGI_EntrezGene <- MGI_EntrezGene[, c(1, 9)]
# colnames(MGI_EntrezGene) <- c("mgi", "ENTREZID")

# MGI_DO <- fread("MGI_DO.rpt", sep = "\t")
# class(MGI_DO) <- "data.frame"
# MGI_DO <- MGI_DO[, c(8, 7)]
# MGI_DO <- MGI_DO[MGI_DO[, 1] != "", ]
# colnames(MGI_DO) <- c("mgi", "ENTREZID")

# mgi2id <- do.call(rbind, list(HMD_HumanPhenotype, MGI_Gene_Model_Coord, MGI_EntrezGene, MGI_DO))
# mgi2id <- mgi2id[mgi2id[, 1] != "", ]
# mgi2id <- mgi2id[mgi2id[, 2] != "", ]
# mgi2id <- na.omit(mgi2id)
# mgi2id <- unique(mgi2id)


# # mgi2SYMBOL
# HMD_HumanPhenotype <- fread("HMD_HumanPhenotype.rpt", sep = "\t")
# class(HMD_HumanPhenotype) <- "data.frame"
# HMD_HumanPhenotype <- HMD_HumanPhenotype[, c(4, 3)]
# colnames(HMD_HumanPhenotype) <- c("mgi", "SYMBOL")

# MGI_Gene_Model_Coord <- fread("MGI_Gene_Model_Coord.rpt", sep = "\t")
# class(MGI_Gene_Model_Coord) <- "data.frame"
# MGI_Gene_Model_Coord <- MGI_Gene_Model_Coord[, c(1, 3)]
# colnames(MGI_Gene_Model_Coord) <- c("mgi", "SYMBOL")

# MGI_EntrezGene <- fread("MGI_EntrezGene.rpt", sep = "\t")
# class(MGI_EntrezGene) <- "data.frame"
# MGI_EntrezGene <- MGI_EntrezGene[, c(1, 2)]
# colnames(MGI_EntrezGene) <- c("mgi", "SYMBOL")

# MGI_DO <- fread("MGI_DO.rpt", sep = "\t")
# class(MGI_DO) <- "data.frame"
# MGI_DO <- MGI_DO[, c(8, 6)]
# MGI_DO <- MGI_DO[MGI_DO[, 1] != "", ]
# colnames(MGI_DO) <- c("mgi", "SYMBOL")


metadata <-rbind(c("DBSCHEMA","MPO_DB"),
        c("DBSCHEMAVERSION","1.0"),
        c("MPOSOURCENAME","MGI"),
        c("MPOSOURCURL","http://www.informatics.jax.org/downloads/reports/index.html#pheno"),
        c("MPOSOURCEDATE","20230302"),
        c("Db type", "MPODb"))


metadata <- as.data.frame(metadata)
colnames(metadata) <- c("name", "value") 
dbWriteTable(conn = db, "metadata", metadata, row.names=FALSE, overwrite = TRUE)



map.counts<-rbind(c("TERM", nrow(MPOTERM)),
        c("CHILDREN", nrow(MPOCHILDREN)),
        c("PARENTS", nrow(MPOPARENTS)),
        c("ANCESTOR", nrow(MPOANCESTOR)),
        c("OFFSPRING", nrow(MPOOFFSPRING)))


map.counts <- as.data.frame(map.counts)
colnames(map.counts) <- c("map_name","count")
dbWriteTable(conn = db, "map_counts", map.counts, row.names=FALSE, overwrite = TRUE)

dbListTables(db)
dbListFields(conn = db, "metadata")
dbReadTable(conn = db,"metadata")


map.metadata <- rbind(c("TERM", "Mouse Phenotype Ontology", "http://www.informatics.jax.org/downloads/reports/index.html#pheno","20230302"),
            c("CHILDREN", "Mouse Phenotype Ontology", "http://www.informatics.jax.org/downloads/reports/index.html#pheno","20230302"),
            c("PARENTS", "Mouse Phenotype Ontology", "http://www.informatics.jax.org/downloads/reports/index.html#pheno","20230302"),
            c("ANCESTOR", "Mouse Phenotype Ontology", "http://www.informatics.jax.org/downloads/reports/index.html#pheno","20230302"),
            c("OFFSPRING", "Mouse Phenotype Ontology", "http://www.informatics.jax.org/downloads/reports/index.html#pheno","20230302"))	
map.metadata <- as.data.frame(map.metadata)
colnames(map.metadata) <- c("map_name","source_name","source_url","source_date")
dbWriteTable(conn = db, "map_metadata", map.metadata, row.names=FALSE, overwrite = TRUE)


dbListTables(db)
dbListFields(conn = db, "map_metadata")
dbReadTable(conn = db,"map_metadata")
dbDisconnect(db)

