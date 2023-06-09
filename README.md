# MPO.db
## :writing_hand: Authors

Erqiang Hu

Department of Bioinformatics, School of Basic Medical Sciences, Southern
Medical University.

## :arrow_double_down: Installation

``` r
if(!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("YuLab-SMU/MPO.db")
```

## Introduction
We have developed the human disease ontology R package HDO.db, 
which provides the semantic relationship between human diseases.
Relying on the DOSE and GOSemSim packages we developed, we can carry out 
disease enrichment and semantic similarity analyses. 
Many biological studies are achieved through mouse models, and a large number 
of data indicate the association between genotypes and phenotypes or diseases. 

The study of model organisms can be transformed 
into useful knowledge about normal human biology and disease to 
facilitate treatment and early screening for diseases. 
Organism-specific genotype-phenotypic associations can be applied 
to cross-species phenotypic studies to clarify previously unknown 
phenotypic connections in other species. Using the same principle 
to diseases can identify genetic associations and even help to 
identify disease associations that are not obvious. Therefore, 
as a supplement to HDO.db and DOSE, we developed mouse phenotypic 
ontology R package MPO.db.

MPO.db mainly contains four kinds of annotation information, which come from:

(1) Mammalian Phenotype Ontology data
The ontology data contains the id, name, def, and synonym of the ontology, 
as also as the parent-child relationship between the ontology. The data comes from:
MPheno_OBO.ontology file downloaded from http://www.informatics.jax.org/downloads/reports/index.html#pheno.

(2) Gene-phenotype association data
These data demonstrate the effect of each genotype on the phenotype. The data come from:
MGI database(http://www.informatics.jax.org/downloads/reports/index.html#pheno) and 
IMPC database(http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-18.0/results/).

(3) Gene-disease association data
These data demonstrate the effect of each genotype on the disease. The data come from:
MGI database(http://www.informatics.jax.org/downloads/reports/index.html#pheno),
IMPC database(http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-18.0/results/),
and alliance of genome resources(https://www.alliancegenome.org/downloads).

(4) Phenotype-disease association data
These data demonstrate the effect of each phenotype on the disease. The data come from:
https://github.com/DiseaseOntology/HumanDiseaseOntology,
and https://github.com/mapping-commons/mh_mapping_initiative.


```{r setup}
library(MPO.db)
```

## Overview

```{r}
library(AnnotationDbi)
```

MPO.db provide these AnnDbBimap object:
```{r}
ls("package:MPO.db")
packageVersion("MPO.db")
```

You can use `help` function to get their documents: `help(MPOFFSPRING)`
```{r}
toTable(MPOmetadata)
MPOMAPCOUNTS
```


## Fetch whole MP terms

In MPO.db, `MPOTERM` represet the whole MP terms and their names. The users can 
also get their aliases and synonyms from `MPOALIAS` and `MPOSYNONYM`, 
respectively.

convert MPOTERM to table
```{r}
doterm <- toTable(MPOTERM)
head(doterm)
```


convert MPOTERM to list
```{r}
dotermlist <- as.list(MPOTERM)
head(dotermlist)
```

get alias of `MP:0000013`
```{r}
doalias <- as.list(MPOALIAS)
doalias[['MP:0000013']]
```

get synonym of `MP:0000013`
```{r}
dosynonym <- as.list(MPOSYNONYM)
dosynonym[['MP:0000013']]
```


## Fetch the relationship between MP terms
Similar to `HDO.db`, we provide four Bimap objects to represent relationship
between MP terms: MPOANCESTOR,MPOPARENTS,MPOOFFSPRING, and MPOCHILDREN.

### MPOANCESTOR
MPOANCESTOR describes the association between MP terms and their ancestral 
terms based on a directed acyclic graph (DAG) defined by the Mouse Phenotype Ontology.
We can use `toTable` function in `AnnotationDbi` package to get a two-column 
data.frame: the first column means the MP term ids, and the second column means 
their ancestor terms.
```{r}
anc_table <- toTable(MPOANCESTOR)
head(anc_table)
```

get ancestor of "MP:0000013"
```{r}
anc_list <- AnnotationDbi::as.list(MPOANCESTOR)
anc_list[["MP:0000013"]]
```


### MPOPARENTS
MPOPARENTS describes the association between MP terms and their direct parent 
terms based on DAG. We can use `toTable` function in `AnnotationDbi` package to 
get a two-column data.frame: the first column means the MP term ids, and the 
second column means their parent terms.
```{r}
parent_table <- toTable(MPOPARENTS)
head(parent_table)
```


get parent term of "MP:0000013"
```{r}
parent_list <- AnnotationDbi::as.list(MPOPARENTS)
parent_list[["MP:0000013"]]
```

###  MPOOFFSPRING
MPOPARENTS describes the association between MP terms and their offspring  
terms based on DAG. it's the exact opposite of `MPOANCESTOR`, 
whose usage is similar to it.

get offspring of "MP:0000013"
```{r}
off_list <- AnnotationDbi::as.list(MPO.db::MPOOFFSPRING)
off_list[["MP:0000013"]]
```

### MPOCHILDREN
MPOCHILDREN describes the association between MP terms and their direct 
children terms based on DAG. it's the exact opposite of `MPOPARENTS`, 
whose usage is similar to it.

get children of "MP:0000013"
```{r}
child_list <- AnnotationDbi::as.list(MPO.db::MPOCHILDREN)
child_list[["MP:0000013"]]
```

The MPO.db support the `select()`, `keys()`, `keytypes()`,
and `columns` interface.
```{r}
columns(MPO.db)
## use mpid keys
dokeys <- keys(MPO.db)[1:100]
res <- select(x = MPO.db, keys = dokeys, keytype = "mpid", 
    columns = c("offspring", "term", "doid", "mgi"))
head(na.omit(res))

key <-  keys(MPO.db, "mpid")[1:100]   
res <- select(x = MPO.db, keys = key, keytype = "mpid", 
    columns = c("mpid", "term", "children"))   
head(na.omit(res))

## use term keys
# dokeys <- head(keys(MPO.db, keytype = "term"))
# res <- select(x = MPO.db, keys = dokeys, keytype = "term", 
#     columns = c("offspring", "mpid", "parent"))   
# head(res)

dokeys <- keys(MPO.db, keytype = "term")[1:100]
res <- select(x = MPO.db, keys = dokeys, keytype = "term", 
    columns = c("offspring", "mpid", "doid", "mgi"))   
head(na.omit(res))

## use mgi keys
key <- keys(MPO.db, "mgi")[1:100]
res <- select(x = MPO.db, keys = key, keytype = "mgi", 
    columns = c("mgi", "mpid", "children"))   
head(na.omit(res))

res <- select(x = MPO.db, keys = key, keytype = "mgi", 
    columns = c("doid", "mgi"))   
head(na.omit(res))


```


## Semantic similarity analysis
Please go to 
<https://yulab-smu.top/biomedical-knowledge-mining-book/> for the vignette.


## Disease enrichment analysis
Please go to 
<https://yulab-smu.top/biomedical-knowledge-mining-book/dose-enrichment.html> 
for the vignette.
