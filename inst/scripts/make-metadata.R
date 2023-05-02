meta <- data.frame(
      Title = "Mouse Phenotype Ontology database",
      Description = paste0("A set of annotation maps describing the Mouse Phenotype Ontology, ",
          "it mainly contains four kinds of annotation information: ",
          "Mammalian Phenotype Ontology data,",
          "mouse gene - phenotype association data, ",
          "mouse gene - human disease association data, and ",
          "mouse phenotype - human disease association data"
      ),
      BiocVersion = "3.17",
      Genome = NA,
      SourceType = "Multiple",
      SourceUrl = paste("http://www.informatics.jax.org/downloads/reports/index.html#pheno", 
          "http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/",
          sep = ","),
      SourceVersion = "1.2",
      Species = NA,
      TaxonomyId = NA,
      Coordinate_1_based = TRUE,
      DataProvider = "MGI",
      Maintainer = "Erqiang Hu <13766876214@163.com>",
      RDataClass = "SQLite",
      # DispatchClass = "SQLiteFile",
      DispatchClass = "FilePath",
      RDataPath = "MPO.db/MPO.sqlite",
      ResourceName = "MPO.sqlite",
      Tags = "Annotation"
)
write.csv(meta, file="inst/extdata/metadata.csv", row.names=FALSE)


