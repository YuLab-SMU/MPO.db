MPODb <- setRefClass("MPODb", contains="AnnotationDb")
.keys <- getFromNamespace(".keys", "AnnotationDbi")
.cols <- getFromNamespace(".cols", "AnnotationDbi")
smartKeys <- getFromNamespace("smartKeys", "AnnotationDbi")

.queryForKeys <- getFromNamespace(".queryForKeys", "AnnotationDbi")
dbQuery <- getFromNamespace("dbQuery", "AnnotationDbi")


setMethod("keys", "MPODb",
    function(x, keytype, ...){
        if(missing(keytype)) keytype <- "mpid"
        if (keytype == "mgi") {
            return(unique(toTable(MPOMPMGI)[, keytype]))
        } else {
            return(unique(toTable(MPOTERM)[, keytype]))
        }


    }
)



setMethod("keytypes", "MPODb",
    function(x) {
        c("mpid", "term", "mgi")
    }
)


setMethod("select", "MPODb",
    function(x, keys, columns, keytype, ...){
        if (missing(keytype)) keytype <- "mpid"
        keytype <- match.arg(keytype, c("mpid","term", "mgi"))
        strKeys <- paste0("\"", keys, "\"", collapse = ",")

        columns <- unique(c(keytype, columns))
        if (length(setdiff(columns, c("mgi", "doid"))) > 0) {
            columns <- unique(c("mpid", columns))
        }
        if (keytype == "mgi") {
            columns2 <- setdiff(columns, c("mgi", "doid"))
            if (length(columns2) > 0) {
                sqls <- paste("SELECT ", paste(columns, collapse = ","),
                    " FROM mp_mgi")
                if ("doid" %in% columns) {
                    leftJoin <- "LEFT JOIN  mgi_doid USING (mgi)"
                    sqls <- c(sqls, leftJoin)
                }
                columns3 <- setdiff(columns, c("mgi", "mpid", "doid"))
                for (col in columns3) {
                    leftJoin <- paste0("LEFT JOIN  ", paste0("mp_",col,
                        " USING (mpid)"))
                    sqls <- c(sqls, leftJoin)
                }
                sqls <- c(sqls, paste0("WHERE mp_mgi.mgi in (", strKeys, ")"))
            } else {
                sqls <- paste("SELECT ", paste(columns, collapse = ","),
                    " FROM mgi_doid")
                sqls <- c(sqls, paste0("WHERE mgi_doid.mgi in (", strKeys, ")"))
            }
        }
        if (keytype == "term") {
            sqls <- paste("SELECT ", paste(columns, collapse = ","),
                " FROM mp_term")
            columns2 <- setdiff(columns, c("term", "mpid"))
            if (length(columns2) > 0) {
                for (col in columns2) {
                    leftJoin <- paste0("LEFT JOIN  ", paste0("mp_",col,
                        " USING (mpid)"))
                    sqls <- c(sqls, leftJoin)
                }
            }
            sqls <- c(sqls,
                    paste0("WHERE mp_term.term in (", strKeys, ")"))
        }

        if (keytype == "mpid") {
            sqls <- paste("SELECT ", paste(columns, collapse = ","),
                " FROM mp_term")
            columns2 <- setdiff(columns, c("mpid", "term"))
            if (length(columns2) > 0) {
                for (col in columns2) {
                    leftJoin <- paste0("LEFT JOIN  ", paste0("mp_",col,
                        " USING (mpid)"))
                    sqls <- c(sqls, leftJoin)
                }
            }
            sqls <- c(sqls,
                    paste0("WHERE mp_term.mpid in (", strKeys, ")"))
        }
        sqls <- paste(sqls, collapse = " ")
        res <- dbQuery(dbconn(x), sqls)
        return(res)
    }
)


setMethod("columns", "MPODb",
    function(x) {
        c("mpid","term", "alias", "synonym", "parent", "children",
            "ancestor", "offspring", "mgi", "doid")
    }
)
