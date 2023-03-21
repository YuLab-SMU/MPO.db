parse_mp <- function(obofile) {
    x = readLines(obofile)

    start = grep("^\\[Term\\]", x)
    end <- c(start[-1] -1, length(x))

    res <- lapply(seq_along(start), function(i) {
        extract_mp_item(x[start[i]:end[i]])
    })

    j <- vapply(res, is.null, logical(1))
    message(paste0(sum(j), '/', length(j)), " obsolete terms found.")
    res <- res[!j]

    mpinfo <- lapply(res, function(x) x$mp) %>% do.call('rbind', .) %>%
        as.data.frame()

    ## 修改一下，以拿到全部信息    
    alias <- lapply(res, function(x) x$alias) %>% do.call('rbind', .)
    synonym <- lapply(res, function(x) x$synonym) %>% do.call('rbind', .)
    rel <- lapply(res, function(x) x$relation) %>% do.call('rbind', .)

    return(list(mpinfo = mpinfo, rel = rel, alias = alias, synonym = synonym))
}


extract_mp_item <- function(item) {
    i <- grep('^\\[Typedef\\]', item)
    if (length(i) > 0) {
        item <- item[-(i[1]:length(item))]
    }
    ## is_obsolete: true
    useless <- get_mp_info(item, '^is_obsolete:')
    if (!is.na(useless)) return(NULL)

    id <- get_mp_info(item, "^id:")
    name <- get_mp_info(item, "^name:")
    alt_id <- get_mp_info(item, "^alt_id:")
    synonym <- get_mp_info(item, "^synonym:")

    def <- get_mp_info(item, "^def:")
    def <- sub('\\"', "", def)
    def <- sub('\\".*', "", def)

    isa <- get_mp_info(item, '^is_a:')
    isa <- sub("\\s*!.*", "", isa)
    
    partof <- get_mp_info(item, '^relationship: part_of')
    partof <- sub("\\s*!.*", "", partof)
    
    res <- list(mp=c(id=id, name=name, def=def),
        alias = data.frame(id = id, alias = alt_id),
        synonym = data.frame(id = id, synonym = synonym),
        relationship = data.frame(id=id, parent=c(isa, partof)))
}

get_mp_info <- function(item, pattern) {
    i <- grep(pattern, item)
    if (length(i) == 0) return(NA)

    sub("\\s*", "",
            sub(pattern, "", item[i])
    )
}
