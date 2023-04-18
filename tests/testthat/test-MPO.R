test_that("HDOANCESTOR", {
    aa <- length(as.list(MPOANCESTOR))
    expect_true(aa > 0)
})


test_that("MPOTERM", {
    doterm <- toTable(MPOTERM)
    expect_true("mpid" %in% colnames(doterm))
    expect_true("MP:0000001" %in% doterm$mpid)
    dotermlist <- as.list(MPOTERM)
    expect_true("MP:0000001" %in% names(dotermlist))
})


test_that("MPOALIAS", {
    doalias <- as.list(MPOALIAS)
    expect_true("MP:0000003" %in% names(doalias))
})


test_that("MPOSYNONYM", {
    dosynonym <- as.list(MPOSYNONYM)
    expect_true("MP:0000003" %in% names(dosynonym))
})

test_that("MPOANCESTOR", {
    anc_list <- AnnotationDbi::as.list(MPOANCESTOR)
    expect_true("MP:0000013" %in% names(anc_list))
})

test_that("MPOPARENTS", {
    parent_table <- toTable(MPOPARENTS)
    parent_list <- AnnotationDbi::as.list(MPOPARENTS)
    expect_true("parent" %in% colnames(parent_table))
    expect_true("MP:0000013" %in% names(parent_list))
})

test_that("MPOOFFSPRING", {
    off_list <- AnnotationDbi::as.list(MPO.db::MPOOFFSPRING)
    expect_true("MP:0000010" %in% names(off_list))
})

test_that("MPOCHILDREN", {
    child_list <- AnnotationDbi::as.list(MPO.db::MPOCHILDREN)
    expect_true("MP:0000003" %in% names(child_list))
})

test_that("keys", {
    dokeys <- keys(MPO.db)[1:100]
    expect_true(grep("MP", dokeys[1]) == 1)
})


test_that("select", {
    dokeys <- keys(MPO.db)[1:100]
    res <- select(x = MPO.db, keys = dokeys, keytype = "mpid", 
        columns = c("offspring", "term", "doid", "mgi"))
    expect_true("doid" %in% colnames(res))
})

