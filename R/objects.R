createAnnDbBimaps <- getFromNamespace('createAnnDbBimaps','AnnotationDbi')
createMAPCOUNTS <- getFromNamespace('createMAPCOUNTS','AnnotationDbi')
prefixAnnObjNames <- getFromNamespace('prefixAnnObjNames','AnnotationDbi')


MPO_DB_AnnDbBimap_seeds <- list(
    list(
        objName="PARENTS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_parent",
                Lcolname="mpid",
                Rcolname="parent"
            )
        )
    ),

    list(
        objName="CHILDREN",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_children",
                Lcolname="mpid",
                Rcolname="children"
            )
        )
    ),

    list(
        objName="ANCESTOR",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_ancestor",
                Lcolname="mpid",
                Rcolname="ancestor"
            )
        )
    ),

    list(
        objName="OFFSPRING",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_offspring",
                Lcolname="mpid",
                Rcolname="offspring"
            )
        )
    ),

    list(
        objName="TERM",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_term",
                Lcolname="mpid",
                Rcolname="term"
            )
        )
    ),
    list(
        objName="ALIAS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_alias",
                Lcolname="mpid",
                Rcolname="alias"
            )
        )
    ),
    list(
        objName="SYNONYM",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_synonym",
                Lcolname="mpid",
                Rcolname="synonym"
            )
        )
    ),

    list(
        objName="ANATOMY",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_anatomy",
                Lcolname="mpid",
                Rcolname="anatomy"
            )
        )
    ),

    list(
        objName="MPMGI",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_mgi",
                Lcolname="mpid",
                Rcolname="mgi"
            )
        )
    ),

    list(
        objName="MPDO",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mp_doid",
                Lcolname="mpid",
                Rcolname="doid"
            )
        )
    ),
    list(
        objName="MGIDO",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="mgi_doid",
                Lcolname="mgi",
                Rcolname="doid"
            )
        )
    ),

    list(
        objName="metadata",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="metadata",
                Lcolname="name",
                Rcolname="value"
            )
        )
    )
)

createAnnObjs.MPO_DB <- function(prefix, objTarget, dbconn, datacache)
{
    #Now skip here
    #checkDBSCHEMA(dbconn, "DO_DB")

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    ann_objs <- createAnnDbBimaps(MPO_DB_AnnDbBimap_seeds, seed0)
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)
    prefixAnnObjNames(ann_objs, prefix)
}




