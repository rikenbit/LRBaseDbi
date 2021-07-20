library("LRBaseDbi")
library("AnnotationHub")

# Data retrieval from AnnotationHub
ah <- AnnotationHub()
dbfile <- query(ah, c("LRBaseDb", "Sus scrofa", "v001"))[[1]]

# Constructor
LRBase.Ssc.eg.db <- LRBaseDbi::LRBaseDb(dbfile)

# show
LRBase.Ssc.eg.db

# dbconn
expect_true("SQLiteConnection" %in% is(dbconn(LRBase.Ssc.eg.db)))

# dbfile
expect_true(dbfile(LRBase.Ssc.eg.db) != "")

# dbschema
expect_true(all(dbschema(LRBase.Ssc.eg.db) != ""))

# dbInfo
expect_true(all(dim(dbInfo(LRBase.Ssc.eg.db)) != 0))

# species
expect_true(species(LRBase.Ssc.eg.db) != "")

# lrNomenclature
expect_true(lrNomenclature(LRBase.Ssc.eg.db) != "")

# lrListDatabases
expect_true(all(dim(lrListDatabases(LRBase.Ssc.eg.db)) != 0))

# lrVersion
expect_true(all(dim(lrVersion(LRBase.Ssc.eg.db)) != 0))

# columns
cols <- c("GENEID_L", "GENEID_R", "SOURCEDB", "SOURCEID")
expect_identical(sort(columns(LRBase.Ssc.eg.db)), sort(cols))

# keytypes
kts <- cols
expect_identical(sort(keytypes(LRBase.Ssc.eg.db)), sort(kts))

# keys
ks <- keys(LRBase.Ssc.eg.db, keytype="GENEID_L")[seq(10)]
expect_equal(
	length(ks),
	10)

# select
out <- select(LRBase.Ssc.eg.db,
	columns=cols,
	keys=ks,
	keytype="GENEID_L"
	)
expect_true(all(dim(out) != 0))
