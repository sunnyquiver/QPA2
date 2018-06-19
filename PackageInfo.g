SetPackageInfo( rec(
PackageName := "QPA",
Subtitle := "Quivers and Path Algebras",
Version := "2.0-dev",
Date := "19/06/2018",

PackageWWWHome := "http://www.math.ntnu.no/~oyvinso/QPA/",
ArchiveURL := Concatenation( ~.PackageWWWHome, "qpa-", ~.Version ),
ArchiveFormats := ".tar.gz",

Persons := [
  rec( 
    LastName      := "Skartsæterhagen",
    FirstNames    := "Øystein",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "oysteini@math.ntnu.no",
    WWWHome       := "http://www.math.ntnu.no/~oysteini/",
    Place         := "Trondheim",
    Institution   := "Norges teknisk-naturvitenskapelige universitet"
  ),
],

Status := "dev",

README_URL := 
  Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL := 
  Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),

AbstractHTML := 
  "The <span class=\"pkgname\">QPA</span> package provides data structures \
   and algorithms for doing computations with finite dimensional quotients \
   of path algebras, and finitely generated modules over such algebras. The \
   current version of the QPA package has data structures for quivers, \
   quotients of path algebras, and modules, homomorphisms and complexes of \
   modules over quotients of path algebras.",

PackageDoc := rec(
  BookName  := "QPA",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Quivers and Path Algebras",
),

Dependencies := rec(
  GAP := "4.5",
  NeededOtherPackages := [["CAP", ">=2016.05.30"]],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.tst",

Keywords := ["quiver", "path algebra"]

));
