LoadPackage("AutoDoc");

make_qpa_version_string := function()
  local fstr, str;
  str := "";
  fstr := Concatenation( "This manual documents the GAP package QPA, ",
                         "version 2.0-dev, revision <URL>",
                         "<Link>https://github.com/oysteins/QPA2/commit/%H</Link>",
                         "<LinkText>%h</LinkText>",
                         "</URL>, %ci." );
  Process( DirectoryCurrent(),
           Filename( DirectoriesSystemPrograms(), "git" ),
           InputTextNone(),
           OutputTextString( str, false ),
           [ "show", "-s", Concatenation( "--format=format:", fstr ), "HEAD" ] );
  return str;
end;

latex_preamble := "\\usepackage{amsmath}\n";

AutoDoc( rec(
         gapdoc := rec( LaTeXOptions := rec( LateExtraPreamble := latex_preamble ) ),
         autodoc := rec( section_intros := [ [ "Quivers", "" ],
                                             [ "Quiver algebras", "" ],
                                             [ "Modules", "" ],
                                             [ "Representations", "" ],
                                             [ "Ideals", "" ],
                                             [ "Vector spaces", "" ],
                                             [ "Hom spaces", "Field categories", "" ],
                                             [ "Hom spaces", "Hom spaces", "" ],
                                             [ "Hom spaces", "Hom spaces of vector spaces", "" ],
                                             [ "Hom spaces", "Hom spaces of representations", "" ],
                                             [ "Hom spaces", "Hom spaces of modules", "" ],
                                             [ "Hom spaces", "Morphisms of hom spaces", "" ],
                                             [ "Hom spaces", "Hom modules", "" ],
                                             [ "Hom spaces", "Hom functors", "" ],
                                             [ "Functors", "" ],
                                             [ "Special quivers, algebras and modules", "" ],
                                             ] ),
         scaffold := rec( TitlePage := rec( TitleComment := make_qpa_version_string() ) ),
         maketest := rec( commands := [ "LoadPackage( \"QPA\" );" ] )
));

QUIT;
