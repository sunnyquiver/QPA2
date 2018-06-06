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

AutoDoc( "QPA" : 
         autodoc := rec( section_intros := [ [ "Quivers", "" ],
                                             [ "Quiver algebras", "" ],
                                             [ "Modules", "" ],
                                             [ "Representations", "" ],
                                             [ "Ideals", "" ],
                                             [ "Vector spaces", "" ],
                                             [ "Functors", "" ],
                                             [ "Special quivers, algebras and modules", "" ],
                                             ] ),
         scaffold := rec( TitlePage := rec( TitleComment := make_qpa_version_string() ),
                          gapdoc_latex_options := rec( LateExtraPreamble := latex_preamble ) ),
         maketest := rec( commands := [ "LoadPackage( \"QPA\" );" ] )
);

QUIT;
