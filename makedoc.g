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

AutoDoc( "QPA" : 
         autodoc := rec( section_intros := [ [ "Quivers", "" ],
                                             [ "Quiver algebras", "" ],
                                             [ "Modules", "" ],
                                             [ "Representations", "" ],
                                             [ "Vector spaces", "" ],
                                             ] ),
         scaffold := rec( TitlePage := rec( TitleComment := make_qpa_version_string() ) ),
         maketest := rec( commands := [ "LoadPackage( \"QPA\" );" ] )
);

QUIT;
