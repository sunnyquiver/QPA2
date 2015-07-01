LoadPackage("AutoDoc");

make_qpa_version_string := function()
  local commit_id;
  commit_id := "";
  Process( DirectoryCurrent(),
           Filename( DirectoriesSystemPrograms(), "git" ),
           InputTextNone(),
           OutputTextString( commit_id, false ),
           [ "rev-parse", "--verify", "HEAD" ] );
  return Concatenation( "This manual documents the GAP package QPA, ",
                        "version 2.0-dev, revision <URL>",
                        "<Link>https://github.com/oysteins/QPA2/commit/", commit_id, "</Link>",
                        "<LinkText>", commit_id, "</LinkText>",
                        "</URL>" );
end;

AutoDoc( "QPA" : 
         autodoc := rec( section_intros := [ [ "Quivers", "" ],
                                             [ "Path algebras", "" ] ] ),
         scaffold := rec( TitlePage := rec( TitleComment := make_qpa_version_string() ) ),
         maketest := rec( commands := [ "LoadPackage( \"QPA\" );" ] )
);

QUIT;
