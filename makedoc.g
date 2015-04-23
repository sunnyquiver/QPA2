LoadPackage("AutoDoc");

AutoDoc( "QPA" : 
         autodoc := rec( section_intros := [ [ "Quivers", "" ],
                                             [ "Path algebras", "" ] ] ),
         scaffold := true,
         maketest := rec( commands := [ "LoadPackage( \"QPA\" );" ] )
);

QUIT;
