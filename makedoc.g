LoadPackage("AutoDoc");

AutoDoc( "QPA" : 
         autodoc := true,
         scaffold := true,
         maketest := rec( commands := [ "LoadPackage( \"QPA\" );" ] )
);

QUIT;
