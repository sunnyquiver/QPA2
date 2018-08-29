InstallMethod( UnderlyingField,
               [ IsFieldCategoryObject ],
               obj -> UnderlyingField( CapCategory( obj ) ) );

InstallMethod( UnderlyingField,
               [ IsFieldCategoryMorphism ],
               m -> UnderlyingField( CapCategory( m ) ) );
