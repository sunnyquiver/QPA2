#! @Chapter Decomposition of objects

#! @Section Decomposition for modules and representations
#! All operations described in this section are given for both modules 
#! and represenations of quiver algebras, where the description of them
#! only are given for modules.  The description of the operations for 
#! representations is similar.

#! @BeginGroup
#! @Description
#!  Returns a set of modules <M>\{ M_1,\ldots, M_t\}</M> such that 
#!  <M>M \simeq M_1\oplus \cdots \oplus M_t</M>, where each <M>M_i</M> is
#!  isomorphic to <M>X^{n_i}_i</M> for some indecomposable module  
#!  <M>X_i</M> and positive integer <M>n_i</M> for all <M>i</M>, where 
#!  <M>X_i \not\simeq X_j</M> for <M>i \neq j</M>.  
#! @Returns a list
#! @Arguments M
DeclareOperation( "BlockDecompositionOfModule", [ IsQuiverModule ] ); 
#! @Arguments R
DeclareOperation( "BlockDecompositionOfRepresentation", [ IsQuiverRepresentation ] ); 
#! @EndGroup

#! @BeginGroup
#! @Description
#!   Returns a set <M>\{ e_1,\ldots,e_t\}</M> of idempotents in the endomorphism of <A>M</A> 
#!   such that <M>M \simeq \Im e_1 \oplus \cdots \oplus \Im e_t</M>, where each <M>\Im e_i</M> 
#!   is isomorphic to <M>X^{n_i}_i</M> for some representation <M>X_i</M> and positive integer
#!   <M>n_i</M> for all <M>i</M>.
#! @Returns a list
#! @Arguments M
DeclareOperation( "BlockSplittingIdempotents", [ IsQuiverModule ] ); 
#! @Arguments R
DeclareOperation( "BlockSplittingIdempotents", [ IsQuiverRepresentation ] ); 
#! @EndGroup

#! @BeginGroup
#! @Description
#!   Returns a list of four modules <M>[ X, U, X, V ]</M>, where <M>X</M> 
#!   is one common non-zero direct summand of <A>M1</A> and <A>M2</A>, the sum of 
#!   <M>X</M> and <M>U</M> is <A>M1</A> and the sum of <M>X</M> and <M>V</M> is 
#!   <A>M2</A>, if such a non-zero direct summand exists. Otherwise it returns false.
#! @Returns a list of four quiver modules
#! @Arguments M1, M2
DeclareOperation( "CommonDirectSummand", [ IsQuiverModule, IsQuiverModule ] );
#! @Arguments R1, R2
DeclareOperation( "CommonDirectSummand", [ IsQuiverRepresentation, IsQuiverRepresentation ] ); 
#! @EndGroup

#! @BeginGroup
#! @Description
#!   Given a module  <A>M</A>  this function computes a list of 
#!   modules  <M>L</M>  such that  <A>M</A>  is isomorphic to the direct 
#!   sum of the modules on the list <M>L</M>. 
#! @Returns a list of modules
#! @Arguments M
DeclareOperation( "DecomposeModule", [ IsQuiverModule ] );
#! @Arguments R
DeclareOperation( "DecomposeRepresentation", [ IsQuiverRepresentation ] );
#! @EndGroup


#! @BeginGroup
#! @Description
#!   Returns a list of length two, where the first entry is a list of all indecomposable 
#!   non-isomorphic direct summands of <A>M</A> and the second entry is the list of the 
#!   multiplicities of these direct summands in the module <A>M</A>.
#! @Returns a list
#! @Arguments M
DeclareAttribute( "DecomposeModuleWithMultiplicities", IsQuiverModule );
#! @Arguments R
DeclareAttribute( "DecomposeRepresentationWithMultiplicities", IsQuiverRepresentation );
#! @EndGroup

#! @BeginGroup
#! @Description
#!  This function returns true if the module  <A>M1</A>  is isomorphic to a direct 
#!  of the module  <A>M2</A>, an error message is issued if  <A>M1</A>  and  <A>M2</A>  are 
#!  not modules over over the same algebra.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments M
DeclareOperation( "IsDirectSummand", [ IsQuiverModule, IsQuiverModule ] );
#! @Arguments R
DeclareOperation( "IsDirectSummand", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @EndGroup

#! @BeginGroup
#! @Description
#!  If the entered module  <A>M</A>  is an indecomposable module over an 
#!  algebra over a finite field, then the function returns true.  If the 
#!  field is not finite, the function makes some easy checks ensuring 
#!  that the module is indecomposable.  If they fail, it searches for 
#!  other methods to apply.
#! @Arguments M
DeclareProperty( "IsIndecomposableModule",  IsQuiverModule );
#! @Arguments R
DeclareProperty( "IsIndecomposableRepresentation",  IsQuiverRepresentation );
#! @EndGroup

InstallTrueMethod( IsIndecomposableModule, IsSimpleQuiverModule );
InstallTrueMethod( IsIndecomposableRepresentation, IsSimpleRepresentation );

#! @BeginGroup
#! @Description
#!   Returns a list of three modules <M>[ X, U, V ]</M>, where <M>X</M> 
#!   is a maximal common non-zero direct summand of <A>M1</A> and <A>M2</A>, the
#!   sum of <M>X</M> and <M>U</M> is <A>M1</A> and the sum of <M>X</M> and <M>V</M>
#!   is <A>M2</A>, if such a non-zero maximal direct summand exists. Otherwise it 
#!   returns false.
#! @Returns a list of three quiver represenations
#! @Arguments M1, M2
DeclareOperation( "MaximalCommonDirectSummand", [ IsQuiverModule, IsQuiverModule ] ); 
#! @Arguments R1, R2
DeclareOperation( "MaximalCommonDirectSummand", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @EndGroup

#