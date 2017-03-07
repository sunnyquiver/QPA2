#! @Chapter Modules

#! @Section Categories for modules, module elements and module categories

#! @Description
#!  Category for elements of modules over quiver algebras.
DeclareCategory( "IsQuiverModuleElement", IsAlgebraModuleElement );

#! @Description
#!  Category for modules over quiver algebras.
DeclareCategory( "IsQuiverModule",
                 IsVectorSpace and IsAlgebraModule and IsCapCategoryObject
                 and CategoryCollections( IsQuiverModuleElement ) );

#! @Description
#!  Category for module categories.
DeclareCategory( "IsQuiverModuleCategory", IsCapCategory );

#! @Description
#!  Category for elements of left modules over quiver algebras.
DeclareCategory( "IsLeftQuiverModuleElement",
                 IsQuiverModuleElement and IsLeftAlgebraModuleElement );

#! @Description
#!  Category for left modules over quiver algebras.
DeclareCategory( "IsLeftQuiverModule",
                 IsQuiverModule and IsLeftAlgebraModule
                 and CategoryCollections( IsLeftQuiverModuleElement ) );

#! @Description
#!  Category for elements of right modules over quiver algebras.
DeclareCategory( "IsRightQuiverModuleElement",
                 IsQuiverModuleElement and IsRightAlgebraModuleElement );

#! @Description
#!  Category for right modules over quiver algebras.
DeclareCategory( "IsRightQuiverModule",
                 IsQuiverModule and IsLeftAlgebraModule
                 and CategoryCollections( IsRightQuiverModuleElement ) );

#! @Description
#!  Category for elements of bimodules over quiver algebras.
DeclareCategory( "IsQuiverBimoduleElement",
                 IsLeftQuiverModuleElement and IsRightQuiverModuleElement );

#! @Description
#!  Category for bimodules over quiver algebras.
DeclareCategory( "IsQuiverBimodule",
                 IsLeftQuiverModule and IsRightQuiverModule
                 and CategoryCollections( IsQuiverBimoduleElement ) );

#! @Section Constructing modules

#! @BeginGroup LeftQuiverModule
#! @Description
#!  Construct a left module over the quiver algebra <A>A</A>.
#!  The argument <A>dimensions</A> is a list of positive integers
#!  giving the dimension of the vector space in each vertex.
#!  The argument <A>matrices</A> is a list of matrices describing
#!  the maps corresponding to the arrows.
#!  If the quiver of the algebra <A>A</A> is left-oriented,
#!  then the maps go in the same directions as the arrows.
#!  If the quiver is right-oriented, then the maps go in the opposite
#!  directions of the arrows
#!  (more precisely, the module is given by a representation over the
#!  opposite algebra, as described below).
#!  <P/>
#!  This operation first constructs a quiver representation <C>R</C>
#!  (see the <Ref Oper="QuiverRepresentation"/> constructor)
#!  and then constructs a left module using <C>R</C> as its
#!  <Ref Attr="UnderlyingRepresentation" Label="for IsQuiverModule"/>.
#!  <P/>
#!  If the quiver of the algebra <A>A</A> is left-oriented,
#!  then representations over <A>A</A> naturally correspond to left modules;
#!  if the quiver is right-oriented,
#!  then representations over <A>A</A> naturally correspond to right modules.
#!  The underlying representation <C>R</C> must therefore be over
#!  <A>A</A> if the quiver is left-oriented, and over the opposite algebra
#!  of <A>A</A> if the quiver is right-oriented.
#!  The appropriate algebra (<A>A</A> or its opposite) can be obtained
#!  by calling <Ref Attr="AlgebraForLeftModules" Label="for IsQuiverAlgebra"/>.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A, dimensions, matrices
DeclareOperation( "LeftQuiverModule",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );
#! @EndGroup

#! @Description
#!  Construct a left module over the quiver algebra <A>A</A>,
#!  by specifying matrices for certain arrows.
#!  <P/>
#!  This works like <Ref Oper="LeftQuiverModule"/>, except that
#!  the entries in the list <A>matrices</A> correspond to the arrows
#!  in the list <A>arrows</A>.  All arrows of the quiver that are not present in
#!  this list get zero maps.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A, dimensions, arrows, matrices
DeclareOperation( "LeftQuiverModuleByArrows",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );

#! @Description
#!  Returns the left zero module over the algebra <A>A</A>.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A
DeclareAttribute( "LeftZeroModule", IsQuiverAlgebra );

#! @Description
#!  Construct a right module over the quiver algebra <A>A</A>.
#!  This operation works like <Ref Oper="LeftQuiverModule"/>,
#!  except that it creates a right module.
#! @Arguments A, dimensions, matrices
DeclareOperation( "RightQuiverModule",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );

#! @Description
#!  Construct a right module over the quiver algebra <A>A</A>,
#!  by specifying matrices for certain arrows.
#!  This operation works like <Ref Oper="LeftQuiverModuleByArrows"
#!    Label="for IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList"/>,
#!  except that it creates a right module.
#! @Arguments A, dimensions, matrices
DeclareOperation( "RightQuiverModuleByArrows",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );

#! @Description
#!  Returns the left zero module over the algebra <A>A</A>.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A
DeclareAttribute( "RightZeroModule", IsQuiverAlgebra );

#! @Description
#!  Returns the appropriate algebra to use for representations of left
#!  modules over the algebra <A>A</A>.
#!  This is the algebra <A>A</A> itself if its quiver is left-oriented,
#!  and the opposite algebra of <A>A</A> if its quiver is right-oriented.
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Arguments A
DeclareAttribute( "AlgebraForLeftModules", IsQuiverAlgebra );

#! @Description
#!  Returns the appropriate algebra to use for representations of right
#!  modules over the algebra <A>A</A>.
#!  This is the algebra <A>A</A> itself if its quiver is right-oriented,
#!  and the opposite algebra of <A>A</A> if its quiver is left-oriented.
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Arguments A
DeclareAttribute( "AlgebraForRightModules", IsQuiverAlgebra );

#! @Description
#!  Returns the appropriate algebra to use for representations of <A>A</A>-<A>B</A>-bimodules.
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Arguments A, B
DeclareOperation( "AlgebraForBimodules", [ IsQuiverAlgebra, IsQuiverAlgebra ] );

#! @Description
#!  The representation <A>R</A> considered as a (left or right) module
#!  over the algebra <A>A</A>.
#!  The algebra <A>A</A> must either be the algebra of the representation <A>R</A>,
#!  or the opposite of this algebra.
#!  This, together with the orientation of the algebra's quiver,
#!  determines whether the resulting module is a left module or a right module.
#!  Giving any other algebra than the algebra of <A>R</A> or its opposite
#!  as the argument <A>A</A> results in an error.
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments R, A
DeclareOperation( "AsModule", [ IsQuiverRepresentation, IsQuiverAlgebra ] );

#! @Description
#!  The representation <A>R</A> considered as a left module.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments R
DeclareOperation( "AsLeftModule", [ IsQuiverRepresentation ] );

#! @Description
#!  The representation <A>R</A> considered as a right module.
#! @Returns <Ref Filt="IsRightQuiverModule"/>
#! @Arguments R
DeclareOperation( "AsRightModule", [ IsQuiverRepresentation ] );

#! @Description
#!  The representation <A>R</A> considered as a bimodule over the algebras
#!  <A>A</A> (left) and <A>B</A> (right).
#!  The algebras <A>A</A> and <A>B</A> must have the same base field $k$,
#!  and the representation <A>R</A> must be a representation over the tensor
#!  algebra $<A>A</A> \otimes_k <A>B</A>^\mathrm{op}$.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments R, A, B
DeclareOperation( "AsBimodule", [ IsQuiverRepresentation, IsQuiverAlgebra, IsQuiverAlgebra ] );

#! @BeginGroup QuiverBimodule
#! @Description
#!  Create an <A>A</A>-<A>B</A>-bimodule, where <A>A</A> and <A>B</A> are quiver algebras.
#! @Returns <Ref Filt="IsQuiverBimodule"/>
#! @Arguments A, B, dimensions, matrices
DeclareOperation( "QuiverBimodule", [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsList ] );
#! @Arguments A, B, dimensions, arrows, matrices_for_arrows
DeclareOperation( "QuiverBimodule", [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsDenseList, IsList ] );
#! @EndGroup


#! @Section Information about a module

#! @Description
#!  Returns the quiver representation for the module <A>M</A>.
#!  All module operations are delegated to the underlying representation.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments M
DeclareAttribute( "UnderlyingRepresentation", IsQuiverModule );

#! @Description
#!  Returns the quiver that the module <A>M</A> represents.
#! @Returns <Ref Filt="IsQuiver"/>
#! @Arguments M
DeclareAttribute( "QuiverOfModule", IsQuiverModule );

#! @Description
#!  Returns the base field that the module <A>M</A> is a vector space over;
#!  that is, the base field of the algebra it is a module over.
#!  This is also available by the builtin GAP attribute
#!  <Ref BookName="Reference" Attr="LeftActingDomain"/>.
#! @Returns <Ref BookName="Reference" Filt="IsField"/>
#! @Arguments M
DeclareAttribute( "FieldOfModule", IsQuiverModule );

#! @Description
#!  Returns the dimensions of the vector spaces in the module <A>M</A>,
#!  as a list.  The entries in the list correspond to the vertices of the quiver.
#! @Returns <Ref BookName="Reference" Filt="IsDenseList"/>
#! @Arguments M
DeclareAttribute( "VertexDimensions", IsQuiverModule );

#! @Description
#!  Returns the dimension of the vector space in the module <A>M</A>
#!  at vertex number <A>i</A>, where <A>i</A> is a positive integer.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments M, i
DeclareOperation( "VertexDimension", [ IsQuiverModule, IsPosInt ] );

#! @Description
#!  Returns the dimension of the vector space in the module <A>M</A>
#!  at the vertex <A>v</A>.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments M, v
DeclareOperation( "VertexDimension", [ IsQuiverModule, IsVertex ] );

#! @Section Constructing elements

#! @Description
#!  Construct the element in the module <A>M</A>
#!  given by the vectors in the list <A>vectors</A>.
#!  Each entry in <A>vectors</A> corresponds to a vertex in the quiver.
#! @Returns <Ref Filt="IsQuiverModuleElement"/>
#! @Arguments M, vectors
DeclareOperation( "QuiverModuleElement", [ IsQuiverModule, IsDenseList ] );

#! @Description
#!  Construct an element in the module <A>M</A> by assigning
#!  vectors to certain given vertices.
#!  All vertices of the quiver which are not present in the list
#!  <A>vertices</A> are assigned the zero vector.
#! @Returns <Ref Filt="IsQuiverModuleElement"/>
#! @Arguments M, vertices, vectors
DeclareOperation( "QuiverModuleElementByVertices", [ IsQuiverModule, IsDenseList, IsDenseList ] );

#! @Description
#!  Returns the element of the module <A>M</A> corresponding to the
#!  element <A>e</A> in the underlying representation of <A>M</A>.
#! @Returns <Ref Filt="IsQuiverModuleElement"/>
#! @Arguments e, M
DeclareOperation( "AsModuleElement", [ IsQuiverRepresentationElement, IsQuiverModule ] );

#! @Section Information about an element

#! @Description
#!  Given an element <A>e</A> in a quiver module <C>M</C>,
#!  this attribute returns the corresponding element in the
#!  underlying representation of <C>M</C>.
#! @Returns <Ref Filt="IsQuiverRepresentationElement"/>
#! @Arguments e
DeclareAttribute( "UnderlyingRepresentationElement", IsQuiverModuleElement );

#! @Description
#!  Returns the quiver module which the element <A>e</A> belongs to.
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments e
DeclareAttribute( "ModuleOfElement", IsQuiverModuleElement );

#! @Description
#!  Returns all the vectors of the element <A>e</A>, as a list ordered
#!  in the same way as the vertices of the quiver.
#! @Returns <Ref BookName="Reference" Filt="IsDenseList"/>
#! @Arguments e
DeclareAttribute( "ElementVectors", IsQuiverModuleElement );

#! @BeginGroup ElementVector
#! @Description
#!  Returns the vector of the element <A>e</A> at the vertex with number <A>i</A>.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments e, i
DeclareOperation( "ElementVector", [ IsQuiverModuleElement, IsPosInt ] );
#! @Arguments e, i
DeclareOperation( "\[\]", [ IsQuiverModuleElement, IsPosInt ] );
#! @EndGroup

#! @Description
#!  Returns the vector of the element <A>e</A> at the vertex <A>v</A>.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments e, v
DeclareOperation( "ElementVector", [ IsQuiverModuleElement, IsVertex ] );

#! @Section Acting on elements

#! @Description
#!  For an element <A>a</A> in a quiver algebra <C>A</C>
#!  and an element <A>e</A> in a left <C>A</C>-module,
#!  the expression <C><A>a</A>^<A>e</A></C> produces the product
#!  of <A>a</A> and <A>e</A>.
#! @Returns <Ref Filt="IsLeftQuiverModuleElement"/>
#! @Arguments a, e
DeclareOperation( "\^", [ IsQuiverAlgebraElement, IsLeftQuiverModuleElement ] );

#! @Description
#!  For an element <A>a</A> in a quiver algebra <C>A</C>
#!  and an element <A>e</A> in a right <C>A</C>-module,
#!  the expression <C><A>e</A>^<A>a</A></C> produces the product
#!  of <A>e</A> and <A>a</A>.
#! @Returns <Ref Filt="IsLeftQuiverModuleElement"/>
#! @Arguments e, a
DeclareOperation( "\^", [ IsRightQuiverModuleElement, IsQuiverAlgebraElement ] );


#! @Section Module categories

#!
DeclareAttribute( "CategoryOfLeftModules", IsQuiverAlgebra );

#!
DeclareAttribute( "CategoryOfRightModules", IsQuiverAlgebra );

#!
DeclareOperation( "CategoryOfBimodules", [ IsQuiverAlgebra, IsQuiverAlgebra ] );

#!
DeclareAttribute( "AsDirectCategoryOfModules", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "AsOppositeCategoryOfModules", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "AsCategoryOfLeftModules", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "AsCategoryOfRightModules", IsQuiverRepresentationCategory );

#!
DeclareOperation( "AsCategoryOfModules", [ IsQuiverRepresentationCategory, IsBool ] );

#!
DeclareAttribute( "AsCategoryOfBimodules", IsQuiverRepresentationCategory );

# TODO: remove?
#!
DeclareOperation( "CategoryOfLeftModulesOverVectorSpaceCategory",
                  [ IsQuiverAlgebra, IsVectorSpaceCategory ] );

#!
DeclareOperation( "CategoryOfRightModulesOverVectorSpaceCategory",
                  [ IsQuiverAlgebra, IsVectorSpaceCategory ] );

#!
DeclareOperation( "CategoryOfBimodulesOverVectorSpaceCategory",
                  [ IsQuiverAlgebra, IsQuiverAlgebra, IsVectorSpaceCategory ] );

#!
DeclareAttribute( "AlgebraOfCategory", IsQuiverModuleCategory );

#!
DeclareAttribute( "UnderlyingRepresentationCategory", IsQuiverModuleCategory );

