#! @Chapter Modules

#! @Section Categories for modules, module elements and module categories

#! @Description
#!  Category for elements of modules over quiver algebras.
DeclareCategory( "IsQuiverModuleElement", IsAlgebraModuleElement and IsObjectWithSide );

#! @Description
#!  Category for modules over quiver algebras.
DeclareCategory( "IsQuiverModule",
                 IsVectorSpace and IsAlgebraModule and IsFieldCategoryObject
                 and IsObjectWithSide
                 and CategoryCollections( IsQuiverModuleElement ) );

#! @Description
#!  Category for module categories.
DeclareCategory( "IsQuiverModuleCategory", IsFieldCategory and IsObjectWithSide );

#! @Description
#!  Category for left module categories.
DeclareCategory( "IsLeftQuiverModuleCategory", IsQuiverModuleCategory );

#! @Description
#!  Category for right module categories.
DeclareCategory( "IsRightQuiverModuleCategory", IsQuiverModuleCategory );

#! @Description
#!  Category for bimodule categories.
DeclareCategory( "IsQuiverBimoduleCategory", IsQuiverModuleCategory );

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
                 IsQuiverModule
                 and CategoryCollections( IsQuiverBimoduleElement ) );

#! @Section Constructing modules

#! @BeginGroup QuiverModule
#! @Description
#!  Construct a (left or right) module over the quiver algebra <A>A</A>.
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments side, A, dimensions, matrices
DeclareOperation( "QuiverModule",
                  [ IsSide, IsQuiverAlgebra, IsDenseList, IsList ] );
#! @Arguments side, A, dimensions, arrows, matrices_for_arrows
DeclareOperation( "QuiverModule",
                  [ IsSide, IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );
#! @EndGroup

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
                  [ IsQuiverAlgebra, IsDenseList, IsList ] );
#! @Arguments A, dimensions, arrows, matrices_for_arrows
DeclareOperation( "LeftQuiverModule",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );
#! @EndGroup

#! @Description
#!  Construct a right module over the quiver algebra <A>A</A>.
#!  This operation works like <Ref Oper="LeftQuiverModule"/>,
#!  except that it creates a right module.
#! @Arguments A, dimensions, matrices
DeclareOperation( "RightQuiverModule",
                  [ IsQuiverAlgebra, IsDenseList, IsList ] );
DeclareOperation( "RightQuiverModule",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );

#! @Description
#!  Construct a right module over the quiver algebra <A>A</A>,
#!  by specifying matrices for certain arrows.
#!  This operation works like <Ref Oper="LeftQuiverModuleByArrows"
#!    Label="for IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList"/>,
#!  except that it creates a right module.
#! @Arguments A, dimensions, matrices
DeclareOperation( "RightQuiverModuleByArrows",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );

DeclareOperation( "ZeroModule", [ IsSide, IsQuiverAlgebra ] );

#! @Description
#!  Returns the left zero module over the algebra <A>A</A>.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A
DeclareAttribute( "LeftZeroModule", IsQuiverAlgebra );

#! @Description
#!  Returns the left zero module over the algebra <A>A</A>.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A
DeclareAttribute( "RightZeroModule", IsQuiverAlgebra );

DeclareOperation( "ZeroBimodule", [ IsDenseList ] );

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
#!  Produces a module based on the representation <A>R</A>.
#!  The argument <A>side</A> should be one of the constants
#!  <C>LEFT</C>, <C>RIGHT</C> or <C>LEFT_RIGHT</C>,
#!  for creating a left module, right module or bimodule, respectively.
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments side, R
DeclareOperation( "AsModule", [ IsSide, IsQuiverRepresentation ] );

#! @Description
#!  The representation <A>R</A> considered as a left module.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments R
DeclareAttribute( "AsLeftQuiverModule", IsQuiverRepresentation );

#! @Description
#!  The representation <A>R</A> considered as a right module.
#! @Returns <Ref Filt="IsRightQuiverModule"/>
#! @Arguments R
DeclareAttribute( "AsRightQuiverModule", IsQuiverRepresentation );

#! @Description
#!  The representation <A>R</A> considered as a bimodule.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments R
DeclareAttribute( "AsBimodule", IsQuiverRepresentation );

#! @BeginGroup QuiverBimodule
#! @Description
#!  Create an <A>A</A>-<A>B</A>-bimodule, where <A>A</A> and <A>B</A> are quiver algebras.
#! @Returns <Ref Filt="IsQuiverBimodule"/>
#! @Arguments A, B, dimensions, matrices
DeclareOperation( "QuiverBimodule", [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsList ] );
#! @Arguments A, B, dimensions, arrows, matrices_for_arrows
DeclareOperation( "QuiverBimodule", [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsDenseList, IsList ] );
#! @Arguments algebras, dimensions, matrices
DeclareOperation( "QuiverBimodule", [ IsDenseList, IsDenseList, IsList ] );
#! @Arguments algebras, dimensions, arrows, matrices_for_arrows
DeclareOperation( "QuiverBimodule", [ IsDenseList, IsDenseList, IsDenseList, IsList ] );
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
DeclareAttribute( "DimensionVector", IsQuiverModule );

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
DeclareOperation( "QuiverModuleElement", [ IsQuiverModule, IsList ] );

#! @Description
#!  Construct an element in the module <A>M</A> by assigning
#!  vectors to certain given vertices.
#!  All vertices of the quiver which are not present in the list
#!  <A>vertices</A> are assigned the zero vector.
#! @Returns <Ref Filt="IsQuiverModuleElement"/>
#! @Arguments M, vertices, vectors
DeclareOperation( "QuiverModuleElement", [ IsQuiverModule, IsDenseList, IsList ] );

#! @Description
#!  Returns the element of the module <A>M</A> corresponding to the
#!  element <A>e</A> in the underlying representation of <A>M</A>.
#! @Returns <Ref Filt="IsQuiverModuleElement"/>
#! @Arguments e, M
DeclareOperation( "AsModuleElement", [ IsQuiverRepresentationElement, IsQuiverModule ] );

#! @Description
#!  Returns the representation element <A>r</A> as a module element.
#!  The argument <A>side</A> determines if the module element should be
#!  in a left module, right module or bimodule.
#! @Returns <Ref Filt="IsQuiverModuleElement"/>
#! @Arguments side, r
DeclareOperation( "AsModuleElement", [ IsSide, IsQuiverRepresentationElement ] );

#! @Description
#!  Returns the representation element <A>r</A> as a left module element.
#! @Returns <Ref Filt="IsLeftQuiverModuleElement"/>
#! @Arguments side, r
DeclareAttribute( "AsLeftModuleElement", IsQuiverRepresentationElement );

#! @Description
#!  Returns the representation element <A>r</A> as a right module element.
#! @Returns <Ref Filt="IsRightQuiverModuleElement"/>
#! @Arguments side, r
DeclareAttribute( "AsRightModuleElement", IsQuiverRepresentationElement );

#! @Description
#!  Returns the representation element <A>r</A> as a bimodule element.
#! @Returns <Ref Filt="IsQuiverBimoduleElement"/>
#! @Arguments side, r
DeclareAttribute( "AsBimoduleElement", IsQuiverRepresentationElement );

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
DeclareOperation( "ModuleCategory", [ IsSide, IsQuiverAlgebra ] );

#!
DeclareAttribute( "LeftModuleCategory", IsQuiverAlgebra );

#!
DeclareAttribute( "RightModuleCategory", IsQuiverAlgebra );

#!
DeclareOperation( "BimoduleCategory", [ IsDenseList ] );

#!
DeclareOperation( "BimoduleCategory", [ IsQuiverAlgebra, IsQuiverAlgebra ] );

#!
DeclareAttribute( "AsDirectModuleCategory", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "AsOppositeModuleCategory", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "AsLeftModuleCategory", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "AsRightModuleCategory", IsQuiverRepresentationCategory );

#!
DeclareOperation( "AsModuleCategory", [ IsSide, IsQuiverRepresentationCategory ] );

#!
DeclareAttribute( "AsBimoduleCategory", IsQuiverRepresentationCategory );

# TODO: remove?
#!
DeclareOperation( "LeftModuleCategoryOverVectorSpaceCategory",
                  [ IsQuiverAlgebra, IsVectorSpaceCategory ] );

#!
DeclareOperation( "RightModuleCategoryOverVectorSpaceCategory",
                  [ IsQuiverAlgebra, IsVectorSpaceCategory ] );

#!
DeclareOperation( "BimoduleCategoryOverVectorSpaceCategory",
                  [ IsQuiverAlgebra, IsQuiverAlgebra, IsVectorSpaceCategory ] );

#!
DeclareAttribute( "AlgebraOfCategory", IsQuiverModuleCategory );

#!
DeclareAttribute( "AlgebrasOfCategory", IsQuiverModuleCategory );

#!
DeclareAttribute( "UnderlyingRepresentationCategory", IsQuiverModuleCategory );

#!
DeclareAttribute( "ModuleType", IsQuiverModuleCategory );

#! @Section Functors between module categories and representation categories

#! @Description
#!  For a module category <A>C</A>
#! @Returns IsCapFunctor
#! @Arguments C
DeclareAttribute( "UnderlyingRepresentationFunctor", IsQuiverModuleCategory );

#! @Description
#!  According to the given argument <A>side</A>, that is, <C>RIGHT</C>, <C>LEFT</C> or 
#!  <C>LEFT_RIGHT</C>, it returns the functor 
#!  <Ref Attr="AsLeftModuleFunctor" Label="for IsQuiverRepresentationCategory"/>,
#!  <Ref Attr="AsRightModuleFunctor" Label="for IsQuiverRepresentationCategory"/>, or
#!  <Ref Attr="AsBimoduleFunctor" Label="for IsQuiverRepresentationCategory"/>, 
#!  respectively. 
#! @Returns IsCapFunctor
#! @Arguments side, C
#!
DeclareOperation( "AsModuleFunctor", [ IsSide, IsQuiverRepresentationCategory ] );

#! @Description
#!  This is a functor from the entered category of representations of a quiver to 
#!  the corresponding category of left modules.
#! @Returns IsCapFunctor
#! @Arguments C
#!
DeclareAttribute( "AsLeftModuleFunctor", IsQuiverRepresentationCategory );

#! @Description
#!  This is a functor from the entered category of representations of a quiver to 
#!  the corresponding category of right modules. 
#! @Returns IsCapFunctor
#! @Arguments C
#!
DeclareAttribute( "AsRightModuleFunctor", IsQuiverRepresentationCategory );

#! @Description
#!  This is a functor from the entered category of representations of a quiver to 
#!  the corresponding category of bimodules, given that the entered category of 
#!  quiver representation is given by a tensor product of two algebras. 
#! @Returns IsCapFunctor
#! @Arguments C
#!
DeclareAttribute( "AsBimoduleFunctor", IsQuiverRepresentationCategory );

#! @Section Operations on modules

#! @Description
#!  Returns the annihilator of a quiver module in the algebra over which 
#!  the module <A>M</A> is defined.
#! @Returns <Ref Filt="IsIdeal"/>
#! @Arguments M
DeclareAttribute( "AnnihilatorOfModule", IsQuiverModule );

#! @Description
#!  Returns the intersection of a finite set of quiver modules which are 
#!  submodules of one given quiver module. 
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments list
DeclareOperation( "IntersectionOfModules", [ IsDenseList ] );


#! @Description
#! Returns the projective cover of the quiver module <A>M</A>. 
#! @Returns IsQuiverModuleHomomorphism
#! @Arguments M
DeclareAttribute( "ProjectiveCover", IsQuiverModule );

#
