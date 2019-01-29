#! @Chapter Representations

#! @Section Categories for representations and their elements

#! @Description
#!  Category for elements of a quiver representation.
DeclareCategory( "IsQuiverRepresentationElement", IsVector );

#! @Description
#!  Category for quiver representations.
DeclareCategory( "IsQuiverRepresentation",
                 IsVectorSpace and
                 IsFieldCategoryObject and
                 CategoryCollections( IsQuiverRepresentationElement ) );


#! @Section Categories of representations

#!
DeclareCategory( "IsQuiverRepresentationCategory", IsFieldCategory );

#!
DeclareAttribute( "AlgebraOfCategory", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "VectorSpaceCategory", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "CategoryOfQuiverRepresentations", IsQuiverAlgebra );

#!
DeclareAttribute( "VectorSpaceConstructor", IsQuiverRepresentationCategory );

#!
DeclareAttribute( "LinearTransformationConstructor", IsQuiverRepresentationCategory );

#!
DeclareOperation( "CategoryOfQuiverRepresentationsOverVectorSpaceCategory",
                  [ IsQuiverAlgebra, IsAbelianCategory ] );


#! @Section Constructing elements

#! @BeginGroup QuiverRepresentationElement
#! @Returns <Ref Filt="IsQuiverRepresentationElement"/>
#! @Description
#!  Construct the element in the quiver representation <A>R</A>
#!  given by the vectors in the list <A>vectors</A>.
#!  Each entry in <A>vectors</A> corresponds to a vertex in the quiver.
#!  <P/>
#!  The variant <C>QuiverRepresentationElementNC</C> does not check
#!  that the contents of the list <A>vectors</A> is appropriate for an
#!  element in the representation <A>R</A>.
#! @Arguments R, vectors
DeclareOperation( "QuiverRepresentationElement",
                  [ IsQuiverRepresentation, IsList ] );
#! @Arguments R, vectors
DeclareOperation( "QuiverRepresentationElementNC",
                  [ IsQuiverRepresentation, IsDenseList ] );
#! @EndGroup

#! @Description
#!  Construct an element in the representation <A>R</A> by assigning
#!  vectors to certain given vertices.
#!  All vertices of the quiver which are not present in the list
#!  <A>vertices</A> are assigned the zero vector.
#! @Returns <Ref Filt="IsQuiverRepresentationElement"/>
#! @Arguments R, vertices, vectors
DeclareOperation( "QuiverRepresentationElement",
                  [ IsQuiverRepresentation, IsDenseList, IsList ] );

#! @Arguments R, v
#! @Description
#!  Create a representation element from a vector.
DeclareOperation( "QuiverRepresentationElement",
                  [ IsQuiverRepresentation, IsQPAVector ] );

#! @Section Information about an element

#! @Description
#!  Returns the quiver representation which the element <A>e</A> belongs to.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments e
DeclareAttribute( "RepresentationOfElement", IsQuiverRepresentationElement );

#! @Description
#!  Returns all the vectors of the element <A>e</A>, as a list ordered
#!  in the same way as the vertices of the quiver.
#! @Returns <Ref BookName="Reference" Filt="IsDenseList"/>
#! @Arguments e
DeclareAttribute( "ElementVectors", IsQuiverRepresentationElement );

#! @Description
#!  Returns the vector of the element <A>e</A> at the vertex with number <A>i</A>.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments e, i
DeclareOperation( "ElementVector", [ IsQuiverRepresentationElement, IsPosInt ] );

#! @Description
#!  Returns the vector of the element <A>e</A> at the vertex <A>v</A>.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments e, v
DeclareOperation( "ElementVector", [ IsQuiverRepresentationElement, IsVertex ] );

#! @Arguments e
#! @Description
#!  The representation element <A>e</A> considered as a vector.
DeclareAttribute( "AsVector", IsQuiverRepresentationElement );

#! @Section Acting on elements

#! @Description
#!  Produces the element which is the result of letting the path <A>p</A>
#!  act on the element <A>e</A>.
#! @Returns <Ref Filt="IsQuiverRepresentationElement"/>
#! @Arguments e, p
DeclareOperation( "PathAction", [ IsQuiverRepresentationElement, IsPath ] );

#! @Description
#!  Produces the element which is the result of letting the
#!  algebra element <A>ae</A> act on the representation element <A>re</A>.
#! @Returns <Ref Filt="IsQuiverRepresentationElement"/>
#! @Arguments re, ae
DeclareOperation( "QuiverAlgebraAction", [ IsQuiverRepresentationElement, IsQuiverAlgebraElement ] );

#! @Arguments R, a
#! @Description
#!  Given a quiver representation <A>R</A>, and an algebra element <A>a</A>,
#!  produces the linear transformation on <C>V</C> given by the action of <A>a</A> on <A>R</A>,
#!  where <C>V</C> is the underlying vector space of <A>R</A>.
DeclareOperation( "QuiverAlgebraActionAsLinearTransformation",
                  [ IsQuiverRepresentation, IsQuiverAlgebraElement ] );

#! @Section Constructing representations

#! @BeginGroup QuiverRepresentation
#! @Description
#!  Construct a quiver representation over the quiver algebra <A>A</A>.
#!
#!  The list <A>dimensions</A> gives the dimension of the vector space
#!  in each vertex.
#!  The list <A>matrices</A> gives the matrices for the linear maps
#!  corresponding to the arrows.
#!
#!  In the second form of the constructor, a list <A>arrows</A> of some
#!  arrows from the quiver is supplied, and the list <A>matrices_for_arrows</A>
#!  gives the matrices for these arrows, in the same order.
#!  For all arrows that are not in the list <A>arrows</A>, the
#!  maps in the representation will be zero maps.
#!
#!  In both forms, the list of matrices may contain fewer entries than
#!  the number of arrows.  Any unbound entry in the list of matrices
#!  gives a zero map in the representation.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments A, dimensions, matrices
DeclareOperation( "QuiverRepresentation", [ IsQuiverAlgebra, IsDenseList, IsList ] );
#! @Arguments A, dimensions, arrows, matrices_for_arrows
DeclareOperation( "QuiverRepresentation", [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsList ] );
#! @EndGroup

#! @BeginExampleSession
#! gap> kQ := PathAlgebra( Rationals, LeftQuiver( "Q(3)[a:1->2,b:2->3]" ) );;
#! gap> R := QuiverRepresentation( kQ, [ 1, 2, 1 ], [ [ 2, 5 ], [ 1, 0 ] ] );
#! <1,2,1>
#! @EndExampleSession

#! @BeginGroup QuiverRepresentation-cat
#! @Description
#!  Construct a quiver representation in the category <A>cat</A>.
#!
#!  The list <A>objects</A> gives the object for each vertex.
#!  The list <A>morphisms</A> gives the morphisms
#!  corresponding to the arrows.
#!
#!  In the second form of the constructor, a list <A>arrows</A> of some
#!  arrows from the quiver is supplied, and the list <A>morphisms_for_arrows</A>
#!  gives the morphisms for these arrows, in the same order.
#!  For all arrows that are not in the list <A>arrows</A>, the
#!  morphisms in the representation will be zero morphisms.
#!
#!  In both forms, the list of matrices may contain fewer entries than
#!  the number of arrows.  Any unbound entry in the list of matrices
#!  gives a zero morphism in the representation.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments cat, objects, morphisms
DeclareOperation( "QuiverRepresentation", [ IsQuiverRepresentationCategory, IsDenseList, IsList ] );
#! @Arguments cat, objects, arrows, morphisms_for_arrows
DeclareOperation( "QuiverRepresentation", [ IsQuiverRepresentationCategory,
                                            IsDenseList, IsDenseList, IsList ] );
#! @EndGroup

#! @Description
#!  Non-checking variant of QuiverRepresentation.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments cat, objects, morphisms
DeclareOperation( "QuiverRepresentationNC", [ IsQuiverRepresentationCategory, IsDenseList, IsList ] );

#! @Description
#!  Given an algebra $<A>A</A> = kQ/I$ and a representation <A>R</A> over
#!  the path algebra $kQ$ which respects the relations $I$, this operation
#!  produces the corresponding representation over the algebra <A>A</A>.
#!  If the representation <A>R</A> does not respect the relations, then
#!  an error is signalled.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments R, A
DeclareOperation( "AsRepresentationOfQuotientAlgebra",
                  [ IsQuiverRepresentation, IsQuotientOfPathAlgebra ] );

#! @Description
#!  If <A>R</A> is a representation over a quotient $kQ/I$ of a path algebra,
#!  then this attribute gives <A>R</A> as a representation over the
#!  path algebra $kQ$.  If <A>R</A> is a representation over a path algebra,
#!  then the value of the attribute is just <A>R</A> itself.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments R
DeclareAttribute( "AsRepresentationOfPathAlgebra", IsQuiverRepresentation );

#! @Arguments R
#! @Description
#!  The quiver representation <A>R</A> considered as a vector space.
DeclareAttribute( "AsQPAVectorSpace", IsQuiverRepresentation );

#!
DeclareAttribute( "UnderlyingCategoryForRepresentations", IsQuiverAlgebra );

#! @Description
#!  Produces the zero representation over the quiver algebra <A>A</A>.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments A
DeclareAttribute( "ZeroRepresentation", IsQuiverAlgebra );

#! @Description
#!  Returns the algebra that the representation <A>R</A> is a representation over.
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Arguments R
DeclareAttribute( "AlgebraOfRepresentation", IsQuiverRepresentation );

#! @Description
#!  Returns the quiver that the representation <A>R</A> is a representation over.
#!  Calling <C>QuiverOfRepresentation( R )</C> is equivalent to calling
#!  <C>QuiverOfAlgebra( AlgebraOfRepresentation( R ) )</C>.
#! @Returns <Ref Filt="IsQuiver"/>
#! @Arguments R
DeclareAttribute( "QuiverOfRepresentation", IsQuiverRepresentation );

#! @Description
#!  Returns the field that the representation <A>R</A> is a vector space over.
#!  This is the same as <C>LeftActingDomain( AlgebraOfRepresentation( R ) )</C>.
#! @Returns <Ref BookName="Reference" Filt="IsField"/>
#! @Arguments R
DeclareAttribute( "FieldOfRepresentation", IsQuiverRepresentation );

#! @Description
#!  Returns the dimensions of the vector spaces in the representation <A>R</A>,
#!  as a list.  The entries in the list correspond to the vertices of the quiver.
#! @Returns <Ref BookName="Reference" Filt="IsDenseList"/>
#! @Arguments R
DeclareAttribute( "DimensionVector", IsQuiverRepresentation );

#!
DeclareAttribute( "VectorSpacesOfRepresentation", IsQuiverRepresentation );

#!
DeclareOperation( "VectorSpaceOfRepresentation", [ IsQuiverRepresentation, IsPosInt ] );

#!
DeclareOperation( "VectorSpaceOfRepresentation", [ IsQuiverRepresentation, IsVertex ] );

#!
DeclareAttribute( "MapsOfRepresentation", IsQuiverRepresentation );

#! @Description
#!  Returns the dimension of the vector space in the representation <A>R</A>
#!  at vertex number <A>i</A>, where <A>i</A> is a positive integer.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments R, i
DeclareOperation( "VertexDimension", [ IsQuiverRepresentation, IsPosInt ] );

#! @Description
#!  Returns the dimension of the vector space in the representation <A>R</A>
#!  at the vertex <A>v</A>.
#! @Returns <Ref BookName="Reference" Filt="IsVector"/>
#! @Arguments R, v
DeclareOperation( "VertexDimension", [ IsQuiverRepresentation, IsVertex ] );

#! @Description
#!  Returns a list of the matrices for the maps in the representation <A>R</A>.
#!  The list is ordered in the same way as the arrows of the quiver.
#! @Returns <Ref BookName="Reference" Filt="IsDenseList"/>
#! @Arguments R
DeclareAttribute( "MatricesOfRepresentation", IsQuiverRepresentation );

#! @Description
#!  Returns the matrix for the map in the representation <A>R</A>
#!  corresponding to arrow number <A>i</A>, where <A>i</A> is a positive integer.
#! @Returns <Ref BookName="Reference" Filt="IsMatrix"/>
#! @Arguments R, i
DeclareOperation( "MapForArrow", [ IsQuiverRepresentation, IsPosInt ] );

#! @Description
#!  Returns the matrix for the map in the representation <A>R</A>
#!  corresponding to the arrow <A>a</A>.
#! @Returns <Ref BookName="Reference" Filt="IsMatrix"/>
#! @Arguments R, a
DeclareOperation( "MapForArrow", [ IsQuiverRepresentation, IsArrow ] );

#! @Description
#!  Returns the matrix for the map in the representation <A>R</A>
#!  corresponding to the path <A>p</A>, that is, the composition of
#!  the maps corresponding to the arrows in <A>p</A>.
#! @Returns <Ref BookName="Reference" Filt="IsMatrix"/>
#! @Arguments R, p
DeclareOperation( "MapForPath", [ IsQuiverRepresentation, IsPath ] );

#! @Description
#!  Returns the matrix for the map in the representation <A>R</A>
#!  corresponding to the homogeneous algebra element <A>e</A>.
#! @Returns <Ref BookName="Reference" Filt="IsMatrix"/>
#! @Arguments R, e
DeclareOperation( "MapForAlgebraElement", [ IsQuiverRepresentation, IsQuiverAlgebraElement ] );

#! @Section Homomorphisms of representations

#!
DeclareCategory( "IsQuiverRepresentationHomomorphism",
                 IsMapping and IsSPGeneralMapping and IsVectorSpaceHomomorphism
                 and IsFieldCategoryMorphism and IsQPAVector );

#!
DeclareOperation( "QuiverRepresentationHomomorphism",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsList ] );

#!
DeclareOperation( "QuiverRepresentationHomomorphismNC",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsDenseList ] );

#!
DeclareAttribute( "MapsOfRepresentationHomomorphism",
                  IsQuiverRepresentationHomomorphism );

#!
DeclareAttribute( "MatricesOfRepresentationHomomorphism",
                  IsQuiverRepresentationHomomorphism );

#!
DeclareOperation( "MapForVertex",
                  [ IsQuiverRepresentationHomomorphism, IsVertex ] );

#!
DeclareOperation( "MapForVertex",
                  [ IsQuiverRepresentationHomomorphism, IsPosInt ] );

#! @Arguments f
#! @Returns <Ref Filt="IsLinearTransformation"/>
#! @Description
#!  Given a homomorphism <A>f</A> of quiver representations,
#!  returns the corresponding linear transformation on the underlying vector spaces.
DeclareAttribute( "AsLinearTransformation", IsQuiverRepresentationHomomorphism );

# DeclareOperation( "ImageElm",
#                   [ IsQuiverRepresentationHomomorphism,
#                     IsQuiverRepresentationElement ] );

# DeclareOperation( "MatrixVectorMultiplication", [ IsQuiver ] );



#!
DeclareOperation( "Transpose", [ IsDenseList ] );

#! @Section Subrepresentations

#! @Description
#!  Returns the inclusion from the subrepresentation of <A>R</A> generated by
#!  the elements <A>gens</A> to <A>R</A>.  The function signals an error if
#!  not all elements in <A>gens</A> are in <A>R</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R, gens
DeclareOperation( "SubrepresentationInclusion",
                  [ IsQuiverRepresentation, IsHomogeneousList ] );

#! @Description
#!  Returns the vertices where the element  <A>r</A>  in a representation is
#!  supported. 
#! @Returns list of vertices
#! @Arguments r
DeclareAttribute( "SupportOfElement", IsQuiverRepresentationElement );

#! @Description
#!  Returns a inclusion of the radical the representation  <A>R</A>  into the 
#!  representation  <A>R</A>.
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R
DeclareAttribute( "RadicalInclusion", IsQuiverRepresentation );

#! @Description
#!  Returns the radical of the representation homomorphism  <A>f</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments f
DeclareAttribute( "RadicalOfMorphism", IsQuiverRepresentationHomomorphism );

#! @Description
#!  Returns the radical functor for the category <A>C</A>.
#! @Returns IsCapFunctor
#! @Arguments C
DeclareOperation( "RadicalFunctor", [ IsQuiverRepresentationCategory ] );

#! @Description
#!  Returns the natural transformation from the radical functor to the identity 
#!  functor for a category <A>C</A>.
#! @Returns IsCapNaturalTransformation
#! @Arguments C
DeclareOperation( "RadicalInclusionTransformation", [ IsQuiverRepresentationCategory ] );

#! @Description
#!  Returns a projection from the representation  <A>R</A>  onto the top of the 
#!  representation  <A>R</A>.
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R
DeclareAttribute( "TopProjection", IsQuiverRepresentation );

#! @Description
#!  Returns the top of the representation homomorphism  <A>f</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments f
DeclareAttribute( "TopOfMorphism", IsQuiverRepresentationHomomorphism );

#! @Description
#!  Returns the top functor for the category <A>C</A>.
#! @Returns IsCapFunctor
#! @Arguments C
DeclareOperation( "TopFunctor", [ IsQuiverRepresentationCategory ] );

#! @Description
#!  Returns the natural transformation from the identity functor to the top functor
#!  for a category <A>C</A>.
#! @Returns IsCapNaturalTransformation
#! @Arguments C
DeclareOperation( "TopProjectionTransformation", [ IsQuiverRepresentationCategory ] );

#! @Description
#!  Returns a homomorphism from the indecomposable projetive representation corresponding to
#!  the vertex <C>i</C>, where <A>r</A> is supported to <A>R</A>, sending a top element to
#!  <A>r</A>. It signals an error message if the element <A>r</A> is not supported in only one
#!  vertex.
#! @Returns IsCapNaturalTransformation
#! @Arguments r, R
DeclareOperation( "HomFromProjective", [ IsQuiverRepresentationElement, IsQuiverRepresentation ] );

#! @Description
#! Returns a list which is a minimal generating set for the representation  <A>R</A>.
#! @Returns IsList
#! @Arguments R
DeclareAttribute( "MinimalGeneratingSet", IsQuiverRepresentation );

#! @Description
#! Returns the projective cover of the quiver representation  <A>R</A>. 
#! @Returns IsQuiverRepresentationHomomorphism
#! @Arguments R
DeclareAttribute( "ProjectiveCover", IsQuiverRepresentation );

#! @Description
#! Category for bases of quiver representations.
DeclareCategory( "IsQuiverRepresentationBasis", IsBasis );

#! @Description
#! Returns the elements in a basis of a representation, ordered by vertex.
#! The result is a list of lists, such that the basis elements belonging to vertex
#! number <C>i</C> are in the <C>i</C>th element of the list.
#! @Arguments B
DeclareAttribute( "BasisVectorsByVertex", IsQuiverRepresentationBasis );

#! @Description
#!  Create a homomorphism by specifying its action on a generating set.
#!  
#!  The arguments <A>R1</A> and <A>R2</A> are representations over the same algebra, <A>generators</A> 
#!  is a list of generators of <A>R1</A> and <A>images</A> is a list of elements in <A>R2</A> with the 
#!  same length as <A>generators</A>.  Returns a homomorphism from <A>R1</A> to <A>R2</A> sending the 
#!  i-th element in <A>generators</A> to the i-th element in <A>images</A>. If there is no such 
#!  homomorphism, then an error is signalled. 
#! @Returns IsQuiverRepresentationHomomorphism 
#! @Arguments R1, R2, generator, images
DeclareOperation( "QuiverRepresentationHomomorphismByImages", [ IsQuiverRepresentation, IsQuiverRepresentation, IsHomogeneousList, IsHomogeneousList ] );


#! @Section Layered representations

#! @Description
#!  Turn a flat representation into a layered representation.
#!
#!  The argument <A>R</A> is a representation over a tensor algebra
#!  <M>A \otimes B</M>, and the argument <A>s</A> is either <C>1</C> or <C>2</C>.
#!
#!  If <A>s</A> is <C>1</C>, then the result is a representation over <M>A</M>
#!  consisting of <M>B</M>-representations.
#!  If <A>s</A> is <C>2</C>, then the result is a representation over <M>B</M>
#!  consisting of <M>A</M>-representations.
#! @Returns IsQuiverRepresentation
#! @Arguments s, R
DeclareOperation( "AsLayeredRepresentation", [ IsPosInt, IsQuiverRepresentation ] );

#! @Description
#!  Turn a layered representation into a flat representation.
#! @Returns IsQuiverRepresentation
#! @Arguments s, R
DeclareOperation( "AsFlatRepresentation", [ IsPosInt, IsQuiverRepresentation ] );

#! @Section Operations on representations

#! @Description
#!  Returns the annihilator of a quiver representation in the algebra over which 
#!  the representation <A>R</A> is defined.
#! @Returns <Ref Filt="IsIdeal"/>
#! @Arguments R
DeclareAttribute( "AnnihilatorOfRepresentation", IsQuiverRepresentation );

#! @Description
#!  Returns true if the dimension of the vector space in vertex  i 
#!  in the representation  <A>R1</A> is less or equal to the 
#!  corresponding dimension for the representation <A>R2</A> for 
#!  all vertices.  Otherwise it returns false.
#! @Returns true or false
#! @Arguments R1, R2
DeclareOperation( "DimensionVectorPartialOrder", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#! This function computes endomorphism ring of the module  <A>R</A>  and
#! representing it as an general GAP algebra. The algorithm it uses is
#! based purely on linear algebra.
#! @Arguments R
#! @Returns an algebra
DeclareOperation( "EndomorphismAlgebra", [ IsQuiverRepresentation ] );

#! @Description
#! This function gives a translation from an element in 
#! EndomorphismAlgebra( <A>R</A> ) to an endomorphism of the representation  <A>R</A>. 
#! @Arguments R
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
DeclareOperation( "FromEndRToHomRR", [ IsQuiverRepresentation, IsMatrix ] );

#! @Description
#! This function gives a translation from an element in <C>Hom(R,R)</C>
#! to <C>EndomorphismAlgebra( <A>R</A> )</C> of the representation  <A>R</A>. 
#! @Arguments R
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
DeclareOperation( "FromHomRRToEndR", [ IsQuiverRepresentationHomomorphism ] );

#! @Description
#!  Returns the intersection of a finite set of quiver representations which are 
#!  subrepresentations of one given quiver representation. 
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments list
DeclareOperation( "IntersectionOfRepresentations", [ IsDenseList ] );

#