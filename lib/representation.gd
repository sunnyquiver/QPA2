#! @Chapter Representations

#! @Section Categories for representations and their elements

#! @Description
#!  Category for elements of a quiver representation.
DeclareCategory( "IsQuiverRepresentationElement", IsVector );

#! @Description
#!  Category for quiver representations.
DeclareCategory( "IsQuiverRepresentation",
                 IsVectorSpace and
                 CategoryCollections( IsQuiverRepresentationElement ) );

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
                  [ IsQuiverRepresentation, IsDenseList ] );
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
DeclareOperation( "QuiverRepresentationElementByVertices",
                  [ IsQuiverRepresentation, IsDenseList, IsDenseList ] );

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

#! @Section Constructing representations

#! @BeginGroup QuiverRepresentation
#! @Description
#!  Construct a quiver representation over the quiver algebra <A>A</A>.
#!  The list <A>dimensions</A> gives the dimension of the vector space
#!  in each vertex.
#!  The list <A>matrices</A> gives the matrices for the linear maps
#!  corresponding to the arrows.
#!  <P/>
#!  The variant <C>QuiverRepresentationNC</C> does not check that the
#!  contents of the lists <A>dimensions</A> and <A>matrices</A> are
#!  compatible with each other and with the algebra.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments A, dimensions, matrices
DeclareOperation( "QuiverRepresentation", [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );
#! @Arguments A, dimensions, matrices
DeclareOperation( "QuiverRepresentationNC", [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );
#! @EndGroup

#! @Description
#!  Construct a quiver representation by specifying matrices for certain arrows.
#!  This works like the constructor <Ref Oper="QuiverRepresentation"/>,
#!  except that the entries in the list <A>matrices</A> correspond to the arrows
#!  in the list <A>arrows</A>.  All arrows of the quiver that are not present in
#!  this list get zero maps in the representation.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments A, dimensions, arrows, matrices
DeclareOperation( "QuiverRepresentationByArrows",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );

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
DeclareAttribute( "VertexDimensions", IsQuiverRepresentation );

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
DeclareOperation( "MatrixForArrow", [ IsQuiverRepresentation, IsPosInt ] );

#! @Description
#!  Returns the matrix for the map in the representation <A>R</A>
#!  corresponding to the arrow <A>a</A>.
#! @Returns <Ref BookName="Reference" Filt="IsMatrix"/>
#! @Arguments R, a
DeclareOperation( "MatrixForArrow", [ IsQuiverRepresentation, IsArrow ] );

#! @Description
#!  Returns the matrix for the map in the representation <A>R</A>
#!  corresponding to the path <A>p</A>, that is, the composition of
#!  the maps corresponding to the arrows in <A>p</A>.
#! @Returns <Ref BookName="Reference" Filt="IsMatrix"/>
#! @Arguments R, p
DeclareOperation( "MatrixForPath", [ IsQuiverRepresentation, IsPath ] );

#! @Description
#!  Returns the matrix for the map in the representation <A>R</A>
#!  corresponding to the homogeneous algebra element <A>e</A>.
#! @Returns <Ref BookName="Reference" Filt="IsMatrix"/>
#! @Arguments R, e
DeclareOperation( "MatrixForAlgebraElement", [ IsQuiverRepresentation, IsQuiverAlgebraElement ] );

DeclareCategory( "IsQuiverRepresentationHomomorphism",
                 IsMapping and IsSPGeneralMapping and IsVectorSpaceHomomorphism );

DeclareOperation( "QuiverRepresentationHomomorphism",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsDenseList ] );

DeclareOperation( "QuiverRepresentationHomomorphismNC",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsDenseList ] );

DeclareOperation( "MatricesOfRepresentationHomomorphism",
                  [ IsQuiverRepresentationHomomorphism ] );

# DeclareOperation( "ImageElm",
#                   [ IsQuiverRepresentationHomomorphism,
#                     IsQuiverRepresentationElement ] );

DeclareOperation( "MatrixVectorMultiplication", [ IsQuiver ] );
