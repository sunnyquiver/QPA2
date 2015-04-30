#! @Chapter Quiver algebras

#! We use the term **quiver algebra** for an algebra that is
#! either a path algebra or a quotient of a path algebra by
#! some ideal.

#! @Section Categories for algebras, elements and ideals

#! @Description
#!  Category for elements of quiver algebras.
DeclareCategory( "IsQuiverAlgebraElement", IsRingElementWithOne );

#! @Description
#!  Category for quiver algebras.
DeclareCategory( "IsQuiverAlgebra", IsAlgebraWithOne and CategoryCollections( IsQuiverAlgebraElement ) );

#! @Description
#!  Category for elements of path algebras.
DeclareCategory( "IsPathAlgebraElement", IsQuiverAlgebraElement );

#! @Description
#!  Category for path algebras.
DeclareCategory( "IsPathAlgebra", IsQuiverAlgebra );

#! @Description
#!  Category for (two-sided) ideals in path algebras.
DeclareCategory( "IsPathIdeal", IsRing );

#! @Description
#!  Category for elements of quotients of path algebras.
DeclareCategory( "IsQuotientOfPathAlgebraElement", IsQuiverAlgebraElement );

#! @Description
#!  Category for quotients of path algebras.
DeclareCategory( "IsQuotientOfPathAlgebra", IsQuiverAlgebra );


#! @Section Constructing algebras

#! @Arguments k, Q
#! @Description
#!  Constructs the path algebra over the field <A>k</A>
#!  using the quiver <A>Q</A>.
DeclareOperation( "PathAlgebra", [ IsField, IsQuiver ] );

#! @InsertChunk Example_PathAlgebra

#! @BeginGroup QuotientOfPathAlgebra
#! @Description
#!  Constructs a quotient of a path algebra.
#!  <P/>
#!  The first argument is a path algebra <A>kQ</A>.
#!  The second argument is either an ideal <A>I</A> in the algebra <A>kQ</A>
#!  or a list <A>relations</A> of elements of <A>kQ</A>.
#!  The result is <A>kQ</A> divided by the ideal <A>I</A>,
#!  or by the ideal generated by <A>relations</A>.
#!  <P/>
#!  The <C>/</C> operator is defined to do the same as <C>QuotientOfPathAlgebra</C>,
#!  so <C>QuotientOfPathAlgebra( <A>kQ</A>, <A>I</A> )</C> or
#!  <C>QuotientOfPathAlgebra( <A>kQ</A>, <A>relations</A> )</C>
#!  can also be written as
#!  <C><A>kQ</A>/<A>I</A></C> or
#!  <C><A>kQ</A>/<A>relations</A></C>.
#! @Arguments kQ, I
DeclareOperation( "QuotientOfPathAlgebra", [ IsPathAlgebra, IsPathIdeal ] );
#! @Arguments kQ, relations
DeclareOperation( "QuotientOfPathAlgebra", [ IsPathAlgebra, IsHomogeneousList ] );
#! @Arguments kQ, I
DeclareOperation( "\/", [ IsPathAlgebra, IsPathIdeal ] );
#! @Arguments kQ, relations
DeclareOperation( "\/", [ IsPathAlgebra, IsHomogeneousList ] );
#! @EndGroup

#! @InsertChunk Example_QuotientOfPathAlgebra

#! @Section Information about an algebra

#! @Arguments A
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Returns the quiver Q of the quiver algebra <A>A</A> = $kQ/I$.
DeclareAttribute( "QuiverOfAlgebra", IsQuiverAlgebra );

#! @Arguments A
#! @Returns list of <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Returns a list of relations for the quiver algebra <A>A</A>.
#!  That is, if <A>A</A> = $kQ/I$, then this operation returns a list
#!  of generators for the ideal $I$.
#!  If <A>A</A> is a path algebra, this operation returns the empty list.
DeclareAttribute( "RelationsOfAlgebra", IsQuiverAlgebra );

#! @Arguments A
#! @Returns <Ref Filt="IsPathIdeal"/>
#! @Description
#!  For a quotient <A>A</A> = $kQ/I$ of a path algebra,
#!  this operation returns the ideal $I$.
DeclareAttribute( "IdealOfQuotient", IsQuotientOfPathAlgebra );

# field of a QuiverAlgebra: LeftActingDomain
#! <ManSection>
#! <Attr Name="LeftActingDomain" Arg="A"/>
#! <Description>
#! Returns the underlying field of the algebra <A>A</A>.
#! </Description>
#! <Returns><C>IsField</C></Returns>
#! </ManSection>

#! @Arguments A
#! @Returns <Ref Filt="IsPathAlgebra"/>
#! @Description
#!  Given a quiver algebra <A>A</A> = $kQ/I$, this operation returns
#!  the path algebra $kQ$.
DeclareAttribute( "PathAlgebra", IsQuiverAlgebra );


#! @Section Accessing algebra elements

#! @BeginGroup PathAlgebraElement
#! @Returns <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Creates an element of a path algebra.
#!  <P/>
#!  The argument <A>kQ</A> is a path algebra,
#!  <A>paths</A> is a list of paths in the quiver of <A>kQ</A>,
#!  and <A>coefficients</A> a list of coefficients.
#!  The lists <A>paths</A> and <A>coefficients</A> must have the same length.
#!  If the paths are $p_1, \ldots, p_n$
#!  and the coefficients are $c_1, \ldots, c_n$,
#!  then the result is the linear combination
#!  $\sum_{i=1}^n c_i p_i$.
#!  <P/>
#!  For the operation <C>PathAlgebraElement</C>,
#!  the elements in <A>paths</A> may be listed in any order,
#!  and the same paths can occur several times in the list.
#!  The operation <C>PathAlgebraElementNC</C> assumes that
#!  the list of paths is sorted in decreasing order
#!  (by the path ordering of the quiver)
#!  and does not have any duplicates.
#! @Arguments kQ, coefficients, paths
DeclareOperation( "PathAlgebraElement", [ IsPathAlgebra, IsHomogeneousList, IsHomogeneousList ] );
#! @Arguments kQ, coefficients, paths
DeclareOperation( "PathAlgebraElementNC", [ IsPathAlgebra, IsHomogeneousList, IsHomogeneousList ] );
#! @EndGroup

#! @InsertChunk Example_PathAlgebraElement

#! @Arguments A, e
#! @Description
#!  Given a quotient <A>A</A> = kQ/I of a path algebra kQ,
#!  and an element <A>e</A> of kQ, this operation produces
#!  the element of <A>A</A> represented by <A>e</A>.
DeclareGlobalFunction( "QuotientOfPathAlgebraElement" );

#! @InsertChunk Example_QuotientOfPathAlgebraElement

#! @Arguments A, p
#! @Description
#!  Returns the path <A>p</A> as an element of the quiver algebra <A>A</A>.
DeclareOperation( "PathAsAlgebraElement", [ IsQuiverAlgebra, IsPath ] );

#! @InsertChunk Example_PathAsAlgebraElement

#! @BeginGroup AlgebraElementByLabel
#! @Description
#!  Returns the primitive path (vertex or arrow) with label <A>label</A>,
#!  as an element of the quiver algebra <A>A</A>.
#!  If no such path exists, then <C>fail</C> is returned.
#!  The operation <C><A>A</A>[ <A>label</A> ]</C> is equivalent to
#!  <C>AlgebraElementByLabel( <A>A</A>, <A>label</A> )</C>.
#! @Returns <Ref Filt="IsQuiverAlgebraElement"/> or <C>fail</C>
#! @Arguments A, label
DeclareOperation( "AlgebraElementByLabel", [ IsQuiverAlgebra, IsObject ] );
#! @Arguments A, label
DeclareOperation( "\[\]", [ IsQuiverAlgebra, IsObject ] );
#! @EndGroup

#! @InsertChunk Example_AlgebraElementByLabel

#! @Arguments A, string
#! @Description
#!  Returns the path described by the string <A>string</A>
#!  (see <Ref Oper="PathFromString"  Label="for IsQuiver, IsString"/>)
#!  as an element of the quiver algebra <A>A</A>.
#!  If no such path exists, then <C>fail</C> is returned.
#!  <P/>
#!  This operation can also be called by writing <C><A>A</A>.str</C>,
#!  where <C>str</C> is an unquoted string literal.
DeclareOperation( "AlgebraElementFromString", [ IsQuiverAlgebra, IsString ] );

#! @InsertChunk Example_AlgebraElementFromString


#! @Section Information about an element

#! @Arguments e
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Description
#!  Returns the quiver algebra the element <A>e</A> belongs to.
DeclareAttribute( "AlgebraOfElement", IsQuiverAlgebraElement );

#! @Arguments e
#! @Returns list of field elements
#! @Description
#!  Returns a list of coefficients for the algebra element <A>e</A>.
#!  <P/>
#!  Every element of a quiver algebra can be written as a linear combination
#!  $\sum_{i=1}^n c_i p_i$ of paths.
#!  The attribute <Ref Attr="Paths" Label="for IsQuiverAlgebraElement"/> gives the list
#!  $p_1, \ldots, p_n$ of paths for the element <A>e</A>,
#!  and this attribute returns the corresponding list
#!  $c_1, \ldots, c_n$ of coefficients.
DeclareAttribute( "Coefficients", IsQuiverAlgebraElement );

#! @Arguments e
#! @Returns list of <Ref Filt="IsPath"/>
#! @Description
#!  Returns a list of paths for the algebra element <A>e</A>.
#!  <P/>
#!  Every element of a quiver algebra can be written as a linear combination
#!  $\sum_{i=1}^n c_i p_i$ of paths.
#!  This operation produces the list
#!  $p_1, \ldots, p_n$ of paths for the element <A>e</A>.
#!  The corresponding coefficients
#!  $c_1, \ldots, c_n$ can be obtained by the attribute
#!  <Ref Attr="Coefficients" Label="for IsQuiverAlgebraElement"/>.
#!  <P/>
#!  The paths are ordered in decreasing order,
#!  by the path ordering of the quiver.
#!  <P/>
#!  If <A>e</A> is an element of a path algebra, then there is a unique
#!  way to express it as a linear combination of paths.
#!  If <A>e</A> is an element of a quotient of a path algebra, then there
#!  may be several ways to express it as a linear combination of paths.
#!  In this case, the chosen combination is that of
#!  <C>Representative( <A>e</A> )</C>
#!  (see <Ref Attr="Representative" Label="for IsQuotientOfPathAlgebraElement"/>).
DeclareAttribute( "Paths", IsQuiverAlgebraElement );

#! @Arguments e
#! @Returns <C>true</C> or <C>false</C>
#! @Description
#!  Checks whether the element <A>e</A> is uniform,
#!  that is, whether all paths in the element have the same
#!  source and the same target.
DeclareProperty( "IsUniform", IsQuiverAlgebraElement );

#! @Arguments e
#! @Returns <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Returns the canonical representative path algebra element
#!  of the element <A>e</A> of a quotient of a path algebra.
DeclareAttribute( "Representative", IsQuotientOfPathAlgebraElement );


#! @Section Manipulation of elements

#! @Arguments e, A, f
#! @Returns <Ref Filt="IsQuiverAlgebraElement"/>
#! @Description
#!  Translate the element <A>e</A> to an element in the
#!  quiver algebra <A>A</A> by using the function <A>f</A>
#!  on each path.
DeclareOperation( "TranslateAlgebraElement", [ IsQuiverAlgebraElement, IsQuiverAlgebra, IsFunction ] );


#! @Section Operations related to Groebner basis theory

#! @Arguments e
#! @Returns <Ref Filt="IsPath"/>
#! @Description
#!  Returns the leading path of the path algebra element <A>e</A>,
#!  that is, the path occuring in <A>e</A> which is largest with
#!  respect to the quiver's ordering of paths.
DeclareAttribute( "LeadingPath", IsPathAlgebraElement );

#! @Arguments e
#! @Returns field element
#! @Description
#!  Returns the leading coefficient of the path algebra element <A>e</A>,
#!  that is, the coefficient of the leading path.
DeclareOperation( "LeadingCoefficient", [ IsPathAlgebraElement ] );

#! @Arguments e
#! @Returns <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Returns the leading term of the path algebra element <A>e</A>.
DeclareAttribute( "LeadingTerm", IsPathAlgebraElement );

#! @Arguments e
#! @Returns <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Returns the sum of all the non-leading terms of the path algebra element <A>e</A>.
DeclareAttribute( "NonLeadingTerms", IsPathAlgebraElement );

#! @Arguments e, divisors
#! @Returns list
#! @Description
#!  Performs division of the path algebra element <A>e</A>
#!  by the elements in the list <A>divisors</A>.
#!  <P/>
#!  When dividing an element $e$ by a list $d_1, \ldots, d_n$ of divisors,
#!  the goal is to find a remainder $r$ and left/right quotients
#!  $L_{i,j}$ and $R_{i,j}$
#!  (for $1 \le i \le n$ and $1 \le j \le m_i$, where
#!  $m_i$ is some integer depending on $i$) such that
#!  $$ e = r + \sum_{i=1}^n \sum_{j=1}^{m_i} L_{i,j} \cdot d_i \cdot R_{i,j}. $$
#!  <P/>
#!  This operation returns a list <C>[ quotients, remainder ]</C>,
#!  where <C>quotients</C> is a list containing the left/right quotients,
#!  and <C>remainder</C> is the remainder.
#!  The left quotient $L_{i,j}$ is accessible as <C>quotients[ i ][ j ][ 1 ]</C>,
#!  and the right quotient $R_{i,j}$ as <C>quotients[ i ][ j ][ 2 ]</C>.
DeclareOperation( "DivideByList", [ IsPathAlgebraElement, IsList ] );

#! @Arguments e, divisors
#! @Returns <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Reduce the path algebra element <A>e</A> by the elements
#!  in the list <A>divisors</A>.
#!  <P/>
#!  This operation returns the remainder produced by
#!  <Ref Oper="DivideByList" Label="for IsPathAlgebraElement, IsList"/>.
DeclareOperation( "Reduce", [ IsPathAlgebraElement, IsList ] );

#! @Arguments f, g, b, c
#! @Returns <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Computes an overlap relation.
#!  Given two path algebra elements <A>f</A> and <A>g</A>,
#!  and two paths <A>b</A> and <A>c</A>, such that
#!  <C>LeadingPath( f ) * c = b * LeadingPath( g )</C>,
#!  this operation produces the overlap relation
#!  $$ \frac{1}{LC(f)} fc - \frac{1}{LC(g)} bg $$
#!  <A>f</A> and <A>g</A> with respect to <A>b</A> and <A>c</A>.
DeclareOperation( "OverlapRelation",
                  [ IsPathAlgebraElement, IsPathAlgebraElement,
                    IsPath, IsPath ] );

#! @Arguments f, g
#! @Returns <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Finds all overlap relations between the path algebra elements
#!  <A>f</A> and <A>g</A>.
#!  <P/>
#!  We consider two elements $f$ and $g$ to have an overlap
#!  when there are paths $b$ and $c$ such that
#!  $LP(f) \cdot c = b \cdot LP(g)$.
#!  The corresponding overlap relation is then the element
#!  $$ \frac{1}{LC(f)} fc - \frac{1}{LC(g)} bg. $$
#!  For a given pair $(f,g)$, there may be several such overlap
#!  relations, corresponding to several choices of paths $(b,c)$.
#!  This operation constructs all these overlap relations,
#!  and returns them as a list.
#!  <P/>
#!  Overlap relations are interesting because they point to obstructions
#!  to a list $G$ of generators of an ideal forming a Groebner basis:
#!  if some overlap relation between two elements of $G$ reduces to
#!  a nonzero remainder $r$ when divided by $G$, then $G$ is not a Groebner basis.
#!  In Buchberger's algorithm, we then add the remainder $r$ to the
#!  list $G$.
#!  <P/>
#!  Note that our definition of overlaps does not include situations of the form
#!  $LP(f) = a \cdot LP(g) \cdot a'$,
#!  although they too give obstructions to a set being a Groebner basis,
#!  and would be considered as overlaps by some authors.
DeclareOperation( "OverlapRelations",
                  [ IsPathAlgebraElement, IsPathAlgebraElement ] );

#! @Arguments G
#! @Returns list of <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Returns a tip reduced version of the list <A>G</A>
#!  of path algebra elements.
#!  The original list is not modified.
#!  <P/>
#!  The resulting list has the following properties:
#!  (1) It generates the same ideal as <A>G</A> does.
#!  (2) No element in the list has a leading path which divides
#!  the leading path of another element in the list.
DeclareOperation( "TipReduce", [ IsHomogeneousList ] );

#! @Arguments G
#! @Returns list of <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Given a list <A>G</A> of generators for an ideal $I$ in a path algebra,
#!  this operation produces a Groebner basis for the ideal $I$.
DeclareOperation( "ComputeGroebnerBasis", [ IsHomogeneousList ] );

#! @Arguments I
#! @Returns list of <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  A Groebner basis for the ideal <A>I</A>.
DeclareAttribute( "GroebnerBasis", IsPathIdeal );

#! @Section Algebra constructions

#! @Arguments A
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Description
#!  Returns the opposite algebra of <A>A</A>.
DeclareAttribute( "OppositeAlgebra", IsQuiverAlgebra );

#! @Arguments e
#! @Returns <Ref Filt="IsQuiverAlgebraElement"/>
#! @Description
#!  Returns the element corresponding to <A>e</A> in the opposite algebra.
DeclareAttribute( "OppositeAlgebraElement", IsQuiverAlgebraElement );

#! @Arguments A, B
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Description
#!  Returns the tensor product of the quiver algebras <A>A</A> and <A>B</A>.
DeclareOperation( "TensorProductOfAlgebras", [ IsQuiverAlgebra, IsQuiverAlgebra ] );

DeclareAttribute( "EnvelopingAlgebra", IsQuiverAlgebra );

#
