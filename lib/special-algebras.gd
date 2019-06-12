#! @Chapter Special quivers, algebras and modules

#! @Section Associated monomial algebras

#!
DeclareProperty( "IsFiniteGlobalDimensionAlgebra", IsAlgebraWithOne );

#! @Description
#!  Returns the associated monomial algebra by using the Groebner basis  <A>A</A>  is 
#!  endoved with and in particular the ordering of the vertices and the arrows.
#!  Taking another ordering of the vertices and the arrows might change the
#!  associated algebra.
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Arguments A
DeclareAttribute( "AssociatedMonomialAlgebra", IsQuiverAlgebra );

#! @Section Canonical algebras

#! @BeginGroup CanonicalAlgebra
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Description
#!  Given a field  <A>K</A>, a sequence of  <A>weights</A>  and a sequence of 
#!  coefficients <A>relcoeff</A> for the relations, this function constructs the 
#!  canonical algebra with this data. If the number of weights is
#!  two, then the number of coefficients for the relations must be
#!  zero, that is, represented by an empty list, [].<P/>
#!  The algebra is constructed as a quiver algebra.
#!  Use either <C>LeftCanonicalAlgebra</C> or <C>RightCanonicalAlgebra</C>
#!  to get an algebra based on either a left or a right quiver.
#!  (Alternatively, pass one of the constants <C>LEFT</C> or <C>RIGHT</C>
#!  as the first argument to <C>CanonicalAlgebra</C>.)
#! @Arguments dir, K, weights, relcoeff
DeclareOperation( "CanonicalAlgebra", [ IsDirection, IsField, IsList, IsList ] );
#! @Arguments K, weights, relcoeff
DeclareOperation( "LeftCanonicalAlgebra", [ IsField, IsList, IsList ] );
#! @Arguments K, weights, relcoeff
DeclareOperation( "RightCanonicalAlgebra", [ IsField, IsList, IsList ] );
#! @EndGroup

#! @Description
#!  Returns <C>true</C> if this property has been set for the algebra <A>A</A>.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsCanonicalAlgebra", IsAlgebraWithOne );


#! @Section Kronecker algebras

#! @Description
#!  Returns <C>true</C> if this property has been set for the algebra <A>A</A>.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsKroneckerAlgebra", IsAlgebraWithOne );

#! @BeginGroup KroneckerAlgebra
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Description
#!  Construct the Kronecker algebra over a field <A>K</A> with  <A>n</A>  
#!  arrows.<P/>
#!  The algebra is constructed as a quiver algebra.
#!  Use either <C>LeftKroneckerAlgebra</C> or <C>RightKroneckerAlgebra</C>
#!  to get an algebra based on either a left or a right quiver.
#!  (Alternatively, pass one of the constants <C>LEFT</C> or <C>RIGHT</C>
#!  as the first argument to <C>KroneckerAlgebra</C>.)
#! @Arguments dir, K, n
DeclareOperation( "KroneckerAlgebra", [ IsDirection, IsField, IS_INT ] );
#! @Arguments K, n
DeclareOperation( "LeftKroneckerAlgebra", [ IsField, IS_INT] );
#! @Arguments K, n
DeclareOperation( "RightKroneckerAlgebra", [ IsField, IS_INT ] );
#! @EndGroup


#! @Section Nakayama algebras

#! @BeginGroup NakayamaAlgebra
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Description
#!  Construct the Nakayama algebra given by the admissible sequence
#!  <A>seq</A> (a list of positive integers) over the field <A>K</A>.
#!  <P/>
#!  The algebra is constructed as a quiver algebra.
#!  Use either <C>LeftNakayamaAlgebra</C> or <C>RightNakayamaAlgebra</C>
#!  to get an algebra based on either a left or a right quiver.
#!  (Alternatively, pass one of the constants <C>LEFT</C> or <C>RIGHT</C>
#!  as the first argument to <C>NakayamaAlgebra</C>.)
#! @Arguments dir, K, seq
DeclareOperation( "NakayamaAlgebra", [ IsDirection, IsField, IsList ] );
#! @Arguments K, seq
DeclareOperation( "LeftNakayamaAlgebra", [ IsField, IsList ] );
#! @Arguments K, seq
DeclareOperation( "RightNakayamaAlgebra", [ IsField, IsList ] );
#! @EndGroup

#! @Returns IsBool
#! @Description
#!  Check whether the sequence <A>seq</A> of positive integers
#!  is an admissible sequence.
#! @Arguments seq
DeclareOperation( "IsAdmissibleSequence", [ IsDenseList ] );

#! @Description
#!  Check whether the algebra <A>A</A> is a Nakayama algebra.
#! @Arguments A
DeclareProperty( "IsNakayamaAlgebra", IsQuiverAlgebra );

#! @Returns list of positive integers
#! @Description
#!  Returns the admissible sequence of the Nakayama algebra <A>A</A>.
#!  Raises an error if <A>A</A> is not a Nakayama algebra.
#! @Arguments A
DeclareAttribute( "AdmissibleSequence", IsQuiverAlgebra );

#! @Section Poset algebras

#! @Description
#!  Returns true if the algebra <A>A</A> was constructed as a poset algebra. 
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsPosetAlgebra", IsQuiverAlgebra ); 

#! @Description 
#!  Takes as arguments a field  <A>K</A>  and a poset  <A>P</A> 
#!  and returns associated poset algebra  <M>KP</M>  as a quiver algebra.
#! @Returns <Ref Filt="IsPosetAlgebra"/>
#! @Arguments K, A
DeclareOperation( "PosetAlgebra", [ IsField, IsPoset ] ); 

#! @Description
#!  Given a poset algebra, this function returns the poset from which it was
#!  constructed. 
#! @Returns <Ref Filt="IsPoset"/>
#! @Arguments A
DeclareAttribute( "PosetOfPosetAlgebra", IsPosetAlgebra );

#!
InstallTrueMethod( IsFiniteGlobalDimensionAlgebra, IsPosetAlgebra );

#! @Section Truncated path algebras

#! @Description
#!  Given a field <A>K</A>, a quiver <A>Q</A> and an integer <A>n</A> this
#!  functions constructs the truncated algebra <M>KQ/J^n</M>, where <M>J</M>
#!  is the ideal in <M>KQ</M> generated by the arrows.
#! @Returns <Ref Filt="IsQuiverAlgebra"/>
#! @Arguments K, Q, n
DeclareOperation( "TruncatedPathAlgebra", [ IsField, IsQuiver, IS_INT ] );

#! @Section Selfinjective algebras

#! @Description
#!  Returns true or false depending on whether or not 
#!  the algebra  <A>A</A>  is selfinjective.
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments A
DeclareProperty( "IsSelfinjectiveAlgebra",  IsQuiverAlgebra );

#! @Description
#!  Checks if the entered algebra is selfinjective, and returns false
#!  otherwise. When the algebra is selfinjective, then it returns a 
#!  list of two elements, where the first is the Nakayama permutation 
#!  on the simple modules, while the second is the Nakayama permutation
#!  on the indexing set of the simple modules. 
#! @Returns two permutations
#! @Arguments A
DeclareAttribute( "NakayamaPermutation", IsQuiverAlgebra );

#! @Description
#!  Checks if the entered algebra is selfinjective, and returns false
#!  otherwise. When the algebra is selfinjective, then it returns the 
#!  Nakayama automorphism of  <A>A</A>.
#! @Returns <Ref Filt="IsQuiverAlgebraHomomorphism"/>
#! @Arguments A
DeclareAttribute( "NakayamaAutomorphism", IsQuiverAlgebra ); 

#! @Description
#!  Checks if the entered algebra is selfinjective, and returns false
#!  otherwise. When the algebra is selfinjective, then it returns a 
#!  list of two elements, where the first is the Nakayama permutation 
#!  on the simple modules, while the second is the Nakayama permutation
#!  on the indexing set of the simple modules.
#! @Returns <C>IS_INT</C>
#! @Arguments A
DeclareAttribute( "OrderOfNakayamaAutomorphism", IsQuiverAlgebra );


#! @Section Other classes of algebras

#!  Returns <C>true</C> if the attribute <C>IsFiniteGlobalDimensionAlgebra</C>
#!  has been set to true.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments A
# DeclareProperty( "IsFiniteGlobalDimensionAlgebra", IsAlgebraWithOne );

#! @Description
#!  The function returns true is  <A>A</A>  is a gentle algebra, and false otherwise.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsGentleAlgebra",  IsQuiverAlgebra );

#!
DeclareProperty( "IsGorensteinAlgebra", IsAlgebraWithOne );
InstallTrueMethod( IsGorensteinAlgebra, IsGentleAlgebra );

#! @Description
#!  Returns true or false depending on whether or not 
#!  the algebra  <A>A</A>  is hereditary.
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments A
DeclareProperty( "IsHereditaryAlgebra", IsQuiverAlgebra );

#! @Description
#!  The function returns true is  <A>A</A>  is a radical square zero algebra, and false otherwise.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsRadicalSquareZeroAlgebra", IsAlgebraWithOne );

#! @Description
#! The function returns true if  <A>A</A>  is a finite dimensional semisimple 
#! algebra and searches for another method otherwise.
#! @Arguments A
DeclareProperty( "IsSemisimpleAlgebra", IsAlgebraWithOne );

#! @Description
#!  The function returns true is  <A>A</A>  is a Schurian algebra, and false otherwise.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsSchurianAlgebra", IsQuiverAlgebra );

#! @Description
#!  The function returns true is  <A>A</A>  is a semi commutative algebra, and false otherwise.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsSemicommutativeAlgebra", IsPathAlgebra );
DeclareProperty( "IsSemicommutativeAlgebra", IsQuotientOfPathAlgebra);

#! @Description
#!  The function tests if the argument <A>A</A>, given as <M>KQ/I</M>, is given by a special 
#!  biserial quiver <M>Q</M>, an admissible ideal <M>I</M> and <M>I</M> satisfies 
#!  the "special biserial" conditions, that is, for any arrow <M>a</M> there exists at most
#!  one arrow <M>b</M> such that <M>ab\notin I</M> and there exists at most one arrow <M>c</M> 
#!  such that <M>ca\notin I</M>.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsSpecialBiserialAlgebra", IsPathAlgebra );
DeclareProperty( "IsSpecialBiserialAlgebra", IsQuotientOfPathAlgebra );
InstallTrueMethod( IsSpecialBiserialAlgebra, IsGentleAlgebra );

#! @Description
#! This functions tests if the argument <A>A</A> is a string algebra, that is,
#! if <A>A</A> is a path algebra, checks if it is a special biserial algebra,
#! if <A>A</A> is a quotient <M>kQ/I</M> of a path algebra <M>kQ/I</M> is a 
#! special biserial algebra and  <M>I</M> is a monomial ideal.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsStringAlgebra", IsPathAlgebra );
DeclareProperty( "IsStringAlgebra", IsQuotientOfPathAlgebra );

#! @Description
#!   This function determines if the algebra  <A>A</A>  is a weakly symmetric 
#!   algebra.
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsWeaklySymmetricAlgebra", IsQuiverAlgebra );

#