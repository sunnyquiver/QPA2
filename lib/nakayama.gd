#! @Chapter Special quivers, algebras and modules

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

