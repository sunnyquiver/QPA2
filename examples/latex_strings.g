#! @BeginChunk Example_LaTeX_RightQuiver
#! @BeginExample
Q := RightQuiver( "Q(3)[alpha:1->2,beta:2->3,gamma:1->3]" );
#! Q(3)[alpha:1->2,beta:2->3,gamma:1->3]
SetLabelsAsLaTeXStrings( Q, 
            [ "V_1", "V_2", "V_3" ],
            [ "\\alpha", "\\beta", "\\gamma" ]
          );
kQ := PathAlgebra( Rationals, Q );
#! Rationals * Q
LabelAsLaTeXString( Q.1 );
#! "V_1"
e := 1/2 * kQ.alpha * kQ.beta - kQ.gamma + kQ.1;
#! 1/2*(alpha*beta) - 1*(gamma) + 1*(1)
LaTeXStringForQPA( e );
#! "\\frac{1}{2}{\\alpha\\beta}-{\\gamma}+{V_1}"
LaTeXStringForQPA( e : MultiplicationSymbol := "*" );
#! "\\frac{1}{2}{\\alpha{*}\\beta}-{\\gamma}+{V_1}"
LaTeXStringForQPA( e : MultiplicationSymbol := "*",
                       ScalarMultiplicationSymbol := "\\cdot" );
#! "\\frac{1}{2}\\cdot{\\alpha{*}\\beta}-{\\gamma}+{V_1}"
#! @EndExample
#! @EndChunk
