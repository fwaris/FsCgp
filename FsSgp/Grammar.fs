namespace FsSgp
module rec Grammar =

    type Start = CompundExp of Exp*Op*Exp | SimpleExp of Exp
    type Exp = CompoundTerm of Term*Op*Term | ParenTerm of Term*Op*Term
    type Term = Var of float | C of float
    type Op = Plus | Minus | Mult | Div


    