% Elements
member(pos_inf).   member(neg_inf).   member(X) :- number(X).
members([0.95, 0.30, 0.60, -0.30, -0.95, -0.60]).

% Distance
distance(X,Y,Z) :- (X=pos_inf;X=neg_inf;Y=pos_inf;Y=neg_inf),!,current_prolog_flag(max_tagged_integer,Z).
distance(X,Y,Z) :- Z is abs(Y-X).

% Ordering relation
leq(_,pos_inf).   leq(neg_inf,_).   leq(X,Y) :- X =< Y.

% Supremum and infimum
bot(neg_inf).    top(pos_inf).

% Binary operations
or_sup(X,Y,Y) :- leq(X,Y).
or_sup(X,_,X).
and_sup(X,Y,X) :- leq(X,Y).
and_sup(_,Y,Y).

% Aggregators
agr_add(pos_inf,_,pos_inf). agr_add(_,pos_inf,pos_inf).
agr_add(neg_inf,_,neg_inf). agr_add(_,neg_inf,neg_inf).
agr_add(X,Y,Z) :- Z is X+Y.

agr_prod(pos_inf,_,pos_inf). agr_prod(neg_inf,_,neg_inf).
agr_prod(_,pos_inf,pos_inf). agr_prod(_,neg_inf,neg_inf).
agr_prod(X,Y,Z) :- Z is X*Y.

agr_sigmoid(pos_inf,pos_inf).
agr_sigmoid(neg_inf,neg_inf).
agr_sigmoid(X,Y) :- Y is 1 / (1+exp(-X)).

agr_binary(pos_inf,pos_inf).
agr_binary(neg_inf,neg_inf).
agr_binary(X,Y) :- (X<0 -> Y=0; Y=1).

agr_softplus(pos_inf,pos_inf).
agr_softplus(neg_inf,neg_inf).
agr_softplus(X,Y) :- Y is log(1+exp(X)).

agr_softsign(pos_inf,pos_inf).
agr_softsign(neg_inf,neg_inf).
agr_softsign(X,Y) :- Y is (X)/(1+abs(X)).

agr_relu(pos_inf,pos_inf).
agr_relu(neg_inf,neg_inf).
agr_relu(X,Y) :- (X<0 -> Y=0; Y=X).

%agr_leaky_relu(pos_inf,pos_inf).
%agr_leaky_relu(neg_inf,neg_inf).
%agr_leaky_relu(X,Y) :- (X<0 -> Y is 0.01*X; Y=X).

agr_tanh(pos_inf,pos_inf).
agr_tanh(neg_inf,neg_inf).
agr_tanh(X,Y) :- Y is ((exp(X)-exp(-X))/(exp(X)+exp(-X)).

%agr_arctan(pos_inf,pos_inf).
%agr_arctan(neg_inf,neg_inf).
%agr_arctan(X,Y) :- Y is atan(X).

%agr_sinusoid(pos_inf,pos_inf).
%agr_sinusoid(neg_inf,neg_inf).
%agr_sinusoid(X,Y) :- (X=0 -> Y is 1; Y is (sin(X)/X)).

% Identity function
agr_linear(pos_inf,pos_inf).
agr_linear(neg_inf,neg_inf).
agr_linear(X,Y) :- Y is X.
