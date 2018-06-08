:- ['wordVectors.pl'].
:- ['trigramModel.pl'].


% Main predicate
%
most_similar(Z, X) :-
	findall((ContextWord, N), (wordvec(ContextWord, _), similarity(Z, ContextWord, N)), L),
	sort(2, @>, L, LL),
	[Head | Tail] = LL,
	member((X, _), Tail).

similarity(X,Y,Z):-
	get_sum_vec(X,VectorXTotal),              % Sum context vectors to Xs blank vector
	get_sum_vec(Y,VectorYTotal),              % Sum context vectors to Ys blank vector
        cosine(VectorXTotal,VectorYTotal,Z),!.   % Calculate cosine/euclidean between Xs vector and Ys vector

% Calculating Dot Product
dotProd([], [], 0).
dotProd([A|X], [B|Y], Z) :-
	dotProd(X, Y, R),
	Z is R + A * B.

% Calculating Sum of Squares the List
sumOfSquare([], 0).
sumOfSquare([Head | Tail], S) :-
	sumOfSquare(Tail , R),
	S is R + (Head * Head).

% The cosine distance
%
cosine(X, Y, Z):-
		dotProd(X, Y, R),						% Storing Dot Product of X and Y in R
		sumOfSquare(X, S1),					% Calculating Sum of Squares of all elements of X and storing result in S1
		sumOfSquare(Y, S2),					% Calculating Sum of Squares of all elements of Y and storing result in S2
		M is sqrt(S1) * sqrt(S2),
		R == 0 -> Z = 0; Z is float(R) / float(M).		% Storing result in Z

%  Create randomly indexed vector
%
get_sum_vec(X,VectorTotal):-
	length(Vector,150),                                    % Create list of size 150
        findall(Y,(member(Y,Vector),Y =0),Vector), % Fill list with 0s
	add_all_vecs_left(X,Vector,VectorTotal).   % Add all context vectors

add_all_vecs_left(Word,Vector,VectorTotal):-
	findall( pair(ContexWord,N), t(ContexWord,_,Word,N), L1),    % Word at position -2
	% write(L1),
	findall( pair(ContexWord,N), t(_,ContexWord,Word,N), L2),    % Word at position -1
	% write(L2),
	findall( pair(ContexWord,N), t(Word,ContexWord,_,N), L3),    % Word at position +1
	% write(L3),
	findall( pair(ContexWord,N), t(Word,_,ContexWord,N), L4),    % Word at position +2
       add(Vector,L1,VTemp1),                                                          % Add all the above vectors
       add(VTemp1,L2,VTemp2),
       add(VTemp2,L3,VTemp3),
       add(VTemp3,L4,VectorTotal).

% Vector addition
%
add(Vector,[],Vector).
add(Vector1,[Pair|L],Vector):-
	add_V_N_times(Vector1,Pair,VectorR),
       add(VectorR,L,Vector).

% Done adding
%
add_V_N_times(Vector,pair(_,0),Vector).

% Add the vector given the number of times the trigram occurs
%
add_V_N_times(Vector1,pair(CWord,N),VectorR):-
        N > 0,
        wordvec(CWord,Vector2),
        maplist(plus, Vector1,Vector2,Vector3),
        M is N-1,
	add_V_N_times(Vector3,pair(CWord,M),VectorR).
