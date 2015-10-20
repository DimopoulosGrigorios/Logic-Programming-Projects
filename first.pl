%_________________________________________1____________________________________________
%________________________________________________________________________________________

lambda(List):-append(Alist,[Switch|Blist],List),append(Alist,[Switch],A),
strictly_increasing(A),strictly_descending([Switch|Blist]).

%synarthsh gia na elenxw an mia lista einai gnhsiws fthinousa
strictly_descending([A,B]):-!,A>B.
strictly_descending([A,B|Rest]):-A>B,strictly_descending([B|Rest]).

%synarthsh gia na elenxw an mia lista einai gnhsiws auksousa
strictly_increasing([A,B]):-!,A<B.
strictly_increasing([A,B|Rest]):-A<B,strictly_increasing([B|Rest]).

%_________________________________________2____________________________________________
%________________________________________________________________________________________


keypad(1,[]).
keypad(2,[a,b,c]).
keypad(3,[d,e,f]).
keypad(4,[g,h,i]).
keypad(5,[j,k,l]).
keypad(6,[m,n,o]).
keypad(7,[p,q,r,s]).
keypad(8,[t,u,v]).
keypad(9,[w,x,y,z]).


mobile(Letters,Numlist):-mobile2(Letters,List),flatten(List,Numlist).

mobile2([First],[List]):-letter_to_num(First,List).
mobile2([First,Second|Rest],[Fout|RestOut]):-keypad(_,Letters),member(First,Letters),member(Second,Letters),
letter_to_num(First,Flet),append(Flet,[hash],Fout),mobile([Second|Rest],RestOut).
mobile2([First,Second|Rest],[Fout|RestOut]):-keypad(_,Letters),member(First,Letters),not member(Second,Letters),
letter_to_num(First,Fout),mobile([Second|Rest],RestOut).

%epestrepse thn akolouthia arithmwn gia ena gramma
letter_to_num(Let,List):-keypad(N,Letters),member(Let,Letters),append(A,[Let|_],Letters),
length(A,Len),Length is Len+1,n_times(N,Length,List).

%dhmiourghse mia lista me N fores to stoixeio A
n_times(_,0,[]):-!.
n_times(A,N,[A|List]):-NN is N-1,n_times(A,NN,List).
%_________________________________________3____________________________________________
%________________________________________________________________________________________

permutation(N,K,L):-K>=1,K<N,from_one_to(N,Lista),smart_permutation(Lista,L,K).

smart_permutation(List, [X|Perm],Num):-delete(X,List,Rest),smart_per(Rest,Perm,X,[],Num).

%Synarthsh epistrofhs permutation wste na ikanopoiounte oi periorismoi tis askhshs
%epishs ginete tipoma twn diaforwn diaforwn pou apothikeuontai se mia lista
smart_per([],[],_,L,0):-write("\n The various differences are:"),write(L).
smart_per(List, [X|Perm],Previous,Lista,Num) :-delete(X,List,Rest),Var is Previous-X,abs(Var,NVar),
member(NVar,Lista),smart_per(Rest,Perm,X,Lista,Num).
smart_per(List, [X|Perm],Previous,Lista,Num) :-Num>0,delete(X,List,Rest),Var is Previous-X,abs(Var,NVar),
not member(NVar,Lista),append(Lista,[NVar],NLista),NNum is Num-1, smart_per(Rest,Perm,X,NLista,NNum).

%epistrofh mias listas apo to 1 ews ton arithmo pou tha dwsoume
from_one_to(1,[1]):-!.
from_one_to(N,[N|List]):-NN is N-1,from_one_to(NN,List).

%_________________________________________4____________________________________________
%________________________________________________________________________________________

bits(L,R,X):-length(R,Rlength),length(L,Llength),Dif is Rlength-Llength,
n_times(0,Dif,ZeroList),append(ZeroList,L,Left),bits2(Left,R,X).

%Synarthsh pou briskei auto pou zhtaei h ekfwnish gia duo listes mhden kai asswn isou mhkous
%Basismenh sto skeptiko tou duadikou susthmatos meta apo parathrhsh sumperiforas twn dyadikwn arithmwn
bits2([],[],[]).
bits2([1|Lrest],[1|Rrest],[1|Xrest]):-bits2(Lrest,Rrest,Xrest).
bits2([0|Lrest],[0|Rrest],[0|Xrest]):-bits2(Lrest,Rrest,Xrest).
bits2([0|Lrest],[1|Rrest],[0|Xrest]):-length(Rrest,Len),Len\=0,n_times(1,Len,Xrest).
bits2([0],[1],[1]).

%_________________________________________5____________________________________________
%________________________________________________________________________________________


doublechain([FL|L],[FM|M],Sum):-doublechain2([FL|L],M,Sumb2,1),doublechain2(L,[FM|M],Sumb1,0),
Sum1 is FL+Sumb1 ,Sum2 is FM+Sumb2,max(Sum1,Sum2,Sum).

%synarthsh upologismou athroismatos megistou monopatiou sugkrinontas ta upomonopatia kai kathe fora 
%epilegwntas to megalutero
doublechain2([],_,0,0).
doublechain2([FL|L],M,Sum,0):- not member(FL,M),doublechain2(L,M,Sumb1,0),Sum is Sumb1+FL.
doublechain2([FL|L],M,Sum,0):- member(FL,M),append(A,[FL|B],M),
doublechain2(L,M,Sumb1,0),doublechain2(L,B,Sumb2,1),max(Sumb1,Sumb2,Sumb),Sum is Sumb+FL.

doublechain2(_,[],0,1).
doublechain2(L,[FM|M],Sum,1):- not member(FM,L),doublechain2(L,M,Sumb1,1),Sum is Sumb1+FM.
doublechain2(L,[FM|M],Sum,1):- member(FM,L),append(A,[FM|B],L),
doublechain2(L,M,Sumb1,1),doublechain2(B,M,Sumb2,0),max(Sumb1,Sumb2,Sumb),Sum is Sumb+FM.



