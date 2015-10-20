%Ylopoihthike se eclipse
:- set_flag(print_depth, 1000).
:-lib(ic).   
:-lib(branch_and_bound). 

musicians([[1,1,0,1,0,1,1,0,1],
           [1,1,0,1,1,1,0,1,0],
           [1,1,0,0,0,0,1,1,0],
           [1,0,0,0,1,1,0,0,1],
           [0,0,1,0,1,1,1,1,0]]).

durations([2,4,1,3,3,2,5,7,6]).
%Basiko kathgorhma ths askhshs
rehearsal(Sequence,Costos):-durations(Dlist),
length(Dlist,N),length(Sequence,N),Sequence:: 1..N,constrain(Sequence),
bb_min(generate(Sequence,Costos),Costos,_).

constrain(List):-alldifferent(List).

generate(Sequence,Costos) :-durations(OldDurations),musicians(M),search(Sequence, 0, first_fail, indomain, complete,[] ),
music_t(Sequence,M,Nt),seq(Sequence,OldDurations,Durations),cost(Nt,Durations,Costos).


%========================================================================================================================================================   
%% Utils:
%========================================================================================================================================================

%kathgorhma gia ola ta kosth twn mousikwn
cost([],D,0):-!.
cost([M1|RM],D,Costos):-cost(RM,D,COST),length(M1,N),sum2(M1,ACES),mus_cost(0,ACES,M1,D,MC),Costos is (MC+COST).

%Kathgorhma pou epistrefei to kostos gia ena mousiko
mus_cost(_,_,[],[],0).
mus_cost(0,A,[0|P],[DN|D],MC2):-!,mus_cost(0,A,P,D,MC2).
mus_cost(B,0,[0|P],[DN|D],MC2):-!,mus_cost(B,0,P,D,MC2).
mus_cost(B,A,[0|P],[DN|D],MC):-mus_cost(B,A,P,D,MC2),MC is (MC2+((B>0) and (A>0))*DN).
mus_cost(B,A,[1|P],[DN|D],MC2):-!,BB is (B+1),AA is A-1,mus_cost(BB,AA,P,D,MC2).

%boithitiko kathgorhma
sum2([],0).
sum2([A|List],S):-sum2(List,R),S is (A + R).

%Synarthsh pou ftiaxnei twn pinaka twn mousikwn kai twn merwn opws tha prepe na htan symfwna me thn akolouthia sthlwn S.
music_t(S,MU,M):-matr_transp(MU,CMU),seq(S,CMU,SCMU),matr_transp(SCMU,M).

%Synarthsh pou taktopoiei mia lista A sumfwna me to prwto orisma pou einai mia akolouthia me ton arithmo apo ta stoixeia me thn seira pou tha mpoun
seq([],A,[]).
seq([FS|S],A,[FN|N]):-part(FS,A,FN),seq(S,A,N).

%synarthsh pou epistrefei to n-osto stoixeio mias listas
part(1,[X|_],X) :- !.
part(Idx,[_|List],X) :-Idx > 1,Idx1 is Idx-1,part(Idx1,List,X).
			 
%Boithitikes synarthseis gia metasxhmatismo pinakawn
%Perneis thn prwth sthlh enos pinaka
f_c_o([],[],[]).
f_c_o([[T]|Rest],Fc,[]):- !,f_c_o(Rest,Fct,R1),append([T],Fct,Fc).
f_c_o([[T|X]|Rest],Fc,R):- f_c_o(Rest,Fct,R1),append([X],R1,R),append([T],Fct,Fc).

%Kanei swap tis grammes se sthles enos pinaka
matr_transp([],[]):-!.
matr_transp(A,B):- f_c_o(A,Fc1,Rest),matr_transp(Rest,Restb),append([Fc1],Restb,B).
