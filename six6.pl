%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%To programma ylopoihthike se eclipse kai trexei kanonika,briskei thn beltisth lush me auta ta dedomena(ennoeitai pws douleuei kai gia alla) se xrono 33 sec ston ypologisth mou
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
:- set_flag(print_depth, 1000).
:-lib(ic).
:-lib(branch_and_bound).
:-lib(ic_cumulative).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
job(j1, [t11,t12,t13,t14]).
job(j2, [t21,t22,t23,t24]).
job(j3, [t31,t32,t33]).
job(j4, [t41,t42,t43]).
job(j5, [t51]).

task(t11, m1, 3, 3).
task(t12, m2, 2, 3).
task(t13, m1, 2, 3).
task(t14, m2, 3, 1).
task(t21, m1, 2, 2).
task(t22, m2, 3, 2).
task(t23, m1, 3, 1).
task(t24, m1, 4, 2).
task(t31, m1, 3, 1).
task(t32, m2, 1, 3).
task(t33, m2, 4, 3).
task(t41, m1, 1, 1).
task(t42, m1, 3, 2).
task(t43, m1, 2, 3).
task(t51, m2, 3, 2).

machine(m1, 2).
machine(m2, 2).

staff(10).
%-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Ylopoihsh tou basikou kathgorhmatos me epanaliptikh ekbathinsh tou eurous domain apo thn synarthsoula until
%arxika theouroume ws lush mono ta start time twn task me thn seira pou orizoun oi mhxanes dld prwta ola ths m1 meta ths m2 kok
%sthn synexeia kai afou brethei h lush tropopoieitai h lush gia na parei thn morfh pou theloume
%Ws tropos aksiologishs tou kostous thewrithike to end time

jobshop(Out,C):-maxdur(Max),until(Max,AR),bb_min(jobshoptmp(S,C,AR),C,_),!,formalism(S,Out).

jobshoptmp(S,MAX,AR):-not_overlaps(S,AR),precedence(S),generate(S),workers_limit(S),maxend(S,MAX).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Periorismoi gia to posa task apo idies mhxanes tha trexoun to ena panw sto allo kai arxikopoihsh ths lushs

not_overlaps(S,AR):-findall(M,machine(M,_),L),not_overlaps0(S,L,AR).

not_overlaps0(_,[],_).
not_overlaps0(S,[FM|RM],AR):- not_overlaps1(S1,FM,AR),not_overlaps0(S2,RM,AR),append(S1,S2,S).

not_overlaps1(S,M,AR):-tasksofmachine(M,TL),length(TL,LEN),length(S,LEN),S#::0..AR,machine(M,NM),
get_characteristic_list(TL,_,DL,_),full_of_SMTH(1,LEN,ML),cumulative(S,DL,ML,NM).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Synarthsh pou  ths dineis mia lista apo task kai sou epistrefei listes me ta xaraktiristika tous

get_characteristic_list([],[],[],[]).
get_characteristic_list([FT|TL],[FM|ML],[FD|DL],[FW|WL]):-task(FT,FM,FD,FW),get_characteristic_list(TL,ML,DL,WL).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Synarthsh pou briskei to megisto start time
maxdur(S):-findall(TL,job(_,TL),L),find_max(L,S).

find_max([],0).
find_max([F|Rest],Max):-get_characteristic_list(F,_,DL,_),sum(DL,Max1),find_max(Rest,Max2),max1([Max1,Max2],Max).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Elegxos gia to posh ergates ergazontai kathe stigmh

workers_limit(N):-findall(M,machine(M,_),L),workers_limit0(S1,L),get_characteristic_list(S1,ML,DL,WL),staff(STAFF),cumulative(N,DL,WL,STAFF).

workers_limit0([],[]).
workers_limit0(S,[FM|RM]):-tasksofmachine(FM,S1),workers_limit0(S2,RM),append(S1,S2,S).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Kathgorhma pou ftiaxnei pinaka megethous N gemato apo kati

full_of_SMTH(SMTH,1,[SMTH]):-!.
full_of_SMTH(SMTH,N,[SMTH|L]):-NN is N-1,full_of_SMTH(SMTH,NN,L).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Epistofh arithmwn apo to 1 ws kai to N
until(N,S):-till(N,O),member(S,O).

till(1,[1]):-!.
till(N,O):-NN is N-1,till(NN,L),append(L,[N],O).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Ylopoihsh kapoiwn xrhsimwn max

max1([X],X).
max1([X|Xs],X):- max1(Xs,Y), X >=Y.
max1([X|Xs],N):- max1(Xs,N), N > X.

%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
generate(Sol) :-search(Sol, 0, most_constrained,indomain_min, complete, []).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tasksofmachine(X,L):-findall(T,task(T,X,_,_),L).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Synathsh gia periorismo wste ta task na ginontai me thn seira pou prepei
precedence(S):-findall(TL,job(_,TL),L),pre1(L,S).

pre1([],S).
pre1([FR|RL],S):-pre2(FR,S),pre1(RL,S).

pre2([TaskA],_).
pre2([TaskA,TaskB|Rest],S):-getTN(TaskA,NumA),getTN(TaskB,NumB),task(TaskA,_,DA,_),task(TaskB,_,DB,_),
getS(S,NumA,SA),getS(S,NumB,SB),eval(SA+DA)#=EA,SB#>=EA,pre2([TaskB|Rest],S).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%boithitikes synarthseis
%dwthentos mias listas S kai enow arithmou N pernoume to n-osto Start time apo mia lista me start time
getS(S,N,Stime):-length(A,N),append(A,[Stime|B],S).

%Pernoume to seiriako arithmo enow task symfwna me thn seira apo machine (dld prwta ola ta task apo m1 meta apo m2 kok)
getTN(Task,Num):-findall(M,machine(M,_),L),workers_limit0(S1,L),append(X,[Task|_],S1),length(X,Num).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Synarthseis wste na kanoume to apotelesma sthn morfh pou theloume
%Arxika ftiaxnoume osa execs theloume gemizwntas ta to kathe ena me ola ta task apo tou tupou mhxanhs pou einai to exec me thn synasrthsh make it formal
%Sthn synexeia me thn synarthsh edit pou xrhsimopoiei mia parallagh ths bubblesort pou taksinomh ta task mesa sta execs kai mias synarthseis (seq)pou 
%spaei auth thn taksinomhmenh lista kai thn spaei katallila, se tosa kommatia wses kai h mhxanes kai h kathe mia pernei kai apo ena diaforetiko
 
formalism(S,Out):-make_it_formal(S,Out1),edit(Out1,Out,1).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
edit([FE],[O1],NUM):-FE=execs(MF,TLF),bubble_sort(TLF,Sorted),seq(Sorted,S1),part(NUM,S1,SFINAL),
O1=execs(MF,SFINAL).
edit([FE,SE|RE],[O1|RO],NUM):-FE=execs(MF,TLF),SE=execs(SM,TLF),SM=MF,NN is NUM+1,bubble_sort(TLF,Sorted),seq(Sorted,S1),part(NUM,S1,SFINAL),
O1=execs(MF,SFINAL),edit([SE|RE],RO,NN).
edit([FE,SE|RE],[O1|RO],NUM):-FE=execs(MF,TLF),SE=execs(SM,_),not(SM=MF),NN is 1,bubble_sort(TLF,Sorted),seq(Sorted,S1),part(NUM,S1,SFINAL),
O1=execs(MF,SFINAL),edit([SE|RE],RO,NN).

%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
make_it_formal(S,O):-findall(M,machine(M,_),L),make_it_formal0(S,L,O).

make_it_formal0([],[],[]).
make_it_formal0(S,[FM|ML],O):-tasksofmachine(FM,TL),length(TL,LEN),length(S1,LEN),machine(FM,NFM),full_of_SMTH(FM,NFM,LFM),append(S1,S2,S),
make_it_formal1(S1,LFM,TL,O1),make_it_formal0(S2,ML,O2),append(O1,O2,O).

make_it_formal1(_,[],_,[]).
make_it_formal1(S,[FM|RM],TList,[O1|RO]):-make_the_t(S,TList,THE_t),O1=execs(FM,THE_t),make_it_formal1(S,RM,TList,RO).

make_the_t([],[],[]).
make_the_t([S1|RS],[T1|RT],[t(T1,S1,E1)|RO]):-task(T1,_,D,_),E1 is S1+D,make_the_t(RS,RT,RO).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bubble_sort(List,Sorted):-b_sort(List,[],Sorted).
b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted):-bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],Sorted).
   
bubble(X,[],[],X).
bubble(X,[Y|T],[Y|NT],Max):-X=t(_,SX,EX),Y=t(_,SY,EY),EX>EY,bubble(X,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-X=t(_,SX,EX),Y=t(_,SY,EY),EX=<EY,bubble(Y,T,NT,Max).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
make_it_flow([S1],[S1],[]).
make_it_flow([S1|R],[S1|REST],OTHERS):-make_it_flow(R,REST,OTHERS),REST=[FR|Rest],S1=t(_,SS1,ES1),FR=t(_,FRS,FRE),TSK is (FRS-ES1),TSK>=0.
make_it_flow([S1|R],REST,[S1|OTHERS]):-make_it_flow(R,REST,OTHERS),REST=[FR|Rest],S1=t(_,SS1,ES1),FR=t(_,FRS,FRE),TSK is (FRS-ES1),TSK<0.

seq(S,[S1]):-make_it_flow(S,S1,[]).
seq(S,[S1|Other]):-make_it_flow(S,S1,Rest),seq(Rest,Other).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%synarthsh pou epistrefei to n-osto stoixeio mias listas
part(1,[X|_],X) :- !.
part(Idx,[_|List],X) :-Idx > 1,Idx1 is Idx-1,part(Idx1,List,X).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%synarthsh pou dinei apo mia lista metablhtwn xronwn enarksis to megisto xrono telous
maxend(S,MAX):-findall(M,machine(M,_),L),workers_limit0(S1,L),maxend1(S,S1,Sum),max1(Sum,MAX).

maxend1([],[],[]).
maxend1([SF|SR],[FT|RT],[S1|SO]):-task(FT,_,D,_),S1 is SF+D,maxend1(SR,RT,SO).
%------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------