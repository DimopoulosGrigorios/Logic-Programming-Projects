%ylopoihthike se eclipse
:-lib(ic).

%============================================================================================================================================================================

fraction3((A/BC,D/EF,G/HI)):-Solution=[A,B,C,D,E,F,G,H,I],Solution#::1..9,constrain3(Solution),generate3(Solution),
BC is (B*10+C),EF is (E*10+F),HI is(H*10+I).

%periorismoi gia na einai ola diaforetika kai ta klasmata na mas kanoun 1,opws epishs kai gia na exoume emfanish kathe lushs xwris epanalipsh
constrain3([A,B,C,D,E,F,G,H,I]):-alldifferent([A,B,C,D,E,F,G,H,I]),
A#>D,D#>G,
(A*(E*10+F)*(H*10+I)+
D*(B*10+C)*(H*10+I)+
G*(B*10+C)*(E*10+F))#=
((E*10+F)*(B*10+C)*(H*10+I)).

generate3(Sol) :-search(Sol, 0, first_fail, indomain, complete, []).

%============================================================================================================================================================================

fraction2((A/BC,D/EF)):-Solution=[A,B,C,D,E,F],Solution#::1..9,constrain2(Solution),generate2(Solution),BC is (B*10+C),EF is (E*10+F).

%periorismoi gia na einai ola diaforetika kai ta klasmata na mas kanoun 1,opws epishs kai gia na exoume emfanish kathe lushs xwris epanalipsh
constrain2([A,B,C,D,E,F]):-alldifferent([A,B,C,D,E,F]),A#>D,
(A*(E*10+F)+D*(B*10+C))#=
((E*10+F)*(B*10+C)).

generate2(Sol) :-search(Sol, 0, first_fail, indomain, complete, []).

%============================================================================================================================================================================

fraction4((A/BC,D/EF,G/HI,J/KL)):-Solution=[A,B,C,D,E,F,G,H,I,J,K,L],Solution#::1..9,constrain4(Solution),generate4(Solution),
BC is (B*10+C),EF is (E*10+F),HI is(H*10+I),KL is (K*10+L).

%pio eksidhkeumenoi periorismoi pou ylopoiounte me tis synarthseis ltn kai con alla kai ton klassiko periorismo gia to athrisma twn klasmatwn iso me 1
constrain4([A,B,C,D,E,F,G,H,I,J,K,L]):-con([A,B,C,D,E,F,G,H,I,J,K,L]),
ltn([A,B,C],ABC),ltn([D,E,F],DEF),ltn([G,H,I],GHI),ltn([J,K,L],JKL),
ABC#>=DEF,DEF#>=GHI,GHI#>=JKL,
(A*(E*10+F)*(H*10+I)*(K*10+L)+
D*(B*10+C)*(H*10+I)*(K*10+L)+
G*(B*10+C)*(E*10+F)*(K*10+L)+
J*(E*10+F)*(B*10+C)*(H*10+I))#=
((E*10+F)*(B*10+C)*(H*10+I)*(K*10+L)).

generate4(Sol) :-search(Sol, 0, most_constrained, indomain, complete, []).


%============================================================================================================================================================================

fraction5((A/BC,D/EF,G/HI,J/KL,M/NO)):-Solution=[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],Solution#::1..9,constrain5(Solution),generate5(Solution),
BC is (B*10+C),EF is (E*10+F),HI is(H*10+I),KL is (K*10+L),NO is (N*10+O).

%pio eksidhkeumenoi periorismoi pou ylopoiounte me tis synarthseis ltn kai con alla kai ton klassiko periorismo gia to athrisma twn klasmatwn iso me 1
constrain5([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):-con([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]),
ltn([A,B,C],ABC),ltn([D,E,F],DEF),ltn([G,H,I],GHI),ltn([J,K,L],JKL),ltn([M,N,O],MNO),
ABC#>=DEF,DEF#>=GHI,GHI#>=JKL,JKL#>=MNO,
(A*(E*10+F)*(H*10+I)*(K*10+L)*(N*10+O)+
D*(B*10+C)*(H*10+I)*(K*10+L)*(N*10+O)+
G*(B*10+C)*(E*10+F)*(K*10+L)*(N*10+O)+
J*(E*10+F)*(B*10+C)*(H*10+I)*(N*10+O)+
M*(E*10+F)*(B*10+C)*(H*10+I)*(K*10+L))#=
((E*10+F)*(B*10+C)*(H*10+I)*(K*10+L)*(N*10+O)).
generate5(Sol) :-search(Sol, 0,most_constrained, indomain, complete, []).

%============================================================================================================================================================================

fraction6((A/BC,D/EF,G/HI,J/KL,M/NO,P/QR)):-Solution=[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],Solution#::1..9,constrain6(Solution),generate6(Solution),
BC is (B*10+C),EF is (E*10+F),HI is(H*10+I),KL is (K*10+L),NO is (N*10+O),QR is (Q*10+R).

%pio eksidhkeumenoi periorismoi pou ylopoiounte me tis synarthseis ltn kai con alla kai ton klassiko periorismo gia to athrisma twn klasmatwn iso me 1
constrain6([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):-con([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]),
ltn([A,B,C],ABC),ltn([D,E,F],DEF),ltn([G,H,I],GHI),ltn([J,K,L],JKL),ltn([M,N,O],MNO),ltn([P,Q,R],PQR),
ABC#>=DEF,DEF#>=GHI,GHI#>=JKL,JKL#>=MNO,MN0#>PQR,
(A*(E*10+F)*(H*10+I)*(K*10+L)*(N*10+O)*(Q*10+R)+
D*(B*10+C)*(H*10+I)*(K*10+L)*(N*10+O)*(Q*10+R)+
G*(B*10+C)*(E*10+F)*(K*10+L)*(N*10+O)*(Q*10+R)+
J*(E*10+F)*(B*10+C)*(H*10+I)*(N*10+O)*(Q*10+R)+
M*(E*10+F)*(B*10+C)*(H*10+I)*(K*10+L)*(Q*10+R)+
P*(E*10+F)*(B*10+C)*(H*10+I)*(K*10+L)*(N*10+O))#=
((E*10+F)*(B*10+C)*(H*10+I)*(K*10+L)*(N*10+O)*(Q*10+R)).

generate6(Sol) :-search(Sol, 0, most_constrained, indomain, complete, []).

%============================================================================================================================================================================

%synarthsh pou apaitei mia lista na xei to poly 2 fores ena stoixeio
con(List):-length(A,9),append(A,B,List),alldifferent(A),alldifferent(B).

%synarths gia  na pernoume tria psifia se ena arithmo ABC wste na kanoume ton elegxo oti ta fractions den tha typwnoun kapoio permutation ths lushs
ltn([A,B,C],N):-N #= ((A*100)+(B*10)+C).

