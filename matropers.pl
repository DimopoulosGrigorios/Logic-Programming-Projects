%_________________________________________1.1____________________________________________
%________________________________________________________________________________________



%========================================================================================================================================================
%kommati kwdika pou xwrizei se ena disdiastato pinaka thn prwth sthlh me ton ypoloipo pinaka

f_c_o([],[],[]).
f_c_o([[T]|Rest],Fc,[]):- !,f_c_o(Rest,Fct,R1),append([T],Fct,Fc).
f_c_o([[T|X]|Rest],Fc,R):- f_c_o(Rest,Fct,R1),append([X],R1,R),append([T],Fct,Fc).
%========================================================================================================================================================
matr_transp([],[]):-!.
matr_transp(A,B):- f_c_o(A,Fc1,Rest),matr_transp(Rest,Restb),append([Fc1],Restb,B).
%========================================================================================================================================================



%_________________________________________1.2____________________________________________
%________________________________________________________________________________________



%========================================================================================================================================================
%ekswteriko ginomeno dyo dianysmatwn

sum_of_2list_mult([],[],0).
sum_of_2list_mult([A|A_Rest],[B|B_Rest],S):-sum_of_2list_mult(A_Rest,B_Rest,T),P is A*B,S is P+T.
%========================================================================================================================================================
%boithitikh synarthsh gia ypologismo ginomenou listas me pinaka

sum_list_of_table([],List,[]).
sum_list_of_table(Table,List,Out):-f_c_o(Table,F_C_T,Rest),sum_list_of_table(Rest,List,O_Rest),!,sum_of_2list_mult(F_C_T,List,O_1),append([O_1],O_Rest,Out).
%========================================================================================================================================================
matr_mult([],B,[]).
matr_mult([A_1|A_Rest],B,[Out_Head|Out_Rest]):-matr_mult(A_Rest,B,Out_Rest),sum_list_of_table(B,A_1,O_1),append(Out_Head,[],O_1).
%========================================================================================================================================================



%_________________________________________1.3____________________________________________
%________________________________________________________________________________________



%========================================================================================================================================================
delete_row([],_,[]).
delete_row([A|Table],0,Table):-!.
delete_row([A|Table],Num,New):-K is Num-1,delete_row(Table,K,New1),append([A],New1,New).
%========================================================================================================================================================
delete_column(Table,Num,New):-matr_transp(Table,New1),delete_row(New1,Num,New2),matr_transp(New2,New).
%========================================================================================================================================================
shift([A|AA],New):-append(AA,[A],New).
%========================================================================================================================================================
many_shift(A,A,0):-!.
many_shift(A,New,Num):-shift(A,New1),New_num is Num-1,many_shift(New1,New,New_num).
%========================================================================================================================================================
table_val(A,Row,Column,G):-many_shift(A,[B|Rest],Row),many_shift(B,[G|Rest2],Column).
%========================================================================================================================================================
power(A,0,1):-!.
power(A,B,O):-C is B -1,power(A,C,O1),O is O1*A.
%========================================================================================================================================================
deti([[A]],A,Num,Depth):-!.

deti(A,Out,Num,Depth):-power((-1),Num,Pr) ,table_val(A,0,Num,G),
Num < Depth,
Nnum is Num + 1,
delete_column(A,Num,New),
delete_row(New,0,Table),
NDepth is Depth -1,
deti(Table,Out1,0,NDepth),
deti(A,Out2,Nnum,Depth),
Out is (Out1*G*Pr+Out2).

deti(A,0,Num,Depth):-Num >= Depth.
%========================================================================================================================================================
matr_det(A,Out):-length(A,Len),deti(A,Out,0,Len).
%========================================================================================================================================================