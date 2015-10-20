% STHN ERGASIA TRIA EGINE MONO TO PRWTO MEROS,DYSTYXWS APO APROSEKSIA MOU DEN EIXA DEI KAPOIA DEDOMENA THS ASKHSHS KAI ARXISA NA THN YLOPOIW
% EXWNTAS STO MYALO MOY KATI ALLO APO AYTO POU ZHTOYSE H ASKHSH,EN TELEI XARH STHN BOITHEIA THS LISTAS BRIKA TI DEN EIXA KATALABEI ALLA DYSTYXWS
% DEN MOU EFTANE O XRONOS NA ALLAKSW OLH THN YLOPOIHSH DIOTI TO KATALABA THN TELEUTAIA MERA,ETSI APLA PERIORISA TO EYROS TON APANTHSEWN MOY,
% ME MERIKES SYNARTHSEIS STO TELOS AFOY TO NA ALLAZA OLH THN YLOPOIHSH SE MERIKES WRES HTAN ADYNATO.WS APOTELESMA TWN PARAPANW TO PROPGRAMA EINAI POLY ARGO,
% AN DOKIMASETAI NA TO TREKSETAI ME TA DEDOMENA TOY PARADEIGMATOS DEN THA TREKSEI KAN,ME MIKROTERA TEST_CASES MOU ETREKSE.
% DOKIMASTE TO PARAKATW TEST CASE AN THELETAI.SYGGNWMH GIA THN TALAIPORIA

%=========================================================3.1=============================================================================================
% DEDOMENA ASKHSHS

job(j1,[t11,t12]). 
job(j2,[t21,t22]). 
job(j3,[t31]). 
 
 
task(t11,m1,2). 
task(t12,m2,6). 
task(t21,m2,5). 
task(t22,m1,3). 
task(t31,m2,4). 
 
machine(m1,1). 
machine(m2,2). 
 
deadline(10).
 
%========================================================================================================================================================
% SYNARTHSH H OPOIA  EPISTREFEI OLA TA TASK POU ANTISTIXOUN SE MIA MHXANH

tasksofmachine(X,L):-findall(T,task(T,X,_),L).
%========================================================================================================================================================
% SYNARTHSH POU PERNEI MIA LISTA APO TASKS KAI THN METATREPEI SE FORMAL MORFH(t(task,Starttime,Endtime))

change_to_formal([],[]).
change_to_formal([F_o|TaskList],[F_n|NewTaskList]):-change_to_formal(TaskList,NewTaskList),F_n=t(F_o,St,Et).
%========================================================================================================================================================
del(X, [X|Tail], Tail).
del(X, [Y|Tail], [Y|Tail1]):-del(X, Tail, Tail1).

list_del([],Y,Y).
list_del([FX|X],Y,NEW):-del(FX,Y,NEWY),list_del(X,NEWY,NEW).

insert(X, List, BiggerList):-del(X, BiggerList, List).

permutation([], []).
permutation([X|L], P) :-permutation(L, L1),insert(X, L1, P).
%========================================================================================================================================================
% ylopoihsh synarthshs gia epistrofh arithmwn apo to 0-n

num_to_n(0,[0]):-!.
num_to_n(N,[Num|Rest]):-Num is N,NN is N-1,num_to_n(NN,Rest).

num_n(N,Num):-N>0,num_to_n(N,L),member(Num,L).
num_n(N,0):-!,N=<0,fail.
%========================================================================================================================================================
% pairnei mia lista apo formal tasks kai tis anathetei xronous enarkshs kai liksis

time_a_line([],[],_).
time_a_line([t(Task,_,_)|Line],[t(Task,St_f,Et)|TimedLine],St):-deadline(D),StR is D-St,num_n(StR,Range),St_f is St+Range,task(Task,_,Duration),Et is St_f+Duration,Et=<D,time_a_line(Line,TimedLine,Et).
%========================================================================================================================================================
% anathetei xronous se kommenh lista(lista listwn formal task)

time_the_breaked_list([],[]).
time_the_breaked_list([BL_F|BL_R],[TL_F|TL_R]):-time_a_line(BL_F,TL_F,0),time_the_breaked_list(BL_R,TL_R).
%========================================================================================================================================================
% kobei mia formal task list se N kommatia

break_in(1,P,[P]):-!.
break_in(2,P,[F,L]):-!,append(F,L,P).
break_in(N,P,[F|Rest]):-!,break_in(2,P,[F,Rest_first]),N_1 is N-1,break_in(N_1,Rest_first,Rest).
%========================================================================================================================================================
% pernei mia lista apo tasks(aplh) kai thn mhxanh pou anoikoun ayta ta tasks kai epistrefei thn kommenh se oses einai h mhxanh autou tou typou xronismenh kai formal tasklist 

timed_perm(X,List,TimedList):-permutation(List, P),machine(X,Num),break_in(Num,P,Breaked_List),time_the_breaked_list(Breaked_List,TimedList).
%========================================================================================================================================================
add(X, L, [X|L]).

add_list(_,0,L,L):-!.
add_list(F,N,L,New):-NN is N-1,add_list(F,NN,L,New1),add(F,New1,New).
%========================================================================================================================================================
% ylopoihsh synarthshs gia na pernoume lista me oles tis mhxanes

machine_list([],[]).
machine_list([F|L],New):-machine(F,Num),add_list(F,Num,[],New1),machine_list(L,New2),append(New1,New2,New).

machine_list2(New):-findall(X,machine(X,_),L),machine_list(L,New).
%========================================================================================================================================================
% synarthsh pou dhmiourgei adeia execs ,sto pedio twn listwn apo formal task ,osa kai oi mhxanes  

create_empty_execs([],[]).
create_empty_execs([FML|ML],[FEL|EL]):-FEL=execs(FML,_),create_empty_execs(ML,EL).
%========================================================================================================================================================
% anamesa se dyo listes apo execs na mhn yparxei kati idio

nothing_same_ex(_,[]).
nothing_same_ex([],_).
nothing_same_ex([FL|L],[FM|M]):-FL=execs(MC1,List_FL),FM=execs(MC2,List_FM),nothing_same_t(List_FM,List_FL),nothing_same_ex(L,[FM|M]),nothing_same_ex([FL|L],M).
%========================================================================================================================================================
% anamesa se dyo listes apo formal task na mhn yparxei kanena idio

nothing_same_t(_,[]).
nothing_same_t([],_).
nothing_same_t([FL|L],[FM|M]):-FL=t(TFL,ST1,EN1),FM=t(TFM,ST2,EN2),not(TFL=TFM),nothing_same_t(L,[FM|M]),nothing_same_t([FL|L],M).
%========================================================================================================================================================
% synarthsh pou gemizei ta execs

empty_execs_to_full([],[],_,_):-!.
empty_execs_to_full([FE|Eexecs],[FF|Fexecs],Previous_Machine,[FSTL|Seed_Task_List]):-FE=execs(Machine,X),Previous_Machine=Machine,X=FSTL,FE=FF,empty_execs_to_full(Eexecs,Fexecs,Machine,Seed_Task_List),nothing_same_ex([FF],Fexecs).
empty_execs_to_full([FE|Eexecs],[FF|Fexecs],Previous_Machine,Seed_Task_List):-FE=execs(Machine,X),not(Previous_Machine=Machine),tasksofmachine(Machine,TaskList),change_to_formal(TaskList,FTaskList),timed_perm(Machine,FTaskList,[X|TimedList]),FE=FF,empty_execs_to_full(Eexecs,Fexecs,Machine,TimedList),nothing_same_ex([FF],Fexecs).
%========================================================================================================================================================
% synarthsh pou tsekarei an xrhsimopoihsame ola ta tasks

use_them_all_check([],[]).
use_them_all_check([],B):-!,length(B,LEN),LEN>0,fail.
use_them_all_check([FS|S],List_of_tasks_formal):-FS=execs(_,TaskList),list_del(TaskList,List_of_tasks_formal,NLotf),use_them_all_check(S,NLotf).
%========================================================================================================================================================
% synarthsh gia ebresh se mia formal task list enos formal task(xronismeno) dothentos enos aplou task

search_for_task_in_tasklist([t(Current_task,St,ET)|Rest_task],Current_task,t(Current_task,St,ET)).
search_for_task_in_tasklist([t(Current_task,St,ET)|Rest_task],Given_task,RTRN):-not(Given_task=Current_task),search_for_task_in_tasklist(Rest_task,Given_task,RTRN).
%========================================================================================================================================================
% synarthsh gia ebresh se mia lista apo execs enos formal task dothentos enos aplou task

search_for_task_in_execlist([FS|Rest_execs],Given_task,RTRN):-FS=execs(_,Task_List),search_for_task_in_tasklist(Task_List,Given_task,RTRN).
search_for_task_in_execlist([FS|Rest_execs],Given_task,RTRN):-!,FS=execs(_,Task_List),not(search_for_task_in_tasklist(Task_List,Given_task,Smth)),search_for_task_in_execlist(Rest_execs,Given_task,RTRN).
%========================================================================================================================================================
% ylopoihsh synarthshs pou tsekarei an mia lush einai swsth ws pros thn seira pou prepei na ektelesthoun ta tasks

time_order_check(S,[]).
time_order_check(S,[A]).
time_order_check(S,[A,B|Tasklist]):-search_for_task_in_execlist(S,A,t(A,StA,EtA)),search_for_task_in_execlist(S,B,t(B,StB,EtB)),EtA=<StB,time_order_check(S,[B|Tasklist]).

time_order_check_final(S):-job(J,Tasklist),time_order_check(S,Tasklist).

time_order(S):-findall(S,time_order_check_final(S),L),length(L,LENL),findall(J,job(J,TSK),M),length(M,LENM),LENM=LENL.
%========================================================================================================================================================
job_shop(S):-machine_list2(New),create_empty_execs(New,Eexecs),empty_execs_to_full(Eexecs,S,whatever,[]),tasksofmachine(M,TaskList),change_to_formal(TaskList,FTaskList),use_them_all_check(S,FTaskList),time_order(S).


