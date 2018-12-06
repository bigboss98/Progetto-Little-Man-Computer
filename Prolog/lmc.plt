%%%% -*- Mode:Prolog -*-
%%% File per effettuare i test della simulazione della macchina Little Man Computer
:-use_module(library(plunit)).
:-include('lmc.pl').
:-begin_tests(testLmc).

/*
 * Test per verificare che le istruzioni aritmetiche forniscono i risultati corretti
 */
 test(mathOperation):-
 	  lmc:one_instruction(state(2, 0, [202, 23, 45], [], [], noflag), X),
      lmc:one_instruction(state(3, 0, [101, 34], [], [], noflag), Y),
      lmc:one_instruction(state(3, 0, [104, 34, 345, 45, 999], [], [], noflag), Z),
      X = state(43, 1, [202, 23, 45], [], [], noflag),
      Y = state(37, 1, [101, 34], [], [], noflag),
      Z = state(2, 1, [104, 34, 345, 45, 999], [], [], flag).
 
 test(memoryOperation):-
      lmc:one_instruction(state(12, 0, [301, 0], [], [], noflag), Res1),
      lmc:one_instruction(state(2, 0, [502, 34, 567], [], [], noflag), Res2),
      %\+ lmc:one_instruction(state(4, 0, [503, 0, 45], [], [], noflag), X),%deve fallire perchè manca elemento in memoria
      %\+ lmc:one_instruction(state(2, 0, [], [], [], noflag), Y), %deve fallire perchè la memoria è vuota
      Res1 = state(12, 1, [301, 12], [], [], noflag),
      Res2 = state(567, 1, [502, 34, 567], [], [], noflag).

test(branch):-
     lmc:one_instruction(state(3, 0, [603, 45, 678], [], [], noflag), Res1),
     lmc:one_instruction(state(4, 0, [603], [], [], noflag), Res2),
     Res1 = state(3, 3, [603, 45, 678], [], [], noflag),
     Res2 = state(4, 3, [603], [], [], noflag),
     lmc:one_instruction(state(2, 0, [703, 45, 456, 678], [], [], noflag), Res3),
     lmc:one_instruction(state(0, 0, [703, 45], [], [], noflag), Res4),
     lmc:one_instruction(state(0, 0, [703], [], [], flag), Res5),
     lmc:one_instruction(state(0, 0, [803, 56], [], [], noflag), Res6),
     lmc:one_instruction(state(2, 0, [803], [], [], flag), Res7),
     lmc:one_instruction(state(2, 0, [804], [], [], noflag), Res8),
     Res3 = state(2, 1, [703, 45, 456, 678], [], [], noflag),
     Res4 = state(0, 3, [703, 45], [], [], noflag),
     Res5 = state(0, 1, [703], [], [], flag),
     Res6 = state(0, 1, [803, 56], [], [], noflag),
     Res7 = state(2, 1, [803], [], [], flag),
     Res8 = state(2, 4, [804], [], [], noflag).


test(inputOutput):-
     lmc:one_instruction(state(3, 0, [901], [456], [], noflag), Res1),  
     lmc:one_instruction(state(45, 0, [902], [], [], noflag), Res2),
     \+ lmc:one_instruction(state(3, 0, [901], [], [], noflag), X),  
     Res2 = state(45, 1, [902], [], [45], noflag),
     Res1 = state(456, 1, [901], [], [], noflag).
     

test(halt):-
     lmc:one_instruction(state(3, 0, [045], [], [], noflag), Res1),
     lmc:one_instruction(state(4, 0, [048], [], [], noflag), Res2),
     Res1 = halted_state(3, 1, [45], [], [], noflag),
     Res2 \= state(4, 1, [48], [], [], noflag).


test(executionLoop):- 
    \+ lmc:execution_loop(state(0, 0, [456, 234, 123], [], [], noflag), X),
    lmc:execution_loop(state(0, 0, [901, 315, 901, 711, 214, 316, 517, 115, 516, 603, 517, 902, 0, 1, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [41, 42], [], noflag), Out1),
    Out1 = [722].
    

%test(loadFiles):-
    %lmc_load("test1.lmc", Mem).
:-end_tests(testLmc).
