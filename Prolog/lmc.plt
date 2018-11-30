%%%% -*- Mode:Prolog -*-
%%% File per effettuare i test della simulazione della macchina Little Man Computer
:-use_module(library(plunit)).
:-include('lmc.pl').
:-begin_tests(testLmc).

/*
 * Test per verificare che le istruzioni aritmetiche forniscono i risultati corretti
 */
 test(mathOperation):-
 	  one_instruction(state(2, 0, [202, 23, 45], [], [], noflag), X),
      one_instruction(state(3, 0, [101, 34], [], [], noflag), Y),
      one_instruction(state(3, 0, [104, 34, 345, 45, 999], [], [], noflag), Z),
      X = state(43, 1, [202, 23, 45], [], [], noflag),
      Y = state(37, 1, [101, 34], [], [], noflag),
      Z = state(2, 1, [104, 34, 345, 45, 999], [], [], flag).
 
:-end_tests(testLmc).
