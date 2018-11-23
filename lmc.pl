%%%% -*- Mode:Prolog -*-

% one_instruction(State, NewState) serve per simulare/eseguire un'istruzione del LMC
% Prevede che State e NewState siano stati validi del LMC  e comporta l'esecuzione
% dell'istruzione che porta da State a NewState.
% Il predicato fallisce nei seguenti casi:
% - lo stato State è un halting_state, ossia il sistema si è arrestato
% - l'istruzioni è di input ma la coda di input è vuota
% - l'istruzione corrente non è valida.
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(NewAcc, NewPc, NewMem, NewIn, NewOut, NewFlag)):-
        execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                              state(NewAcc, NewPc, NewMem, NewIn, NewOut, NewFlag)).

% execution_instruction/2 permette di eseguire le istruzioni proprie del linguaggio macchina
% del LMC, ritorna un errore in caso l'istruzione non sia valida oppure si ha
% la presenza di un istruzione halt.
execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, NewPc, NewMem, NewIn, NewOut, NewFlag)):-
                      nth0(Pc, Mem, Elem, R),%prelevo l'instruzione corrente dalla memoria
                      1 is (Elem / 100), !, %isolo il primo elemento per sapere la tipologia di istruzione
		                  X == Elem mod 100,
                      sum(X, Acc, NewAcc, Flag).


/*Il predicato sum/4 permette l'implementazione della somma tra un registro e
 * l'accumulatore e salva il risultato ,modulo 1000 nell'accumulatore.
 * In caso la somma sia maggiore di 1000 viene settato il Flag altrimenti no.
 */
sum(Elem, Acc, NewAcc, NewFlag):-
          Result is (Elem + Acc),
          Result >= 1000, !,
          NewFlag = string("flag"),
          NewAcc is Result mod 1000.

sum(Elem, Acc, NewAcc, NewFlag):-
          NewAcc is (Elem + Acc),
          NewAcc < 1000,
          NewFlag = string("noflag").

%Predicato diff/4 per implementare la sottrazione tra un registro e l'accumulatore
% e salva il risultato nell'accumulatore
diff(Elem, Acc, NewAcc, NewFlag):-
          Result is (Elem - Acc),
          Result < 0, !,
          NewFlag = string("flag"),
          NewAcc is Result mod 1000.

diff(Elem, Acc, NewAcc, NewFlag):-
          NewAcc is (Elem - Acc),
          NewAcc >= 0,
          NewFlag = string("noflag").
