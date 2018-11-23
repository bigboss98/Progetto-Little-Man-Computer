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
        execution_instuction(state(Acc, Pc, Mem, In,)).

% execution_instruction/2 permette di eseguire le istruzioni proprie del linguaggio macchina
% del LMC, ritorna un errore in caso l'istruzione non sia valida oppure si ha
% la presenza di un istruzione halt.
execution_instruction(state(), NewState):-
                      nth(Pc, Mem, Elem, []),%prelevo l'instruzione corrente dalla memoria
                      Elem / 100 = 1, !, %isolo il primo elemento per sapere la tipologia di istruzione
		      X = Elem mod 100,
                      sum(X, Acc, NewAcc, Flag).

/*Predicato state/6 per permette di rappresentare uno stato valido del LMC
 *Prevede gli seguenti argomenti, con vincoli:
 *Acc, indica l'accumulatore, ossia un numero tra 0 e 999
 *Pc(Program Counter) un numero tra 0 e 999
 *Mem composto da una lista di 100 numeri compresi tra 0 e 999 e rappresenta il
      contenuto in memoria nel LMC
 *In, lista di numeri tra 0 e 999, rappresenta la coda di input nel LMC
 *Out, lista di numeri tra 0 e 999, rappresenta la coda di output nel LMC
 *Flag può assumere solo i valori "flag" o "noflag".
 */
state(Acc, Pc, Mem, In, Out, Flag):-
              between(0, 999, Acc),
              between(0, 999, Pc),
              valid_lists(Mem, 100),%controllo lista
              valid_lists(In, length(In)), %controllo lista
              valid_lists(Out, length(Out)), %controllo lista
              valid_flag(Flag).

%Predicato valid_flag/1 indica se l'argomento è un valido flag, ossia assume come
%valore la stringa "flag" oppure la stringa "noflag"
valid_flag("noflag").
valid_flag("flag").

/* Predicato valid_lists/2(List, Length) indica se la lista è di lunghezza massima
 * pari a Length e i cui tutti gli elementi di List sono tra 0 e 999
 */
valid_lists([], 0).
valid_lists([X | Xs], Length):-
            length([X | Xs]) is Length,
            between(0, 999, X),
            valid_lists(Xs, Length-1).
/*Predicato halted_state/6 per rappresentare un stato in cui si è eseguito un
 *istruzione di halt ed ha gli stessi argomenti e vincoli del predicato state.
 */
halted_state(Acc, Pc, Mem, In, Out, Flag):-%da rivedere e discutere assieme!!!
             Pc / 100 is 0,
             state(Acc, Pc, Mem, In, Out, Flag).

/*Il predicato sum/4 permette l'implementazione della somma tra un registro e
 * l'accumulatore e salva il risultato ,modulo 1000 nell'accumulatore.
 * In caso la somma sia maggiore di 1000 viene settato il Flag altrimenti no.
 */
sum(Elem, Acc, NewAcc, Flag):-
          Result is (Elem + Acc),
          Result >= 1000, !,
          NewAcc is Result mod 1000.

sum(Elem, Acc, NewAcc, Flag):-
          NewAcc is (Elem + Acc),
          NewAcc < 1000.

%Predicato diff/4 per implementare la sottrazione tra un registro e l'accumulatore
% e salva il risultato nell'accumulatore
diff(Elem, Acc, NewAcc, Flag)
