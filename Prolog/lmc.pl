%%%% -*- Mode:Prolog -*-

% one_instruction(State, NewState) serve per simulare/eseguire un'istruzione del LMC
% Prevede che State e NewState siano stati validi del LMC  e comporta l'esecuzione
% dell'istruzione che porta da State a NewState.
% Il predicato fallisce nei seguenti casi:
% - lo stato State è un halting_state, ossia il sistema si è arrestato
% - l'istruzioni è di input ma la coda di input è vuota
% - l'istruzione corrente non è valida.
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                state(NewAcc, NewPc, Mem, In, Out, NewFlag)):-
        execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                              state(NewAcc, NewPc, NewMem, NewIn, NewOut, NewFlag)).

/*one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                halted_state(NewAcc, NewPc, NewMem, NewIn, NewOut, NewFlag):-
                  nth0().
*/

/*execution_instruction/2 permette di eseguire le istruzioni del linguaggio macchina
 * del LMC, ritorna un errore in caso l'istruzione non sia valida oppure si ha
 * la presenza di un istruzione halt.
 */
execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, NewPc, Mem, In, Out, NewFlag)):-
            nth0(Pc, Mem, Elem, R),%prelevo l'instruzione corrente dalla memoria
            write(Elem / 100 is 1), !, %isolo il primo elemento per sapere la tipologia di istruzione
		        write(Elem mod 100 is X),
            sum(X, Acc, NewAcc, NewFlag),
            NewPc is Pc + 1. %aggiorno il Program Counter

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, NewPc, Mem, In, Out, NewFlag)):-
            nth0(Pc, Mem, Elem, R),%prelevo l'instruzione corrente dalla memoria
            Elem / 100 is 2, !, %isolo il primo elemento per sapere la tipologia di istruzione
            X is Elem mod 100,
            diff(X, Acc, NewAcc, NewFlag),
            NewPc is Pc + 1. %aggiorno il Program Counter

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, NewPc, NewMem, In, Out, Flag)):-
                      nth0(Pc, Mem, Elem, R),
                      Elem / 100 is 3, !,
                      Elem mod 100 is X,
                      store(X, Acc, Mem, NewMem),
                      NewPC is Pc + 1.

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, NewPc, Mem, In, Out, Flag)):-
                      nth0(Pc, Mem, Elem, R),
                      Elem / 100 is 5, !,
                      Elem mod 100 is X,
                      load(X, Acc, Mem, NewAcc).

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, NewPc, Mem, In, Out, Flag)):-
                      nth0(Pc, Mem, Elem, R),
                      Elem / 100 is 6, !,
                      Elem mod 100 is X,
                      branch(X, NewPc).

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, NewPc, Mem, In, Out, Flag)):-
                      nth0(Pc, Mem, Elem, R),
                      Elem / 100 is 7, !,
                      Elem mod 100 is X,
                      % .
/*Il predicato sum/4 permette l'implementazione della somma tra un registro e
 * l'accumulatore e salva il risultato ,modulo 1000 nell'accumulatore.
 * In caso la somma sia maggiore di 1000 viene settato il Flag altrimenti no.
 */
sum(Elem, Acc, NewAcc, Flag):-
          Result is (Elem + Acc),
          Result >= 1000, !,
          Flag = string("flag"),
          NewAcc is Result mod 1000.

sum(Elem, Acc, NewAcc, Flag):-
          NewAcc is (Elem + Acc),
          Flag = string("noflag").



%Predicato diff/4 per implementare la sottrazione tra un registro e l'accumulatore
% e salva il risultato nell'accumulatore
diff(Elem, Acc, NewAcc, Flag):-
        Result is (Elem - Acc),
        Result < 0, !,
        Flag = string("flag"),
        NewAcc is Result mod 1000.

diff(Elem, Acc, NewAcc, Flag):-
        NewAcc is (Elem - Acc),
        Flag = string("noflag").

/*predicato store/4 per salvare nella cella di memoria di indirizzo Elem,
 *il valore contenuto nell'accumulatore Acc e rimane inalterato l'accumulatore.
 */
 store(Elem, Acc, Mem, NewMem):-
       replace(Acc, Mem, Elem, NewMem).

/* Predicato load/4 per prelevare un elemento dalla memoria e lo salva nell'
 * accumulatore; il contenuto in memoria rimane inalterato
 */
 load(Elem, Acc, Mem, NewAcc):-
      nth0(Elem, Mem, NewAcc).

/* Predicato branch/2 per effettuare un salto non condizionale in cui viene
 * impostato il Program Counter al valore passato come argomento chiamato Elem
 */
 branch(Elem, Elem).
