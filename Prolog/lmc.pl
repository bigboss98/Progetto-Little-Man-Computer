%%%% -*- Mode:Prolog -*-

/*
 * Predicato one_instruction/2(State, NewState) serve per simulare/eseguire un'istruzione del LMC
 * Prevede che State e NewState siano stati validi del LMC  e comporta l'esecuzione
 * dell'istruzione che porta State a NewState.
 * Il predicato fallisce nei seguenti casi:
 * - l'istruzioni è di input ma la coda di input è vuota
 * - l'istruzione corrente non è valida.
 * In caso in cui viene eseguita un'istruzione di halt NewState è di tipo halted_state
 * altrimenti NewState è di tipo state.
 */
one_instruction(State, NewState):-
        execution_instruction(State, NewState).

/* 
 * Predicato execution_loop/2(State, Out) per eseguire una sequenza di istruzioni 
 * Prevede la definizione di 2 regole:
 * - la prima per la gestione del fatto che sia stata appena eseguita un'istruzione di halt
 *   per cui il programma termina, mostrando la coda di Output dello stato.
 * - la seconda prevede di eseguire una certa serie di istruzioni fino a quando State è di tipo state
 *   e alla fine dell'esecuzione viene ritornata la coda di output 
*/
execution_loop(halted_state(Acc, Pc, Mem, In, Out, Flag), Out).

execution_loop(state(Acc, Pc, Mem, In, Out, Flag), Out):-
               one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NewState),
               execution_loop(NewState, Out).


/* 
 * Predicato execution_instruction/2(State, NewState) esegue un'istruzione del LMC, scegliendo
 * in base allo stato quale eseguire; prevede 10 definizioni quante sono le istruzioni previste nel LMC:
 * La struttura del predicato prevede il prelevamento dell'istruzione della memoria, il riconoscimento
 * dell'istruzione e poi l'effettiva esecuzione dell'istruzione.
 */
execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, NewPc, Mem, In, Out, NewFlag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 1, Rest), !,
          nth0(Rest, Mem, Num, R1), 
          sum(Num, Acc, NewAcc, NewFlag),
          NewPc is (Pc + 1) mod 100. 

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, NewPc, Mem, In, Out, NewFlag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 2, Rest), !,
          nth0(Rest, Mem, Num, R1),
          diff(Num, Acc, NewAcc, NewFlag),
          NewPc is (Pc + 1) mod 100.

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, NewPc, NewMem, In, Out, Flag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 3, Rest), !,
          store(Rest, Acc, Mem, NewMem),
          NewPc is (Pc + 1) mod 100.

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, NewPc, Mem, In, Out, Flag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 5, Rest), !,
          nth0(Rest, Mem, Num, R1),
          load(Num, Acc, Mem, NewAcc),
          NewPc is (Pc + 1) mod 100.

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, NewPc, Mem, In, Out, Flag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 6, Num), !,
          branch(Num, NewPc).

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, NewPc, Mem, In, Out, Flag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 7, Num), !,
          NewPc is (Pc + 1) mod 100,
          branch_zero(Num, Acc, Flag, NewPc).

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, NewPc, Mem, In, Out, Flag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 8, Num), !,
          NewPc is (Pc + 1) mod 100,
          branch_positive(Num, Acc, Flag, NewPc).

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(NewAcc, Pc, Mem, NewIn, Out, Flag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 901), !,
          input(In, NewAcc, NewIn),
          NewPc is (Pc + 1) mod 100.

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      state(Acc, Pc, Mem, In, NewOut)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 902), !,
          output(Acc, Out, NewOut),
          NewPc is (Pc + 1) mod 100.

execution_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                      halted_state(Acc, Pc, Mem, In, Out, Flag)):-
          nth0(Pc, Mem, Elem, R),
          recognize_instruction(Elem, 0).
                      
/*
 * Predicato sum/4 implementa la somma tra un registro e l'accumulatore 
 * e ne salva il risultato modulo 1000 nell'accumulatore.
 * In caso la somma sia maggiore di 1000 viene settato il Flag altrimenti no,
 * per cui si necessita di due regole, per gestire entrambi i casi.
 */
sum(Elem, Acc, NewAcc, flag):-
        Result is (Elem + Acc),
        Result >= 1000, !,
        NewAcc is Result mod 1000.

sum(Elem, Acc, NewAcc, noflag):-
          NewAcc is (Elem + Acc), !.

/*
 * Predicato diff/4 implementa la sottrazione tra un registro e l'accumulatore
 * e salva il risultato modulo 1000 nell'accumulatore.
 * In caso il risultato sia negativo e/o maggiore di 1000 viene settato il flag 
 * altrimenti il flag risulta non settato.
 */
diff(Elem, Acc, NewAcc, flag):-
        Result is (Elem - Acc),
        Result < 0, !,
        NewAcc is Result mod 1000.

diff(Elem, Acc, NewAcc, noflag):-
        NewAcc is (Elem - Acc), !.

/*
 * Predicato store/4(Elem, Acc, Mem, NewMem) per salvare nella cella di memoria di indirizzo Elem,
 * il valore contenuto nell'accumulatore Acc, lasciando inalterato l'accumulatore.
 */
 store(Elem, Acc, Mem, NewMem):-
        replace(Acc, Mem, Elem, NewMem).

/* 
 * Predicato load/4(Elem, Acc,Mem, NewAcc) per prelevare un elemento dalla memoria e lo salva nell'
 * accumulatore, lasciando inalterato il contenuto in memoria.
 */
 load(Elem, Acc, Mem, NewAcc):-
       nth0(Elem, Mem, NewAcc).

/* 
 * Predicato branch/2 per effettuare un salto non condizionale in cui viene
 * impostato il Program Counter al valore passato come argomento chiamato Elem
 */
 branch(Elem, Elem).

/* 
 * Predicato branch_zero/4 per effettuare un salto condizionato in caso in cui
 * il valore nell'accumulatore sia 0 e il flag risulta assente, ponendo il valore del Pc
 * uguale al valore di Elem.
 */
 branch_zero(Elem, 0, flag, Elem).

 branch_zero(Elem, Acc, Flag, NewPc):-
             NewPc is (Pc + 1) mod 100.

/* 
 * Predicato branch_positive/4 per effettuare un salto condizionato in caso il valore
 * nell'accumulatore sia maggiore di 0 e il flag risulta assente, ponendo in tal caso 
 * il valore del PC uguale ad Elem.
 */
branch_positive(Elem, Acc, Flag, Elem):-
             Acc >= 0, !,
             Flag is flag.

branch_positive(Elem, Acc, Flag, NewPc):-
             \+ branch_positive(Elem, Acc, Flag, Elem),
             NewPc is (Pc + 1) mod 100.

/* 
 * Predicato input/3(In, NewAcc, NewIn) per effettuare un operazione di input
 * in cui rimuove un elemento dalla coda di input e lo salva nell'accumulatore.
 * In caso la coda sia vuota fallisce il predicato.
 */
input(In, NewAcc, NewIn):-
      nth0(0, In, NewAcc, NewIn).

/*
 * Predicato output/3(Acc, Out, NewOut) effettua l'operazione di output
 * in cui salva il valore dell'accumulatore in fondo alla coda di output.
 */
output(Acc, Out, [Out | Acc]).
   
/*
 * Predicato recognize_instruction(Elem, Type, Rest) per riconoscere la tipologia
 * di istruzione e separare l'opcode dell'istruzione dal valore della cella di memoria dell'istruzione.
 */
recognize_instruction(Elem, Type, Rest):-
          Type is (Elem div 100), !,
          Num is (Elem mod 100).

recognize_instruction(Elem, Elem):- 
          between(901, 902, Elem).


/*
 * Predicato replace/4(Num, Pos, NewMem) per cambiare il valore dell'elemento 
 * della lista Mem presente nella posizione Pos.
 */
replace(Num, 0, [X | Xs], [Num | Xs]).

replace(Num, Pos, [X | Xs], [X | Ys]):-
        NewPos is Pos - 1,
        replace(Num, NewPos, Xs, Ys).