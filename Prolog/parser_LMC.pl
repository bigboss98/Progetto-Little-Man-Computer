%%%% -*- Mode: Prolog -*-

%%%% parse-stuff.pl
%%%%
%%%% Remember that you are always really "following" a state machine.


%%% is_digit/1

is_digit(C) :- char_type(C, digit).


%%% skip_whitespaces/2
%%% skip_whitespaces(Input, MoreInput)

skip_whitespaces([C | Cs], MoreInput) :-
    is_white(C),
    !,

    skip_whitespaces(Cs, MoreInput).
skip_whitespaces([C | Cs], [C | Cs]) :-
    \+ is_white(C),
    !.

skip_whitespaces([], []) :- !.


%%% parse_int/3

parse_int(Input, I, MoreInput) :-
    skip_whitespaces(Input, PossibleIntInput),
    parse_int(PossibleIntInput,
	      [],
	      I,
	      _,
	      MoreInput).

parse_int([], DigitsSoFar, I, DigitCodes, []) :-
    !,
    reverse(DigitsSoFar, DigitCodes),
    number_string(I, DigitCodes).

parse_int([C | Cs], DigitsSoFar, I, DigitCodes, Rest) :-
    is_digit(C),
    !,
    parse_int(Cs, [C | DigitsSoFar], I, DigitCodes, Rest).

parse_int([C | Cs], DigitsSoFar, I, DigitCodes, [C | Cs]) :-
    \+ is_digit(C),
    !,
    reverse(DigitsSoFar, DigitCodes),
    number_string(I, DigitCodes).



parse_string(Input, I, MoreInput) :-
    skip_whitespaces(Input, PossibleAlphaInput),
    parse_string(PossibleAlphaInput,
	      [],
	      I,
	      _,
	      MoreInput).

parse_string([], DigitsSoFar, I, DigitCodes, []) :-
    !,
    reverse(DigitsSoFar, DigitCodes),
    string_codes(I, DigitCodes).

parse_string([C | Cs], DigitsSoFar, I, DigitCodes, Rest) :-
    is_alpha(C),
    !,
    parse_string(Cs, [C | DigitsSoFar], I, DigitCodes, Rest).

parse_string([C | Cs], DigitsSoFar, I, DigitCodes, [C | Cs]) :-
    \+ is_alpha(C),
    !,
    reverse(DigitsSoFar, DigitCodes),
    string_codes(I, DigitCodes).

lmc_load(Filename, Mem) :-
    read_file_to_codes(Filename, Code, []),
    parse_ass(Code, Mem).

parse_ass([], []) :- !.

parse_ass(Code, [Y|Mem]) :-
    parse_op(Code, Y, Rest),
    parse_ass(Rest, Mem).

parse_op(Code, Y, Rest) :-
    parse_string(Code, Y, Rest),
    Y \= "",
    string_upper(Y, YY),
    YY = "ADD",
    parse_int(Rest, Y, Rest),

%%%% end of file -- parse-stuff.pl
