%%%% -*- Mode: Prolog -*-

%%%% parse-LMC.pl
%%%%
%%%% A simple LMC parser written in prolog.


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

%%% lmc_load/2
%%% lmc_load(Filename, Mem)
%%% The goal reads the content of "Filename" and converts it in ASCII code
%%% Then it starts the assembly parsing using parse_ass/2 predicate.

lmc_load(Filename, Mem) :-
    read_file_to_codes(Filename, Code, []),
    parse_ass(Code, Mem).


%%% parse_ass/2
%%% parse_ass(Code, Mem)
%%% The goal cares of building the Memory list.

parse_ass([], []) :- !.

%%% That is the case a line starts with "\\"

parse_ass([X, X | Code], Mem) :-
    X = 92,!,
    parse_end([X, X | Code], Codes),
    parse_ass(Codes, Mem).

parse_ass(Code, [Y | Mem]) :-
    parse_op(Code, Y, Others),
    parse_end(Others, Rest),
    parse_ass(Rest, Mem).

%%% parse_end_line_comment/2
%%% parse_end_line_comment(Code, Codes)
%%% It deletes the comment from a line, till it finds "\n" (10 in ASCII)

parse_end_line_comment([], []):- !.

parse_end_line_comment([X], []):- X = 10, !.

parse_end_line_comment([X | Z], Z):-
    X = 10, !.

parse_end_line_comment([X | Z], Zs) :-
    X \= 10,
    parse_end_line_comment(Z, Zs), !.


%%% parse_end/2
%%% parse_end(Code, Codes)
%%% It checks the end of a line, it deletes whitespace (32 in ASCII)
%%% however it finds "\n" (10 in ASCII) it breaks.

parse_end([],[]) :- !.

parse_end([First, Second | Body], Rest) :-
    First = 92, Second = 92, !,
    parse_end_line_comment(Body, Rest).

parse_end([X | Body], Body) :-
    X = 10, !.

parse_end([X | Body], Rest) :-
    X = 32,
    parse_end(Body, Rest).

%%% parse_op/3
%%% parse_op(Code, AssCode, Rest)
%%% The goal reads the list of ascii codes, and however it finds a
%%% correct instruction it returns the equivalent assembly code.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "ADD", !,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 100.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "SUB", !,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 200.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "STA",!,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 300.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "LDA", !,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 500.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRA", !,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 600.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRZ", !,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 700.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRP", !,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 800.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRP", !,
    parse_int(Other, MemValue, Rest),
    MemValue < 100, !, MemValue > 0, !,
    AssCode is MemValue + 800.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Rest),
    string_upper(String, StringUp),
    StringUp = "INP", !,
    AssCode is 901.

parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Rest),
    string_upper(String, StringUp),
    StringUp = "OUT", !,
    AssCode is 902.


parse_op(Code, AssCode, Rest) :-
    parse_string(Code, String, Rest),
    string_upper(String, StringUp),
    StringUp = "HLT", !,
    AssCode is 0.

parser

%%%% end of file -- parse-LMC.pl
