%%%% -*- Mode: Prolog -*-

%%%% parse-LMC.pl
%%%%
%%%% A simple LMC parser written in prolog.


%%%% Importing the Association Lists Library
%%%% ref: http://www.swi-prolog.org/pldoc/man?section=summary-lib-assoc

:- use_module(library(assoc)).

%%% skip_whitespaces/2
%%% skip_whitespaces(Input, MoreInput)
%%% It skips all whitespace while it not founds another character.

skip_whitespaces([], []) :- !.

skip_whitespaces([C | Cs], MoreInput) :-
    is_white(C), !,
	skip_whitespaces(Cs, MoreInput).
	
skip_whitespaces([C | Cs], [C | Cs]) :-
    \+ is_white(C), !.

%%% parse_int/3
%%% parse_int(Input, I, MoreInput)
%%% It parses int numbers

parse_int(Input, I, MoreInput) :-
    skip_whitespaces(Input, PossibleIntInput),
    parse_int(PossibleIntInput, [], I, _, MoreInput).

parse_int([], DigitsSoFar, I, DigitCodes, []) :-
    !,
    reverse(DigitsSoFar, DigitCodes),
    number_string(I, DigitCodes).

parse_int([C | Cs], DigitsSoFar, I, DigitCodes, Rest) :-
    is_digit(C), !,
    parse_int(Cs, [C | DigitsSoFar], I, DigitCodes, Rest).

parse_int([C | Cs], DigitsSoFar, I, DigitCodes, [C | Cs]) :-
    \+ is_digit(C), !,
    reverse(DigitsSoFar, DigitCodes),
    number_string(I, DigitCodes).

%%% parse_string/3
%%% parse_string(Input, I, MoreInput)
%%% It parses strings.

parse_string(Input, I, MoreInput) :-
    skip_whitespaces(Input, PossibleAlphaInput),
    parse_string(PossibleAlphaInput, [], I, _, MoreInput).

parse_string([], DigitsSoFar, I, DigitCodes, []) :-
    !,
    reverse(DigitsSoFar, DigitCodes),
    string_codes(I, DigitCodes).

parse_string([C | Cs], DigitsSoFar, I, DigitCodes, Rest) :-
    is_alpha(C), !,
    parse_string(Cs, [C | DigitsSoFar], I, DigitCodes, Rest).

parse_string([C | Cs], DigitsSoFar, I, DigitCodes, [C | Cs]) :-
    \+ is_alpha(C), !,
    reverse(DigitsSoFar, DigitCodes),
    string_codes(I, DigitCodes).

%%% parse_string_label/3
%%% parse_string_label(Input, I, MoreInput)
%%% It parses labels, it reads "_", digits and letters.

parse_string_label(Input, I, MoreInput) :-
    skip_whitespaces(Input, PossibleAlphaInput),
    parse_string_label(PossibleAlphaInput, [], I, _, MoreInput).

parse_string_label([], DigitsSoFar, I, DigitCodes, []) :-
    !,
    reverse(DigitsSoFar, DigitCodes),
    string_codes(I, DigitCodes).

parse_string_label([C | Cs], DigitsSoFar, I, DigitCodes, Rest) :-
    is_csym(C), !,
    parse_string_label(Cs, [C | DigitsSoFar], I, DigitCodes, Rest).

parse_string_label([C | Cs], DigitsSoFar, I, DigitCodes, [C | Cs]) :-
    \+ is_csym(C), !,
    reverse(DigitsSoFar, DigitCodes),
    string_codes(I, DigitCodes).

%%% lmc_load/2
%%% lmc_load(Filename, Mem)
%%% The goal reads the content of "Filename" and converts it in ASCII code
%%% Then it starts the assembly parsing using parse_ass/2 predicate.

lmc_load(Filename, Mem) :-
    read_file_to_codes(Filename, Code, []),
    parse_label_load(Code, Labels),
    parse_ass(Code, Mem, Labels),
    print(Mem).

%%% parse_ass/3
%%% parse_ass(Code, Mem, Labels)
%%% The goal cares of building the Memory list.

parse_ass([], [], _) :- !.

%%% That is the case a line starts with "//"

parse_ass([X, X | Code], Mem, Labels) :-
    X = 47, !,
    parse_end(Code, Codes),
    parse_ass(Codes, Mem, Labels).

%%% That is the case a line starts with "\n"

parse_ass([X | Code], Mem, Labels) :-
    X = 10, !,
    parse_end(Code, Codes),
    parse_ass(Codes, Mem, Labels).

%%% That is the case a line starts with "tab"

parse_ass([X | Code], Mem, Labels) :-
    X = 9, !,
    parse_end(Code, Codes),
    parse_ass(Codes, Mem, Labels).

parse_ass(Code, [Y | Mem], Labels) :-
    parse_op(Code, Y, Others, Labels),!,
    parse_end(Others, Rest),
    skip_whitespaces(Rest, NewRest),
    parse_ass(NewRest, Mem, Labels).

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
%%% and tab (9 in ASCII) however it finds "\n" (10 in ASCII) it breaks.

parse_end([],[]) :- !.

parse_end([First, Second | Body], Rest) :-
    First = 47, Second = 47, !,
    parse_end_line_comment(Body, Rest).

parse_end([X | Body], Body) :-
    X = 10, !.

parse_end([X | Body], Rest) :-
    X = 32, !,
    parse_end(Body, Rest).

parse_end([X | Body], Rest) :-
    X = 9, !,
    parse_end(Body, Rest).

%%% skip_end/2
%%% skip_end(Code, Rest)
%%% Used to skip every characters, it ends when there is not
%%% more code or it finds a "\n" (10 in ASCII)

skip_end([],[]) :- !.

skip_end([First, Second | Body], Rest) :-
    First = 47, Second = 47, !,
    parse_end_line_comment(Body, Rest).

skip_end([X | Body], Body) :-
    X = 10, !.

skip_end([X | Body], Rest) :-
    X \= 10, !,
    skip_end(Body, Rest).
	
%%% parse_op/4
%%% parse_op(Code, AssCode, Rest, Labels)
%%% The goal reads the list of ascii codes, and however it finds a
%%% correct instruction it returns the equivalent assembly code.


parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "ADD",
    parse_int(Other, MemValue, Rest),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 100.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "ADD",
    parse_string_label(Other, Label, Rest),
    get_assoc(Label, Labels, MemValue),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 100.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "SUB",
    parse_int(Other, MemValue, Rest), !,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 200.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "SUB",
    parse_string_label(Other, Label, Rest),
    get_assoc(Label, Labels, MemValue),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 200.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "STA",
    parse_int(Other, MemValue, Rest), !,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 300.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "STA", StringUp \= "",
    parse_string_label(Other, Label, Rest),
    get_assoc(Label, Labels, MemValue),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 300.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "LDA",
    parse_int(Other, MemValue, Rest), !,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 500.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "LDA",
    parse_string_label(Other, Label, Rest),
    get_assoc(Label, Labels, MemValue),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 500.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRA",
    parse_int(Other, MemValue, Rest), !,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 600.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRA",
    parse_string_label(Other, Label, Rest),
    get_assoc(Label, Labels, MemValue),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 600.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRZ",
    parse_int(Other, MemValue, Rest), !,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 700.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRZ",
    parse_string_label(Other, Label, Rest),
    get_assoc(Label, Labels, MemValue),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 700.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRP",
    parse_int(Other, MemValue, Rest), !,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 800.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "BRP",
    parse_string_label(Other, Label, Rest),
    get_assoc(Label, Labels, MemValue),!,
    MemValue < 100, !, MemValue > -1, !,
    AssCode is MemValue + 800.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "DAT",
    parse_int(Other, MemValue, Rest), !,
    MemValue < 1000, !, MemValue > -1, !,
    AssCode is MemValue.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Other),
    string_upper(String, StringUp),
    StringUp = "DAT",
    parse_string(Other, NewString, Rest),
    NewString = "", !,
    AssCode is 0.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Rest),
    string_upper(String, StringUp),
    StringUp = "INP", !, StringUp \= "",
    AssCode is 901.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Rest),
    string_upper(String, StringUp),
    StringUp = "OUT", !, StringUp \= "",
    AssCode is 902.

parse_op(Code, AssCode, Rest, _) :-
    parse_string(Code, String, Rest),
    string_upper(String, StringUp),
    StringUp = "HLT", !,
    AssCode is 0.

parse_op(Code, AssCode, Rest, Labels) :-
    parse_string_label(Code, Label, Ys),
    Label \= "",
    get_assoc(Label, Labels, _), !,
    parse_op(Ys, AssCode, Rest, Labels).

	
%%% parse_label_load/2
%%% parse_label_load(Code, LabelList)
%%% The goal reads the labels from the ASCII code
%%% and then saves them in LabelList

parse_label_load(Code, LabelList):-
    empty_assoc(Empty),
    Operations = ["ADD", "SUB", "STA", "LDA", "BRA", "BRZ", "BRP", "INP", 
	"OUT", "HLT", "DAT"],
    parse_label(Code, Empty, LabelList, 0, Operations).

%%% parse_label/4
%%% parse_label(Code, LabelList, NewLabelList, N, Operations)
%%% It parses the labels, it updates the LabeList
%%% until there is not more code to be checked

parse_label([], X, X, _, _) :- !.

parse_label(Code, LabelList, NewLabelList, N, Operations) :-
    parse_string_label(Code, Label, Rest),
    string_upper(Label, LabelUp),
    Label \= "",
    not(memberchk(LabelUp, Operations)), !,
    parse_string(Rest, String, _),
    string_upper(String, StringUp),
    memberchk(StringUp, Operations),
    N1 is N + 1,
    skip_end(Code, Parsed),
    put_assoc(Label, LabelList, N, Updated),
    parse_label(Parsed, Updated, NewLabelList, N1, Operations).

parse_label(Code, LabelList, Updated, N, Operations) :-
    parse_string_label(Code, Label, _),
    string_upper(Label, LabelUp),
    Label \= "", !,
    memberchk(LabelUp, Operations), !,
    skip_end(Code, Parsed),
    N1 is N + 1,
    parse_label(Parsed, LabelList, Updated, N1, Operations).

parse_label([X, X | Code], LabelList, Updated, N, Operations) :-
    X = 47, !,
    skip_end(Code, Parsed),
    N1 is N,
    parse_label(Parsed, LabelList, Updated, N1, Operations).

parse_label([X | Code], LabelList, Updated, N, Operations) :-
    X = 10, !,
    skip_end(Code, Parsed),
    N1 is N,
    parse_label(Parsed, LabelList, Updated, N1, Operations).

parse_label([X | Code], LabelList, Updated, N, Operations) :-
    X = 32, !,
    skip_end(Code, Parsed),
    N1 is N,
    parse_label(Parsed, LabelList, Updated, N1, Operations).

parse_label([X | Code], LabelList, Updated, N, Operations) :-
    X = 9, !,
    skip_end(Code, Parsed),
    N1 is N,
    parse_label(Parsed, LabelList, Updated, N1, Operations).


%%%% end of file -- parse-LMC.pl
