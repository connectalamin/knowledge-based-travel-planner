:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_files)).

% -------------------------------------------------------
% START SERVER
% -------------------------------------------------------

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% -------------------------------------------------------
% STATIC FILES (WORKING VERSION)
% -------------------------------------------------------

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(files, root(files), []).

% Serve all files inside /www
:- http_handler(files(.), file_handler, [prefix]).

file_handler(Request) :-
    http_reply_from_files('www', [], Request).

% Serve index.html at "/"
:- http_handler(root(.), index_page, []).

index_page(Request) :-
    http_reply_file('www/index.html', [], Request).

% -------------------------------------------------------
% API ROUTE
% -------------------------------------------------------

:- http_handler(root(recommend), recommend_handler, []).

recommend_handler(Request) :-
    % Catch any bad/missing payloads so we always send JSON back
    catch(handle_recommend(Request), Error, (
        print_message(error, Error),
        format(user_error, 'Error in recommend_handler: ~w~n', [Error]),
        % Format error for JSON response
        format(atom(ErrorAtom), '~w', [Error]),
        atom_string(ErrorAtom, ErrorStr),
        reply_json_dict(_{ok:false, error:"invalid_request", details:ErrorStr}, [status(400)])
    )).

% Helper predicates for safe dict access
safe_int(Key, Dict, Default, Value) :-
    (get_dict(Key, Dict, Val) ->
        (integer(Val) -> Value = Val
        ; (number(Val) -> Value is round(Val) ; Value = Default))
    ; Value = Default).

safe_atom(Key, Dict, Default, Value) :-
    (get_dict(Key, Dict, Val) -> 
        (atom(Val) -> Value = Val 
        ; (string(Val) -> atom_string(Value, Val) ; Value = Default))
    ; Value = Default).

% Convert activity (string or atom) to atom
activity_to_atom(Val, Atom) :-
    (atom(Val) -> Atom = Val
    ; (string(Val) -> atom_string(Atom, Val) ; Atom = Val)).

% Compare function for sorting by score (descending - higher scores first)
compare_by_score(Order, Dict1, Dict2) :-
    get_dict(score, Dict1, Score1),
    get_dict(score, Dict2, Score2),
    (Score1 > Score2 -> Order = (<)  % Reverse for descending
    ; Score1 < Score2 -> Order = (>)
    ; Order = (=)).

% Convert a result dict by converting its reasons list to a dict
convert_reasons_to_dict(Result, ResultWithDict) :-
    get_dict(reasons, Result, ReasonsList),
    get_dict(place, Result, Place),
    get_dict(score, Result, Score),
    reasons_list_to_dict(ReasonsList, ReasonsDict),
    ResultWithDict = _{
        place: Place,
        score: Score,
        reasons: ReasonsDict
    }.

% Convert reasons list [Key=Value, ...] to dict
% Build dict by accumulating pairs
reasons_list_to_dict(List, Dict) :-
    reasons_list_to_dict_acc(List, _{}, Dict).

reasons_list_to_dict_acc([], Dict, Dict).
reasons_list_to_dict_acc([Key=Value|Rest], DictIn, DictOut) :-
    put_dict(Key, DictIn, Value, DictNext),
    reasons_list_to_dict_acc(Rest, DictNext, DictOut).

handle_recommend(Request) :-
    % Read JSON from request
    http_read_json_dict(Request, Dict),
    
    % Get all values with safe defaults
    safe_atom(budget, Dict, medium, Budget),
    safe_int(days, Dict, 7, Days),
    safe_atom(climate, Dict, warm, Climate),
    safe_atom(visa, Dict, visa_free, Visa),
    safe_atom(continent, Dict, asia, Continent),
    safe_int(month, Dict, 0, Month),
    safe_int(safety, Dict, 3, Safety),
    safe_atom(food, Dict, any, Food),
    
    % Get activities list - convert strings to atoms if needed
    (get_dict(activities, Dict, Activities0) -> 
        (is_list(Activities0) -> 
            (Activities0 = [] -> Activities = []
            ; maplist(activity_to_atom, Activities0, Activities))
        ; Activities = []) 
    ; Activities = []),

    findall(
        _{
            place: Place,
            score: Score,
            reasons: ReasonsList
        },
        recommend(Budget, Days, Climate, Visa, Continent,
                  Month, Activities, Safety, Food,
                  Place, Score, ReasonsList),
        Results0
    ),

    % Convert reasons lists to dicts for JSON serialization
    maplist(convert_reasons_to_dict, Results0, Results1),
    
    % Verify conversion worked
    (maplist(has_dict_reasons, Results1) -> true 
    ; throw(error('Failed to convert reasons to dicts', _))),

    % Sort by score descending using predsort
    predsort(compare_by_score, Results1, Sorted),

    reply_json_dict(_{ok:true, results:Sorted}).

% Verify that reasons is a dict, not a list
has_dict_reasons(Result) :-
    get_dict(reasons, Result, Reasons),
    is_dict(Reasons).

% -------------------------------------------------------
% DESTINATIONS (20 EXAMPLES – YOU CAN ADD MORE)
% destination(Name, Cost, Days, Climate, Visa, Continent,
%             BestMonths, Activities, Safety, FoodType).
% -------------------------------------------------------

destination(tokyo, cost_high, 5, cool, visa_required, asia,
            [3,4,10,11], [culture,city,food], 5, fine_dining).

destination(bangkok, cost_low, 3, warm, visa_free, asia,
            [1,2,11,12], [street_food,nightlife,temples], 3, street).

destination(osaka, cost_high, 5, cool, visa_required, asia,
            [3,4,10,11], [culture,food,city], 5, fine_dining).

destination(kathmandu, cost_low, 5, cool, visa_on_arrival, asia,
            [2,3,10,11], [hiking,trekking,culture], 3, any).

destination(istanbul, cost_medium, 4, cool, visa_on_arrival, europe,
            [4,5,9,10], [history,food,culture], 4, street).

destination(paris, cost_high, 5, cold, schengen_required, europe,
            [5,6,9], [museum,romance,food], 4, fine_dining).

destination(dubai, cost_high, 4, warm, visa_on_arrival, middle_east,
            [11,12,1,2], [desert,city,shopping], 5, any).

destination(colombo, cost_low, 4, warm, visa_on_arrival, asia,
            [1,2,3,11,12], [beach,wildlife,culture], 3, seafood).

destination(kyoto, cost_high, 4, cool, visa_required, asia,
            [3,4,10,11], [temples,culture,nature], 5, fine_dining).

destination(hanoi, cost_low, 3, warm, visa_free, asia,
            [2,3,10,11], [street_food,culture], 3, street).

% -------------------------------------------------------
% MATCHING LOGIC + SCORE SYSTEM
% -------------------------------------------------------

score(Budget, Cost, S) :-
    (   match_budget(Budget, Cost) -> S = 1 ; S = 0).

match_budget(low, cost_low).
match_budget(medium, cost_low).
match_budget(medium, cost_medium).
match_budget(high, _).

match_climate(Climate, Climate, 1) :- !.
match_climate(_, _, 0).

match_visa(V, V, 1) :- !.
match_visa(_, _, 0).

match_continent(C, C, 1) :- !.
match_continent(_, _, 0).

match_safety(Min, Actual, 1) :- Actual >= Min, !.
match_safety(_, _, 0).

match_month(0, _, 1) :- !.
match_month(Month, List, 1) :- member(Month, List), !.
match_month(_, _, 0).

match_food(any, _, 1) :- !.
match_food(F, F, 1) :- !.
match_food(_, _, 0).

match_activities([], _, 1).
match_activities([A|T], Acts, Score) :-
    (member(A, Acts) -> S1=1 ; S1=0),
    match_activities(T, Acts, S2),
    Score is (S1 + S2) / 2.

recommend(Budget, Days, Climate, Visa, Continent,
          Month, Activities, Safety, Food,
          Place, Score, Reasons) :-

    destination(Place, Cost, ReqDays, Clim, VisaType, Cont,
                Months, Acts, Safe, FoodType),

    Days >= ReqDays,

    score(Budget, Cost, S1),
    match_climate(Climate, Clim, S2),
    match_visa(Visa, VisaType, S3),
    match_continent(Continent, Cont, S4),
    match_month(Month, Months, S5),
    match_safety(Safety, Safe, S6),
    match_food(Food, FoodType, S7),
    match_activities(Activities, Acts, S8),

    Score is S1+S2+S3+S4+S5+S6+S7+S8,

    Reasons = [
        budget_match=S1,
        climate_match=S2,
        visa_match=S3,
        continent_match=S4,
        seasonal_match=S5,
        safety_match=S6,
        food_match=S7,
        activity_match=S8
    ].

