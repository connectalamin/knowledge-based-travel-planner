% -------------------------------------------------------
% RECOMMENDATION RULES
% Matching logic and scoring system for travel recommendations
% -------------------------------------------------------

:- consult('travel_db.pl').

% -------------------------------------------------------
% SCORING LOGIC
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
match_month(Month, List, 0.5) :-
    (Month =:= 1 -> Prev = 12 ; Prev is Month - 1),
    (Month =:= 12 -> Next = 1 ; Next is Month + 1),
    (member(Prev, List) ; member(Next, List)), !.
match_month(_, _, 0).

match_food(any, _, 1) :- !.
match_food(F, F, 1) :- !.
match_food(_, _, 0).

match_activities([], _, 1) :- !.
match_activities(Requested, Available, Score) :-
    Requested \= [],
    intersection(Requested, Available, Matched),
    length(Requested, Total),
    length(Matched, Count),
    Score is Count / Total.

% -------------------------------------------------------
% MAIN RECOMMENDATION PREDICATE (Extended with geo data)
% -------------------------------------------------------

recommend(Budget, Days, Climate, Visa, Continent,
          Month, Activities, Safety, Food,
          Place, Score, Reasons, Lat, Lng, Country, BestMonths, AllActivities, VisaType, FoodType, Cost, SafetyRating) :-

    destination(Place, Cost, ReqDays, Clim, VisaType, Cont,
                BestMonths, AllActivities, SafetyRating, FoodType,
                Lat, Lng, Country),

    Days >= ReqDays,

    score(Budget, Cost, S1),
    match_climate(Climate, Clim, S2),
    match_visa(Visa, VisaType, S3),
    match_continent(Continent, Cont, S4),
    match_month(Month, BestMonths, S5),
    match_safety(Safety, SafetyRating, S6),
    match_food(Food, FoodType, S7),
    match_activities(Activities, AllActivities, S8),

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
