/* travel_server.pl
   SWI-Prolog HTTP server for Knowledge-Based Travel Planner
   - Knowledge base: 30+ destinations
   - Filters: budget, days, climate, visa_status, activities, continent, month, safety_min, food_pref
   - Scoring & explainability
   Run:
     swipl
     ?- consult('travel_server.pl').
     ?- server(8000).
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).   % serve static files
:- use_module(library(http/http_json)).           % JSON
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_error)).          % friendly errors
:- use_module(library(apply)).                    % maplist, include

% ---------- Start server ----------
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% ---------- Static files handler ----------
% Serve ./www directory at root
:- http_handler(root(.), http_reply_from_files('www', []), [prefix]).

% ---------- API handler ----------
:- http_handler(root(recommend), recommend_handler, []).

% Accepts JSON POST with fields:
% budget (low/medium/high)
% days (int)
% climate (warm/cool/cold)
% visa_status (visa_free / visa_on_arrival / has_visa / has_schengen / none)
% activities (list of atoms, optional)
% continent (atom or "any")
% month (1-12 or 0 for any)
% safety_min (1-5)
% food_pref (atom like vegetarian / street / any)
recommend_handler(Request) :-
    http_read_json_dict(Request, DictIn),

    Budget = DictIn.get(budget, medium),
    Days   = DictIn.get(days, 4),
    Climate= DictIn.get(climate, cool),
    Visa   = DictIn.get(visa_status, visa_on_arrival),
    Activities = DictIn.get(activities, []),
    Continent  = DictIn.get(continent, any),
    Month      = DictIn.get(month, 0),
    SafetyMin  = DictIn.get(safety_min, 1),
    FoodPref   = DictIn.get(food_pref, any),

    findall( RecDict,
        ( candidate(Place),
          score_destination(Budget,Days,Climate,Visa,Activities,Continent,Month,SafetyMin,FoodPref,Place,Score,Reasons),
          Score > 0, % filter out zero-score items
          RecDict = _{ place: Place, score: Score, reasons: Reasons }
        ),
        RawResults),

    ( RawResults = [] ->
        Reply = _{ ok:false, message: "No destinations matched. Try relaxing filters.", recommendations: []}
    ;
        sort_by_score_desc(RawResults, Sorted),
        Reply = _{ ok:true, recommendations: Sorted }
    ),
    reply_json_dict(Reply).

% ---------- Helper: candidate places (all destinations) ----------
candidate(Place) :- destination(Place,_,_,_,_,_,_,_,_,_).

% ---------- Sorting helper ----------
% RawResults is list of dicts with key 'score'. Sort descending.
sort_by_score_desc(Raw, SortedDesc) :-
    map_list_to_pairs(get_score, Raw, Pairs),
    keysort(Pairs, PairsSortedAsc),
    reverse(PairsSortedAsc, PairsSortedDesc),
    pairs_values(PairsSortedDesc, SortedDesc).

get_score(Dict, Score) :- Score = Dict.score.

% ---------- Knowledge base: destination(Name, CostCategory, ReqDays, Climate, VisaType, Activities, Continent, BestMonths, SafetyLevel(1-5), FoodTypes)
% CostCategory: cost_low | cost_medium | cost_high
% Climate: warm | cool | cold
% VisaType: visa_free | visa_on_arrival | visa_required | schengen_required
% Activities: list of atoms e.g. [beach, hiking]
% Continent: asia | europe | africa | north_america | south_america | oceania | any
% BestMonths: list of month numbers (1..12) where destination is ideal
% SafetyLevel: 1 (low) .. 5 (very safe)
% FoodTypes: list like [street, vegetarian, seafood, halal]

destination(bali, cost_low, 4, warm, visa_on_arrival, [beach,culture,surfing], asia, [5,6,7,8,9], 3, [seafood,vegetarian,street]).
destination(thailand, cost_low, 3, warm, visa_free, [beach,food,nightlife], asia, [11,12,1,2,3], 3, [street,seafood,vegetarian]).
destination(nepal, cost_low, 6, cool, visa_on_arrival, [hiking,trekking,culture], asia, [10,11,12], 2, [vegetarian,local]).
destination(sri_lanka, cost_low, 4, warm, visa_on_arrival, [beach,culture,wildlife], asia, [12,1,2,3], 3, [seafood,vegetarian]).
destination(tokyo, cost_high, 5, cool, visa_required, [culture,food,city], asia, [3,4,10,11], 5, [seafood,vegetarian]).
destination(seoul, cost_medium, 4, cool, visa_required, [culture,food,shopping], asia, [4,5,9,10], 4, [street,vegetarian]).
destination(kathmandu, cost_low, 5, cool, visa_on_arrival, [culture,hiking], asia, [10,11], 2, [vegetarian,local]).
destination(istanbul, cost_medium, 4, cool, visa_on_arrival, [culture,history,food], europe, [4,5,9,10], 3, [street,seafood,vegetarian]).
destination(paris, cost_high, 5, cold, schengen_required, [culture,museum,food], europe, [4,5,6,9], 5, [vegetarian,seafood]).
destination(london, cost_high, 5, cool, visa_required, [culture,theater,city], europe, [5,6,7,8], 5, [vegetarian,street]).
destination(rome, cost_medium, 4, warm, schengen_required, [culture,history,food], europe, [4,5,6,9], 4, [seafood,vegetarian]).
destination(barcelona, cost_medium, 4, warm, schengen_required, [beach,culture,food], europe, [5,6,7,8,9], 4, [seafood,vegetarian,street]).
destination(berlin, cost_medium, 4, cool, schengen_required, [culture,nightlife,history], europe, [5,6,7,8,9], 5, [vegetarian,street]).
destination(dubai, cost_high, 4, warm, visa_on_arrival, [city,shopping,desert], asia, [11,12,1,2,3], 4, [halal,seafood,vegetarian]).
destination(cape_town, cost_medium, 5, warm, visa_required, [beach,hiking,wildlife], africa, [11,12,1,2], 3, [seafood,local]).
destination(cairo, cost_low, 4, warm, visa_on_arrival, [history,desert,culture], africa, [10,11,12,1,2,3], 2, [local,street]).
destination(cancun, cost_medium, 4, warm, visa_free, [beach,party,relax], north_america, [11,12,1,2,3,4], 3, [seafood,street]).
destination(new_york, cost_high, 5, cool, visa_required, [city,food,shopping], north_america, [5,6,9,10], 4, [street,vegetarian]).
destination(toronto, cost_medium, 5, cold, visa_required, [city,nature,culture], north_america, [6,7,8,9], 5, [vegetarian,street]).
destination(santiago, cost_medium, 5, warm, visa_required, [city,food,nature], south_america, [10,11,12,1,2], 4, [seafood,vegetarian]).
destination(lima, cost_low, 4, warm, visa_free, [food,culture,history], south_america, [4,5,9,10], 3, [seafood,local]).
destination(auckland, cost_medium, 6, mild, visa_required, [nature,beach,city], oceania, [12,1,2,3], 4, [seafood,vegetarian]).
destination(sydney, cost_high, 5, warm, visa_required, [beach,city,events], oceania, [12,1,2,3], 4, [seafood,vegetarian]).
destination(maldives, cost_high, 5, warm, visa_on_arrival, [beach,relax,diving], asia, [11,12,1,2,3], 4, [seafood]).
destination(vancouver, cost_high, 5, cool, visa_required, [nature,city,food], north_america, [6,7,8,9], 5, [vegetarian,seafood]).
destination(riodejaneiro, cost_medium, 4, warm, visa_free, [beach,party,city], south_america, [12,1,2,3], 3, [street,seafood]).
destination(amsterdam, cost_medium, 4, cool, schengen_required, [culture,canals,nightlife], europe, [5,6,7,8,9], 5, [vegetarian,street]).
destination(prague, cost_low, 4, cool, schengen_required, [culture,history,beer], europe, [5,6,9], 4, [local,vegetarian]).
destination(marrakesh, cost_low, 4, warm, visa_on_arrival, [culture,market,history], africa, [10,11,3,4], 2, [street,local]).
destination(chiang_mai, cost_low, 4, warm, visa_free, [culture,food,nature], asia, [11,12,1,2], 4, [vegetarian,street]).

% ---------- Budget matching ----------
match_budget(low, cost_low).
match_budget(medium, cost_low).
match_budget(medium, cost_medium).
match_budget(high, _).   % high budget accepts any cost

% ---------- Visa compatibility ----------
visa_ok(UserStatus, DestVisa) :-
    ( DestVisa = visa_free -> true ;
      DestVisa = visa_on_arrival,
        member(UserStatus, [visa_on_arrival, has_visa, has_schengen, visa_free]) ;
      DestVisa = visa_required,
        member(UserStatus, [has_visa, has_schengen]) ;
      DestVisa = schengen_required,
        UserStatus = has_schengen
    ).

% ---------- Activities matching ----------
activities_ok([], _DestActivities) :- !.
activities_ok(Requested, DestActivities) :-
    is_list(Requested),
    forall(member(R, Requested), member(R, DestActivities)).

% ---------- Continent matching ----------
continent_ok(any, _).
continent_ok(Cont, DestCont) :- Cont = DestCont.

% ---------- Month (season) matching ----------
month_bonus(0, _DestMonths, 0).   % user chose 0 -> no bonus
month_bonus(Month, DestMonths, 2) :- Month \= 0, member(Month, DestMonths), !.
month_bonus(_, _, 0).

% ---------- Safety check ----------
safety_ok(Min, Level) :- Level >= Min.

% ---------- Food preference matching ----------
food_bonus(any, _, 0).
food_bonus(FoodPref, FoodTypes, 1) :-
    FoodPref \= any,
    member(FoodPref, FoodTypes), !.
food_bonus(_, _, 0).

% ---------- Activities overlap count bonus ----------
activities_overlap_bonus([], _DestActs, 0).
activities_overlap_bonus(Requested, DestActs, Count) :-
    include({DestActs}/[A]>>member(A, DestActs), Requested, Matched),
    length(Matched, Count).

% ---------- Scoring function
% We compute a total score as sum of weighted components:
% - budget_match: 1
% - days_ok: 1  (1 if Days >= ReqDays)
% - climate_match: 1 (exact equality)
% - visa_ok: 1
% - activities overlap: up to 3 (1 per matched activity, cap 3)
% - month_bonus: 0 or 2 (if ideal month)
% - food_bonus: 0 or 1
% - safety: add (SafetyLevel - SafetyMin) if >=0 (small bonus)
score_destination(Budget,Days,Climate,Visa,Activities,ContFilter,Month,SafetyMin,FoodPref,Place,Score,Reasons) :-
    destination(Place, Cost, ReqDays, DestClimate, DestVisa, DestActs, DestCont, DestMonths, SafetyLevel, FoodTypes),

    % 1) basic hard filters: budget, continent, visa, safety, days
    match_budget(Budget, Cost),
    continent_ok(ContFilter, DestCont),
    visa_ok(Visa, DestVisa),
    safety_ok(SafetyMin, SafetyLevel),
    Days >= ReqDays,

    % 2) compute components
    (Cost = cost_low -> BudgetMatch = 1 ; (Cost = cost_medium, Budget \= low -> BudgetMatch = 1 ; (Cost = cost_medium, Budget = low -> BudgetMatch = 0 ; BudgetMatch = 1))),
    (Days >= ReqDays -> DaysMatch = 1 ; DaysMatch = 0),
    (Climate = DestClimate -> ClimateMatch = 1 ; ClimateMatch = 0),
    (visa_ok(Visa, DestVisa) -> VisaMatch = 1 ; VisaMatch = 0),

    activities_overlap_bonus(Activities, DestActs, ActCount),
    ActBonus is min(3, ActCount),

    month_bonus(Month, DestMonths, MonthBonus),
    food_bonus(FoodPref, FoodTypes, FoodBonus),

    SafetyBonus is max(0, SafetyLevel - SafetyMin),  % small positive if safer than requested

    % Weights: budget(1), days(1), climate(1), visa(1), activities(ActBonus), month(2), food(1), safety(SafetyBonus)
    Score is BudgetMatch + DaysMatch + ClimateMatch + VisaMatch + ActBonus + MonthBonus + FoodBonus + SafetyBonus,

    % Explainable reason strings
    atomic_list_concat(['budget_match:',BudgetMatch], RM1),
    format(atom(RM2),'days_match:~w(need~w)', [DaysMatch, ReqDays]),
    atomic_list_concat(['climate_match:',ClimateMatch], RM3),
    format(atom(RM4),'visa_type:~w',[DestVisa]),
    format(atom(RA),'activities_matched:~w', [ActCount]),
    format(atom(RM),'month_bonus:~w', [MonthBonus]),
    format(atom(RF),'food_bonus:~w', [FoodBonus]),
    format(atom(RS),'safety_level:~w', [SafetyLevel]),

    Reasons = [RM1, RM2, RM3, RM4, RA, RM, RF, RS].

% End of file
