% -------------------------------------------------------
% KNOWLEDGE-BASED TRAVEL PLANNER - HTTP SERVER
% -------------------------------------------------------

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_files)).
:- use_module(library(lists)).

% Load recommendation rules (which loads travel_db.pl)
:- consult('rules.pl').

% Set document root to current directory
:- prolog_load_context(directory, Dir), asserta(user:file_search_path(document_root, Dir)).

% -------------------------------------------------------
% SERVER START WITH BANNER
% -------------------------------------------------------

server :- server(8000).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('~n==========================================~n'),
    format('✅ Server started successfully!~n'),
    format('==========================================~n'),
    format('Server running on: http://localhost:~w~n', [Port]),
    format('~n'),
    format('📍 API Endpoints:~n'),
    format('   POST /recommend - Get travel recommendations~n'),
    format('   GET  /activities - List all activities~n'),
    format('~n'),
    format('🎯 Frontend UI: http://localhost:~w/app~n', [Port]),
    format('==========================================~n~n').

% -------------------------------------------------------
% CORS HEADERS
% -------------------------------------------------------

cors_headers :-
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n').

% -------------------------------------------------------
% STATIC FILE ROUTES
% -------------------------------------------------------

% Frontend at /app
:- http_handler(root(app), serve_frontend, []).

% API info at root /
:- http_handler(root(.), api_info_page, []).

% Static files
:- http_handler(root('style.css'), http_reply_file(document_root('style.css'), []), []).
:- http_handler(root('script.js'), http_reply_file(document_root('script.js'), []), []).

serve_frontend(Request) :-
    http_reply_file(document_root('index.html'), [], Request).

api_info_page(_Request) :-
    format('Content-type: text/html; charset=utf-8~n~n'),
    format('<!DOCTYPE html>~n'),
    format('<html><head><meta charset="UTF-8"><title>Travel Planner API</title>~n'),
    format('<style>body{font-family:system-ui,sans-serif;max-width:600px;margin:50px auto;padding:20px;background:#f5f5f5;}~n'),
    format('.card{background:white;padding:30px;border-radius:12px;box-shadow:0 2px 10px rgba(0,0,0,0.1);}~n'),
    format('h1{color:#0f4c5c;margin-bottom:10px;}a{color:#e36414;font-weight:600;}</style></head>~n'),
    format('<body><div class="card">~n'),
    format('<h1>Travel Planner API</h1>~n'),
    format('<p>Knowledge-based travel recommendation system.</p>~n'),
    format('<h3>Endpoints:</h3>~n'),
    format('<ul><li><code>POST /recommend</code> - Get recommendations</li>~n'),
    format('<li><code>GET /activities</code> - List activities</li></ul>~n'),
    format('<p><a href="/app">Open the Travel Planner App</a></p>~n'),
    format('</div></body></html>~n').

% -------------------------------------------------------
% API ROUTES
% -------------------------------------------------------

:- http_handler(root(recommend), recommend_handler, []).
:- http_handler(root(activities), activities_handler, []).

activities_handler(_Request) :-
    cors_headers,
    findall(List, destination(_,_,_,_,_,_,_,List,_,_), Lists),
    flatten(Lists, Flat),
    sort(Flat, Unique),
    reply_json_dict(_{ok:true, activities:Unique}).

recommend_handler(Request) :-
    cors_headers,
    catch(handle_recommend(Request), Error, (
        print_message(error, Error),
        format(user_error, 'Error in recommend_handler: ~w~n', [Error]),
        format(atom(ErrorAtom), '~w', [Error]),
        atom_string(ErrorAtom, ErrorStr),
        reply_json_dict(_{ok:false, error:"invalid_request", details:ErrorStr}, [status(400)])
    )).

% -------------------------------------------------------
% HELPER PREDICATES
% -------------------------------------------------------

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

activity_to_atom(Val, Atom) :-
    (atom(Val) -> Atom = Val
    ; (string(Val) -> atom_string(Atom, Val) ; Atom = Val)).

compare_by_score(Order, Dict1, Dict2) :-
    get_dict(score, Dict1, Score1),
    get_dict(score, Dict2, Score2),
    (Score1 > Score2 -> Order = (<)
    ; Score1 < Score2 -> Order = (>)
    ; Order = (=)).

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

reasons_list_to_dict(List, Dict) :-
    reasons_list_to_dict_acc(List, _{}, Dict).

reasons_list_to_dict_acc([], Dict, Dict).
reasons_list_to_dict_acc([Key=Value|Rest], DictIn, DictOut) :-
    put_dict(Key, DictIn, Value, DictNext),
    reasons_list_to_dict_acc(Rest, DictNext, DictOut).

has_dict_reasons(Result) :-
    get_dict(reasons, Result, Reasons),
    is_dict(Reasons).

% -------------------------------------------------------
% MAIN RECOMMENDATION HANDLER
% -------------------------------------------------------

handle_recommend(Request) :-
    http_read_json_dict(Request, Dict),
    
    safe_atom(budget, Dict, medium, Budget),
    safe_int(days, Dict, 7, Days),
    safe_atom(climate, Dict, warm, Climate),
    safe_atom(visa, Dict, visa_free, Visa),
    safe_atom(continent, Dict, asia, Continent),
    safe_int(month, Dict, 0, Month),
    safe_int(safety, Dict, 3, Safety),
    safe_atom(food, Dict, any, Food),
    
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

    maplist(convert_reasons_to_dict, Results0, Results1),
    
    (maplist(has_dict_reasons, Results1) -> true 
    ; throw(error('Failed to convert reasons to dicts', _))),

    predsort(compare_by_score, Results1, Sorted),

    reply_json_dict(_{ok:true, results:Sorted}).
