% -------------------------------------------------------
% TRAVEL DATABASE
% Destination facts for the Knowledge-Based Travel Planner
% -------------------------------------------------------
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

destination(london, cost_high, 4, cool, visa_required, europe,
            [5,6,7,8,9], [history,culture,museum,shopping], 5, fine_dining).

destination(new_york, cost_high, 5, cool, visa_required, north_america,
            [4,5,6,9,10], [city,shopping,museum,food], 4, fine_dining).

destination(sydney, cost_high, 5, warm, visa_required, oceania,
            [12,1,2,3], [beach,city,nature], 5, any).

destination(rome, cost_medium, 4, warm, schengen_required, europe,
            [4,5,6,9,10], [history,culture,food,romance], 4, fine_dining).

destination(cairo, cost_low, 4, warm, visa_on_arrival, africa,
            [10,11,12,1,2], [history,desert,culture], 3, street).

destination(cape_town, cost_medium, 5, warm, visa_required, africa,
            [11,12,1,2,3], [nature,beach,hiking,wine], 4, fine_dining).

destination(rio, cost_medium, 5, warm, visa_required, south_america,
            [12,1,2,3], [beach,party,nature], 3, street).

destination(bali, cost_low, 5, warm, visa_free, asia,
            [4,5,6,7,8,9], [beach,nature,culture,wellness], 4, any).

destination(amsterdam, cost_high, 3, cool, schengen_required, europe,
            [4,5,6,7,8,9], [city,museum,culture,nightlife], 5, any).

destination(barcelona, cost_medium, 4, warm, schengen_required, europe,
            [5,6,7,8,9], [architecture,beach,food,nightlife], 4, fine_dining).
