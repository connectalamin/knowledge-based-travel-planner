% -------------------------------------------------------
% TRAVEL DATABASE
% Destination facts for the Knowledge-Based Travel Planner
% -------------------------------------------------------
% destination(Name, Cost, Days, Climate, Visa, Continent,
%             BestMonths, Activities, Safety, FoodType,
%             Lat, Lng, Country).
% -------------------------------------------------------

destination(tokyo, cost_high, 5, cool, visa_required, asia,
            [3,4,10,11], [culture,city,food], 5, fine_dining,
            35.6762, 139.6503, 'Japan').

destination(bangkok, cost_low, 3, warm, visa_free, asia,
            [1,2,11,12], [street_food,nightlife,temples], 3, street,
            13.7563, 100.5018, 'Thailand').

destination(osaka, cost_high, 5, cool, visa_required, asia,
            [3,4,10,11], [culture,food,city], 5, fine_dining,
            34.6937, 135.5023, 'Japan').

destination(kathmandu, cost_low, 5, cool, visa_on_arrival, asia,
            [2,3,10,11], [hiking,trekking,culture], 3, any,
            27.7172, 85.3240, 'Nepal').

destination(istanbul, cost_medium, 4, cool, visa_on_arrival, europe,
            [4,5,9,10], [history,food,culture], 4, street,
            41.0082, 28.9784, 'Turkey').

destination(paris, cost_high, 5, cold, schengen_required, europe,
            [5,6,9], [museum,romance,food], 4, fine_dining,
            48.8566, 2.3522, 'France').

destination(dubai, cost_high, 4, warm, visa_on_arrival, middle_east,
            [11,12,1,2], [desert,city,shopping], 5, any,
            25.2048, 55.2708, 'UAE').

destination(colombo, cost_low, 4, warm, visa_on_arrival, asia,
            [1,2,3,11,12], [beach,wildlife,culture], 3, seafood,
            6.9271, 79.8612, 'Sri Lanka').

destination(kyoto, cost_high, 4, cool, visa_required, asia,
            [3,4,10,11], [temples,culture,nature], 5, fine_dining,
            35.0116, 135.7681, 'Japan').

destination(hanoi, cost_low, 3, warm, visa_free, asia,
            [2,3,10,11], [street_food,culture], 3, street,
            21.0285, 105.8542, 'Vietnam').

destination(london, cost_high, 4, cool, visa_required, europe,
            [5,6,7,8,9], [history,culture,museum,shopping], 5, fine_dining,
            51.5074, -0.1278, 'UK').

destination(new_york, cost_high, 5, cool, visa_required, north_america,
            [4,5,6,9,10], [city,shopping,museum,food], 4, fine_dining,
            40.7128, -74.0060, 'USA').

destination(sydney, cost_high, 5, warm, visa_required, oceania,
            [12,1,2,3], [beach,city,nature], 5, any,
            -33.8688, 151.2093, 'Australia').

destination(rome, cost_medium, 4, warm, schengen_required, europe,
            [4,5,6,9,10], [history,culture,food,romance], 4, fine_dining,
            41.9028, 12.4964, 'Italy').

destination(cairo, cost_low, 4, warm, visa_on_arrival, africa,
            [10,11,12,1,2], [history,desert,culture], 3, street,
            30.0444, 31.2357, 'Egypt').

destination(cape_town, cost_medium, 5, warm, visa_required, africa,
            [11,12,1,2,3], [nature,beach,hiking,wine], 4, fine_dining,
            -33.9249, 18.4241, 'South Africa').

destination(rio, cost_medium, 5, warm, visa_required, south_america,
            [12,1,2,3], [beach,party,nature], 3, street,
            -22.9068, -43.1729, 'Brazil').

destination(bali, cost_low, 5, warm, visa_free, asia,
            [4,5,6,7,8,9], [beach,nature,culture,wellness], 4, any,
            -8.3405, 115.0920, 'Indonesia').

destination(amsterdam, cost_high, 3, cool, schengen_required, europe,
            [4,5,6,7,8,9], [city,museum,culture,nightlife], 5, any,
            52.3676, 4.9041, 'Netherlands').

destination(barcelona, cost_medium, 4, warm, schengen_required, europe,
            [5,6,7,8,9], [architecture,beach,food,nightlife], 4, fine_dining,
            41.3851, 2.1734, 'Spain').
