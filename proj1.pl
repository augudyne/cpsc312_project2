:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).

:- rdf_load('nba-2018.ttl', [format(turtle)]).
teamName('http://stellman-greene.com/pbprdf/teams/Magic', "Magic").
teamName('http://stellman-greene.com/pbprdf/teams/Bulls', "Bulls").
teamName('http://stellman-greene.com/pbprdf/teams/Spurs', "Spurs").
teamName('http://stellman-greene.com/pbprdf/teams/Grizzlies', "Grizzlies").
teamName('http://stellman-greene.com/pbprdf/teams/76ers', "76ers").
teamName('http://stellman-greene.com/pbprdf/teams/Pistons', "Pistons").
teamName('http://stellman-greene.com/pbprdf/teams/Pacers', "Pacers").
teamName('http://stellman-greene.com/pbprdf/teams/Thunder', "Thunder").
teamName('http://stellman-greene.com/pbprdf/teams/Suns', "Suns").
teamName('http://stellman-greene.com/pbprdf/teams/Warriors', "Warriors").
teamName('http://stellman-greene.com/pbprdf/teams/Trail_Blazers', "Trail_Blazers").
teamName('http://stellman-greene.com/pbprdf/teams/Hawks', "Hawks").
teamName('http://stellman-greene.com/pbprdf/teams/Pelicans', "Pelicans").
teamName('http://stellman-greene.com/pbprdf/teams/Rockets', "Rockets").
teamName('http://stellman-greene.com/pbprdf/teams/Jazz', "Jazz").
teamName('http://stellman-greene.com/pbprdf/teams/Nuggets', "Nuggets").
teamName('http://stellman-greene.com/pbprdf/teams/Celtics', "Celtics").
teamName('http://stellman-greene.com/pbprdf/teams/Lakers', "Lakers").
teamName('http://stellman-greene.com/pbprdf/teams/Nets', "Nets").
teamName('http://stellman-greene.com/pbprdf/teams/Wizards', "Wizards").
teamName('http://stellman-greene.com/pbprdf/teams/Clippers', "Clippers").
teamName('http://stellman-greene.com/pbprdf/teams/Mavericks', "Mavericks").
teamName('http://stellman-greene.com/pbprdf/teams/Cavaliers', "Cavaliers").
teamName('http://stellman-greene.com/pbprdf/teams/Timberwolves', "Timberwolves").
teamName('http://stellman-greene.com/pbprdf/teams/Knicks', "Knicks").
teamName('http://stellman-greene.com/pbprdf/teams/Raptors', "Raptors").
teamName('http://stellman-greene.com/pbprdf/teams/Heat', "Heat").
teamName('http://stellman-greene.com/pbprdf/teams/Kings', "Kings").
teamName('http://stellman-greene.com/pbprdf/teams/Hornets', "Hornets").
teamName('http://stellman-greene.com/pbprdf/teams/Bucks', "Bucks").

%magic numbers
% Weighing players
% clutchness
mgn(clutchWeight, 0.2).
mgn(clutchAttemptsScale, 0.6).


% base is <http://stellman-greene.com/>
% any with pbprdf http://stellman-greene.com/pbprdf#hasPlayer
% any with rdfs: has <http://www.w3.org/2000/01/rdf-schema#> preceeding
played_for_team(P, TN) :-
    rdf(Roster, 'http://stellman-greene.com/pbprdf#rosterTeam', T), 
    teamName(T, TN), !,
    rdf(Roster, 'http://stellman-greene.com/pbprdf#hasPlayer', P).

players_on_team(TN, Players) :-  aggregate_all(set(X), played_for_team(X, TN), Players).

shot_made_with_min_left(P, M, E) :-
    rdf(E, 'http://stellman-greene.com/pbprdf#period', literal(type('http://www.w3.org/2001/XMLSchema#int', '4'))),
    rdf(E, 'http://stellman-greene.com/pbprdf#secondsLeftInPeriod', literal(type('http://www.w3.org/2001/XMLSchema#int', T))),
    atom_number(T, X),
    X =< (M * 60),
    rdf(E, 'http://stellman-greene.com/pbprdf#shotMade',
        literal(type('http://www.w3.org/2001/XMLSchema#boolean', true))),
    rdf(E, 'http://stellman-greene.com/pbprdf#shotBy', P).

shot_attempt_with_min_left(P, M, E) :-
    rdf(E, 'http://stellman-greene.com/pbprdf#period', literal(type('http://www.w3.org/2001/XMLSchema#int', '4'))),
    rdf(E, 'http://stellman-greene.com/pbprdf#secondsLeftInPeriod', literal(type('http://www.w3.org/2001/XMLSchema#int', T))),
    atom_number(T, X),
    X =< (M * 60),
    rdf(E, 'http://stellman-greene.com/pbprdf#shotBy', P).

team_clutchest(T, Players) :- clutch_players_on_team(T, L), insert_sort(L, Sorted), first_n(Sorted, 5, Players).

clutch_shots_by_player(P, N) :- aggregate_all(count, shot_made_with_min_left(P, 1, _), N).

clutch_attempts_by_player(P, N) :- aggregate_all(count, shot_attempt_with_min_left(P, 1, _), N).

player_clutch_rating(P, R) :- clutch_percentage_of_player(P, Percentage), clutch_attempts_by_player(P, N), mgn(clutchAttemptsScale, W), R is ((1 - W)*Percentage) + W*N.

get_percentage(_, A, -1) :- A is 0.
get_percentage(M, A, R) :- A > 0, R is (M / A) * 100.

clutch_percentage_of_player(P, R) :- clutch_shots_by_player(P, Made), clutch_attempts_by_player(P, Attempts), get_percentage(Made, Attempts, R).
% list/team operations
clutch_players_on_team(Team, List) :- aggregate_all(set(X), played_for_team(X, Team), Players), clutch_rating_for_players(Players, List).

clutch_rating_for_players([], []).
clutch_rating_for_players([H|T], [(H,X)| T2]) :-
    clutch_attempts_by_player(H, A),
    A >= 1,
    player_clutch_rating(H, X), clutch_rating_for_players(T, T2).
clutch_rating_for_players([H|T], T2) :-
    clutch_attempts_by_player(H, A),
    A is 0,
    clutch_rating_for_players(T, T2).

% Shooting percentages
team_shooters_versus(T, Versus, Players) :- shooting_percentages_versus_team(T, Versus, Stats), insert_sort(Stats, Sorted), first_n(Sorted, 5, Players).

shooting_percentages_versus_team(T1, T2, R) :- players_on_team(T1, Players), shooting_percentages_versus_team_list(Players, T2, R).
shooting_percentages_versus_team_list([], _, []).
shooting_percentages_versus_team_list([H|T], Versus, [(H, X)| T2]) :-
    player_shooting_versus(H, Versus, X),
    shooting_percentages_versus_team_list(T, Versus, T2).


player_shooting_versus(Player, TeamName, R) :- player_shot_attempts_versus(Player, TeamName, Attempts), player_shot_makes_versus(Player, TeamName, Makes), get_percentage(Makes, Attempts, R).
player_shot_attempts_versus(Player, TeamName, Attempts) :- aggregate_all(count, player_shot_attempt_versus(Player, TeamName), Attempts).
player_shot_attempt_versus(Player, TeamName) :-
    rdf(E, 'http://stellman-greene.com/pbprdf#shotBy', Player),
    rdf(E, 'http://stellman-greene.com/pbprdf#inGame', Game),
    player_is_versing(Player, Game, TeamName).

player_shot_makes_versus(Player, TeamName, Makes) :- aggregate_all(count, player_shot_make_versus(Player, TeamName), Makes).
player_shot_make_versus(Player, TeamName) :-
    rdf(E, 'http://stellman-greene.com/pbprdf#shotBy', Player),
    rdf(E, 'http://stellman-greene.com/pbprdf#inGame', Game),
    rdf(E, 'http://stellman-greene.com/pbprdf#shotMade', literal(type('http://www.w3.org/2001/XMLSchema#boolean', true))),
    player_is_versing(Player, Game, TeamName).


% UTIL
% ===========
player_is_on_team(Player, Team) :-
    rdf(R, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://stellman-greene.com/pbprdf#Roster'),
    rdf(R, 'http://stellman-greene.com/pbprdf#hasPlayer', Player),
    rdf(R, 'http://stellman-greene.com/pbprdf#rosterTeam', Team).


player_is_versing(Player, Game, TeamName) :-
    teamName(VersusTeam, TeamName),
    rdf(Game, 'http://stellman-greene.com/pbprdf#awayTeam', VersusTeam),
    rdf(Game,  'http://stellman-greene.com/pbprdf#homeTeam', HomeTeam),
    player_is_on_team(Player, HomeTeam).
   
player_is_versing(Player, Game, TeamName) :-
    teamName(VersusTeam, TeamName),
    rdf(Game, 'http://stellman-greene.com/pbprdf#homeTeam', VersusTeam),
    rdf(Game, 'http://stellman-greene.com/pbprdf#awayTeam', AwayTeam),
    player_is_on_team(Player, AwayTeam).


% Comparator sort, implement the compare relationship for data types,
% where returns >0 if first item is less than second
insert_sort(List,Sorted):- i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted) :- insert(H, Acc, NAcc),i_sort(T, NAcc, Sorted).

insert(X,[Y|T],[Y|NT]) :- comparator(X, Y, R), R > 0, insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]) :- comparator(X, Y, R), R =< 0.
insert(X,[],[X]).

comparator((_, V1), (_, V2), R) :- comparator(V1, V2, R).
comparator(pair(_, V1), pair(_, V2), R) :- comparator(V1, V2, R).

comparator(X,Y, -1) :- number(X), number(Y), X > Y.
comparator(X,Y, 0) :- number(X), number(Y), X is Y.
comparator(X,Y, 1):- number(X), number(Y), X < Y.

first_n([], _, []).
first_n(_, 0, []).
first_n([H1|T1], N, [H1|R]) :- N > 0, M is (N - 1), first_n(T1, M, R).
