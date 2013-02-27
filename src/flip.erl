%%% BEGIN flip.erl %%%
%%%
%%% flip - Cat's Eye Technologies' Erlang FLIP
%%%
%%% This work is in the public domain.  See UNLICENSE for more information.
%%%

%% This is an implementation of the venerable and ancient game of Flip,
%% by by John S. James, which first appeared in the March/April 1977 edition
%% of Creative Computing.
%%
%% This implementaion is based largely on the version written in BASIC by
%% Steve North appearing in "More BASIC Computer Games", Ed. David H. Ahl
%% (ISBN 0-89480-137-6).

-module(flip).
-vsn('2002.0731').
-author('cpressey@gmail.com').
-copyright('This work is in the public domain; see UNLICENSE for more info').

-export([start/0]).

quantize(P) ->
  case random:uniform() of
    G when G >= P -> 1;
    _ -> 0
  end.

get_guess() ->
  L = io:get_line('? '),
  case L of
    "y" ++ _ -> 1;
    "Y" ++ _ -> 1;
    "n" ++ _ -> 0;
    "N" ++ _ -> 0;
    _ -> io:fwrite("ERROR, MUST BE  Y  OR  N  .~n"),
      get_guess()
  end.

compute_z2(Z1, F2) when Z1 < 0.5 -> Z1 * F2 + 1 * (1 - F2);
compute_z2(Z1, F2) when Z1 > 0.5 -> Z1 * F2 + 0 * (1 - F2);
compute_z2(Z1, F2) -> random:uniform().

start() ->
  {H,M,S} = time(),                                % this seems to be an adequate way to
  random:seed(S,M,H),                              % seed the random number generator
  P = {0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,     % 16 probabilities
       0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5},
  X = {quantize(0.5), quantize(0.5), quantize(0.5), quantize(0.5)}, % 4 responses
  application:load(flip),
  {ok, Trials} = application:get_env(flip, trials),
  io:fwrite("BEGIN.~n"),
  flip(P, X, 0.8, 0.3, 0, Trials, " ").

% F1 = 0.8 old-memory factor
% F2 = 0.3 randomness factor
% S1 = 0 score
% T  = trial counter (runs backwards)
% A = " " last result response to user

flip(P, X, F1, F2, S1, T, A) when T == 0 ->
  {ok, Trials} = application:get_env(flip, trials),
  io:fwrite("~s~nEND OF GAME.~nYOU GOT  ~w  OUT OF  ~w  CORRECT.~n",
    [A, S1, Trials]),
  ok;

flip(P, X, F1, F2, S1, T, A) ->
  % io:fwrite("P: ~w~nX: ~w~n", [P, X]),
  I9 = 8 * element(4, X) + 4 * element(3, X) + 2 * element(2, X) + element(1, X) + 1,
  Z1 = element(I9, P),
  Z2 = compute_z2(Z1, F2),
  Z5 = quantize(Z2),
  io:fwrite("~s", [A]),
  Z3 = get_guess(),
  NewX = {element(3, X), element(4, X), Z3, Z5},
  NewP = setelement(I9, P, F1 * element(I9, P) + (1 - F1) * element(3, X)),
  case Z3 == Z5 of
    true ->
      flip(NewP, NewX, F1, F2, S1 + 1, T - 1, "*");
    false ->
      flip(NewP, NewX, F1, F2, S1,     T - 1, " ")
  end.

%%% END of flip.erl %%%
