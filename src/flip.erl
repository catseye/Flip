%%% BEGIN flip.erl %%%
%%%
%%% flip - Cat's Eye Technologies' Erlang FLIP
%%% Copyright (c)2002 Cat's Eye Technologies.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

%% @doc The venerable and ancient computer game of FLIP.
%%
%% <p>This version is coded in Erlang.  It is based on the
%% original game of FLIP by John S. James, as implemented in
%% BASIC by Steve North in <i>Creative Computing</i>, March/April 1977.</p>
%%
%% @end

-module(flip).
-vsn('2002.0731').
-author('catseye@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

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

%% @spec start() -> ok
%% @doc Plays a game of FLIP.

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
