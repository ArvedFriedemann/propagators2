% prove B with B from K
run(['prove', B, 'with', B, 'from', K]).

% exchange p' with p in (f : (p' : P') ~> Q') to (f : (p : P) ~> Q) ->
%  prove (p : P) from K ->
%  prove (b : B) with (f p : Q) from K ->
%  prove (b : B) with (f : (p' : P') ~> Q') from K
run(['prove', [_b, ':', B], 'with', [_f, ':', [[_pp, ':', Pp], ['->', Qp]] ], 'from', K]) :-
  run(['exchange', _pp, 'with', _p, 'in', [_f, ':', [[_pp, ':', Pp], ['->', Qp]] ], 'to', [_f, ':', [[_p, ':', P], ['->', Q]] ]]),
  run(['prove', [_b, ':', B], 'with', [[_f, _p], ':', Q], 'from', K]),
  run(['prove', [_p, ':', P], 'from', K]).

% prove B from (A ,- K) -> prove (b : (A ~> B)) from K
run(['prove', [_b, ':', [A, ['->', B]]], 'from', K]) :- 
  run(['prove', B, 'from', [A|K]]).
% prove B from (A ,- K) -> prove (A ~> B) from K
run(['prove', [A, ['->', B]], 'from', K]) :- 
  run(['prove', B, 'from', [A|K]]).

% P is in K -> (prove B with P from K) -> prove B from K
run(['prove', B, 'from', K]) :-
  member(P,K),
  run(['prove', B, 'with', P, 'from', K]).

% prove B from [] -> prove B
run(['prove',B]) :- run(['prove',B,'from',[]]).

% exchange x with y in x to y
run(['exchange', _x, 'with', _y, 'in', _x, 'to', _y]).
% (e /= x) -> (e /= (e1 e2)) -> exchange x with y in e to e
run(['exchange', _x, 'with', _y, 'in', _e, 'to', _e]) :- _x\=_e, _e\=[_|_].
% exchange x with y in e1 to e1' ->
%   exchange x with y in e2 to e2' ->
%   exchange x with y in (e1 e2) to (e1' e2')
run(['exchange', _x, 'with', _y, 'in', [E|Es], 'to', [Ep|Esp]]) :- 
  run(['exchange', _x, 'with', _y, 'in', E, 'to', Ep]),
  run(['exchange', _x, 'with', _y, 'in', Es, 'to', Esp]).
  
%queries:

% prove (p : (a : A) ~> (P : A))
% run(['prove',['p',':',[['a',':','A'],['->',[P,':','A']]]]]).

% prove (p : (a : A) ~> (f : (a' : A) ~> B) ~> (P : B))
% run(['prove',['p',':',[['a',':','A'],['->',[['f',':',[['ap',':','A'],['->','B']]],['->', [P,':','B']]]]] ]]).

% prove (p : (A : Set) ~> (f : (K : Set) ~> (a : K) ~> B) ~> (P : B))
% run(['prove',['p',':',[['A',':','Set'],['->',[['f',':',[['K',':','Set'],['->',[['a',':','K'],['->','B']]]]],['->',[P,':','B']]]]]]]).


