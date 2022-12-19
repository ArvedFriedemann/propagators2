declare Run in
proc {Run R}
   or B K in
      % prove B with B from K
      thread R=['prove' B 'with' B 'from' K] end
   [] `b` B `f` `p'` Pp Qp K `p` P Q in
      % exchange p' with p in (f : (p' : P') ~> Q') to (f : (p : P) ~> Q) ->
      %  prove (p : P) from K ->
      %  prove (b : B) with (f p : Q) from K ->
      %  prove (b : B) with (f : (p' : P') ~> Q') from K
      thread {Run ['exchange' `p'` 'with' `p` 'in' [`f` ':' [[`p'` ':' Pp] ['->' Qp]] ] 'to' [`f` ':' [[`p` ':' P] ['->' Q]] ]]} end
      thread {Run ['prove' [`p` ':' P] 'from' K]} end
      thread {Run ['prove' [`b` ':' B] 'with' [[`f` `p`] ':' Q] 'from' K]} end
      thread R=['prove' [`b` ':' B] 'with' [`f` ':' [[`p'` ':' Pp] ['->' Qp]] ] 'from' K] end
   [] `b` A B K in
      % prove B from (A ,- K) -> prove (b : (A ~> B)) from K
      thread {Run ['prove' B 'from' (A|K)]} end
      thread R=['prove' [`b` ':' [A ['->' B]]] 'from' K] end
   [] B K P in
      % P is in K -> (prove B with P from K) -> prove B from K
      thread {List.member P K true} end
      thread {Run ['prove' B 'with' P 'from' K]} end
      thread R=['prove' B 'from' K] end

   [] `p` `p'` in
      % exchange x with y in x to y
      thread R=['exchange' `p` 'with' `p'` 'in' `p` 'to' `p'`] end
   [] `p` `p'` `k` in
      % (e /= x) -> (e /= (e1 e2)) -> exchange x with y in e to e
      thread`p`==`k`=false end
      thread`k`==(_|_)=false end
      thread R=['exchange' `p` 'with' `p'` 'in' `k` 'to' `k`] end
   [] `p` `p'` E Es Ep Esp in
      % exchange x with y in e1 to e1' ->
      %   exchange x with y in e2 to e2' ->
      %   exchange x with y in (e1 e2) to (e1' e2')
      thread {Run ['exchange' `p` 'with' `p'` 'in' E 'to' Ep]} end
      thread {Run ['exchange' `p` 'with' `p'` 'in' Es 'to' Esp]} end
      thread R=['exchange' `p` 'with' `p'` 'in' (E|Es) 'to' (Ep|Esp)] end
   end  
end

declare Run2 in
proc {Run2 R}
   {Browse 'call to Run2'}
   or A B in
      thread R=['exchange' A 'with' B 'in' A 'to' B] end
   [] A B E Es Ep Esp in
      thread {Browse 'recursive case'} end
      thread {Run2 ['exchange' A 'with' B 'in' E 'to' Ep]} end
      thread {Run2 ['exchange' A 'with' B 'in' Es 'to' Esp]} end
      thread R=['exchange' A 'with' B 'in' (E|Es) 'to' (Ep|Esp)] end
   [] A K in
      thread K==A=false end
      thread K==(_|_)=false end
      thread R=['exchange' A 'with' _ 'in' K 'to' K] end
   end
end


declare I R ID P in
I=['exchange' a 'with' b 'in' [a a] 'to' P]

{Run2 I}

{Browse I}

%ID=['prove' ['p' ':' [['a' ':' 'A'] ['->' [P ':' 'A']]]]]

%{Run ID}
%{Browse ID}


