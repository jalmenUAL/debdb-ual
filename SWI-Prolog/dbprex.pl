:-use_module(library(http/http_open)).
:-use_module(library(clpfd)).
:-use_module(library(clpq)).
:-dynamic num_rule/1.
:-dynamic rule/3.
:-set_prolog_flag(character_escapes,false).

:-rdf_register_prefix(dbr,'http://dbpedia.org/resource/',[force(true)]).
:-rdf_register_prefix(dbo,'http://dbpedia.org/ontology/',[force(true)]).
:-rdf_register_prefix(dbp,'http://dbpedia.org/property/',[force(true)]).
:-rdf_register_prefix(yago,'http://dbpedia.org/class/yago/',[force(true)]).

%cd('/Users/jalmen/Google Drive/Mi unidad/Investigacion/dbpedia-pl').
%debdb(p,[p('http://dbpedia.org/resource/Switzerland'),p('http://dbpedia.org/resource/Germany')],[p('http://dbpedia.org/resource/Italy'),p('http://dbpedia.org/resource/San_Marino')],Query,Constraints,Replacements).

/*
p(COUNTRY):-rdf(COUNTRY,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/Country'),rdf(COUNTRY,'http://dbpedia.org/ontology/populationTotal',POPULATION),rdf(COUNTRY,'http://dbpedia.org/ontology/areaTotal',AREA),rdf(COUNTRY,'http://dbpedia.org/ontology/grossDomesticProduct',GDP),POPULATION=A1,50000000^^'http://www.w3.org/2001/XMLSchema#integer'=B2,{ A1>B2 },AREA=A4,3000000^^'http://www.w3.org/2001/XMLSchema#integer'=B5,{ A4<B5 },GDP=A7,1000000000000^^'http://www.w3.org/2001/XMLSchema#integer'=B8,{ A7>B8 }.
*/

/*
p(COUNTRY):-
       rdf(COUNTRY,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/class/yago/WikicatCountriesInEurope'),
       rdf(COUNTRY,'http://dbpedia.org/ontology/currency','http://dbpedia.org/resource/Euro'),
     rdf(COUNTRY,'http://dbpedia.org/ontology/officialLanguage','http://dbpedia.org/resource/Italian_language'),
     rdf(COUNTRY,'http://dbpedia.org/ontology/populationTotal',POPULATION),
      POPULATION>10000000.
 */

/*
p(MOVIE):-rdf(MOVIE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/Film'),rdf(MOVIE,'http://dbpedia.org/ontology/director','http://dbpedia.org/resource/Martin_Scorsese'),rdf(MOVIE,'http://dbpedia.org/ontology/starring','http://dbpedia.org/resource/Leonardo_DiCaprio'),rdf(MOVIE,'http://dbpedia.org/ontology/runtime',RUNTIME),RUNTIME=A0_1,VAR1=A1,'http://www.w3.org/2001/XMLSchema#float'(A0_1,VAR1),90^^'http://www.w3.org/2001/XMLSchema#integer'=B4,{ A1>B4 }.
*/


/*
p(PLANET):-rdf(PLANET,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/Planet'),rdf(PLANET,'http://dbpedia.org/ontology/volume',VOLUME),rdf(PLANET,'http://dbpedia.org/ontology/maximumTemperature',TEMPMAX),rdf(PLANET,'http://dbpedia.org/ontology/minimumTemperature',TEMPMIN),rdf(PLANET,'http://dbpedia.org/ontology/meanOrbitalSpeed',ORBITALSPEED),VOLUME=A0_1,VAR1=A1,'http://www.w3.org/2001/XMLSchema#float'(A0_1,VAR1),0^^'http://www.w3.org/2001/XMLSchema#integer'=B4,{ A1>B4 },TEMPMAX=A0_6,VAR6=A6,'http://www.w3.org/2001/XMLSchema#float'(A0_6,VAR6),-273^^'http://www.w3.org/2001/XMLSchema#integer'=B9,{ A6>B9 },TEMPMIN=A0_11,VAR11=A11,'http://www.w3.org/2001/XMLSchema#float'(A0_11,VAR11),-273^^'http://www.w3.org/2001/XMLSchema#integer'=B14,{ A11>B14 },ORBITALSPEED=A0_16,VAR16=A16,'http://www.w3.org/2001/XMLSchema#float'(A0_16,VAR16),0^^'http://www.w3.org/2001/XMLSchema#integer'=B19,{ A16>B19 }.
*/

/*
p(UNIVERSITY):-rdf(UNIVERSITY,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/University'),rdf(UNIVERSITY,'http://dbpedia.org/ontology/country','http://dbpedia.org/resource/Germany'),rdf(UNIVERSITY,'http://dbpedia.org/ontology/numberOfStudents',STUDENTS),STUDENTS=A1,500^^'http://www.w3.org/2001/XMLSchema#integer'=B2,{ A1<B2 }.
*/
 
%First example

/*
p(MOVIE):-rdf(MOVIE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/Film'),rdf(MOVIE,'http://dbpedia.org/ontology/runtime',RUNTIME),rdf(MOVIE,'http://dbpedia.org/ontology/budget',BUDGET),rdf(MOVIE,'http://dbpedia.org/ontology/gross',BOXOFFICE),RUNTIME=A1,90^^'http://www.w3.org/2001/XMLSchema#integer'=B2,{ A1>B2 },BUDGET=A4,50000000^^'http://www.w3.org/2001/XMLSchema#integer'=B5,{ A4>B5 },BOXOFFICE=A7,200000000^^'http://www.w3.org/2001/XMLSchema#integer'=B8,{ A7>B8 }.
*/

%Second Example
/*p(MOVIE):-
       rdf(MOVIE,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/Film'),
       rdf(MOVIE,'http://dbpedia.org/ontology/director','http://dbpedia.org/resource/Martin_Scorsese'),
       rdf(MOVIE,'http://dbpedia.org/ontology/starring','http://dbpedia.org/resource/Leonardo_DiCaprio'),
       rdf(MOVIE,'http://dbpedia.org/ontology/runtime',RUNTIME),
       RUNTIME=A1,
       90^^'http://www.w3.org/2001/XMLSchema#integer'=B2,
       { A1>B2 }.
*/

%Third example

/*
p(UNIVERSITY):-rdf(UNIVERSITY,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/University'),rdf(UNIVERSITY,'http://dbpedia.org/ontology/country','http://dbpedia.org/resource/Germany'),rdf(UNIVERSITY,'http://dbpedia.org/ontology/numberOfStudents',STUDENTS),STUDENTS=A1,500^^'http://www.w3.org/2001/XMLSchema#integer'=B2,{ A1<B2 }.
*/

%Fourth example

/*
p(PLANET):-rdf(PLANET,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://dbpedia.org/ontology/Planet'),rdf(PLANET,'http://dbpedia.org/ontology/volume',VOLUME),rdf(PLANET,'http://dbpedia.org/ontology/maximumTemperature',TEMPMAX),rdf(PLANET,'http://dbpedia.org/ontology/minimumTemperature',TEMPMIN),rdf(PLANET,'http://dbpedia.org/ontology/orbitalSpeed',ORBITALSPEED),VOLUME=A1,0^^'http://www.w3.org/2001/XMLSchema#integer'=B2,{ A1>B2 },TEMPMAX=A4,-273^^'http://www.w3.org/2001/XMLSchema#integer'=B5,{ A4>B5 },TEMPMIN=A7,-273^^'http://www.w3.org/2001/XMLSchema#integer'=B8,{ A7>B8 },ORBITALSPEED=A10,0^^'http://www.w3.org/2001/XMLSchema#integer'=B11,{ A10>B11 }.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%AUXILIAR FUNCTIONS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conjunction_to_list((A,B), L) :-!,	conjunction_to_list(A, L0),
  			conjunction_to_list(B, L1),
  			append(L0, L1, L).
conjunction_to_list(A, [A]).

list_to_conjunction([X],X):-!.
list_to_conjunction([X|L],(X,PL)):-list_to_conjunction(L,PL).

genvars([]).
genvars([_|L]):-genvars(L).

match(Free,R):-genvars(Vars),	 
	      R=..[_|Vars],
	      append(_,Free,Vars),!.

syntactic_member(X,L):-member(Y,L),X==Y,!.

%%%%%%%%%%%%%%%%%%%%%WEAK RULES GENERATOR%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clause_gen(_):-retractall(num_rule(_)),assert(num_rule(0)),retractall(rule(_,_,_)),fail.
clause_gen(Pred):-genvars(Vars), Q=..[Pred|Vars], clause(Q,C),!, 
			conjunction_to_list(C,LC),
			generate_rule([],LC,WC,L),
			list_to_conjunction(WC,CC),
			append(Vars,L,Variables),
			NQ=..[Pred|Variables],
			num_rule(N),M is N+1,retract(num_rule(N)),
			assert(num_rule(M)),assert(rule(M,NQ,CC)),fail.

generate_rule(_,[],[],[]):-!.
generate_rule(First,[X|L],L2,V):-X=..['{}'|[E]],!,generate_rule(First,[E|L],L2,V).
generate_rule(First,[X|L],L2,V):-X=..['='|[E,F]],!,E=F,generate_rule(First,L,L2,V).
generate_rule(First,[X|L],[Y|L2],V):-rdf_weak(X,Y,V1),generate_rule([X|First],L,L2,V2),append(V1,V2,V).
generate_rule(First,[rdf(A,P,C)|L],L2,V):- rdf_global_id(NS:_,P),NS\=rdf,NS\=rdfs,generate_rule([rdf(A,P,C)|First],L,L2,V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%WEAKENING OF RDF AND CONSTRAINTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rdf_weak(rdf(A,P,C),rdf(A,P,C),[]):-rdf_global_id(X:_,P),(X=rdf;X=rdfs),!.
rdf_weak(rdf(A,B,C),rdf(A,B,C),[]).
rdf_weak(rdf(A,B,C),rdf(A,B,D),[replaced(C,D)]):-ground(C),ground(A). %object unbinding
rdf_weak(rdf(A,B,C),rdf(A,B,D),[replaced(C,D)]):-ground(C),ground(B). %object unbinding
rdf_weak(rdf(A,B,C),rdf(D,B,C),[replaced(A,D)]):-ground(A),ground(B). %subject unbinding
rdf_weak(rdf(A,B,C),rdf(D,B,C),[replaced(A,D)]):-ground(A),ground(C). %subject unbinding
rdf_weak(rdf(A,B,C),rdf(A,D,C),[replaced(B,D)]):-ground(B),ground(C). %property unbinding
rdf_weak(rdf(A,B,C),rdf(A,D,C),[replaced(B,D)]):-!,ground(A),ground(B). %property unbinding
rdf_weak(F,FW,L):-rdf_weak_exp(F,FW,L).

rdf_weak_exp(A,A,[]):-var(A),!.
rdf_weak_exp(C,WA,L):-C=..['^^'|[A,_]],!,rdf_weak_exp(A,WA,L).
rdf_weak_exp(B,A,[replaced(B,A)]):-atomic(B),ground(B),!.
rdf_weak_exp(F,FW,V):-F=..[Op,A,B],rdf_weak_exp(A,WA,VA),
			rdf_weak_exp(B,WB,VB),  
			FW=..[Op,WA,WB],
			append(VA,VB,V).
 


%%%%%%%%%%%%%%%%%%%%DEBUGGER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debdb(p,P,Q,TP,SV,RV):- deb(p,P,Q,B,V),
					recommended(B,TP),
					term_variables(V,PV),
					show_variables(PV,SV,VARS),
					show_replacements(VARS,V,RV).
					


deb(_,P,Q,_,_):-member(X,P),member(X,Q),write("There is a positive and negative element at the same time"),nl,!.
deb(Pred,_,_,_,_):-clause_gen(Pred),fail.
deb(_,P,Q,C,Free):-rule(N,_,_),deb_step(N,P,Q,C,Free).
deb_step(N,P,Q,C,Free):- pos(N,P,Free),  
			 neg(N,Q,Free),  
			 rule(N,R,C),		 
			 match(Free,R).


pos(N,P,Free):-  conjunction_sparql(P,N,Free,Pattern1,Const1,S),  
		 (P=[]->true;
		 (ground(Pattern1)-> 
			(
			
			concat("ASK WHERE {",S,St1FA),concat(St1FA," }",St1FFA), 
			 
			sparql_query(St1FFA,Row1FA,[ host('dbpedia.org'), path('/sparql/')]),
			Row1FA=true
			);
			(	
				concat("SELECT * WHERE {",S,St1F),concat(St1F," }",St1FF), 
				 
			 	sparql_query(St1FF,Row1F,[ host('dbpedia.org'), path('/sparql/')]),
				
			 	Row1F=..[row|LRow1F],
			 	term_variables(Pattern1,VarsC1),
			 	VarsC1=LRow1F,
				  
			 	solvep(Const1,_)
			))).

neg(N,Q,Free):-  union_sparql(Q,N,Free,Pattern2,_,S2),
			  conjunction_sparql(Q,N,Free,Pattern3,Const3,S3),
		 (Q=[]->true;
		 (ground(Pattern2)->
                        ( 
			concat("ASK WHERE {",S2,St2FA),concat(St2FA," }",St2FFA),
			 
			sparql_query(St2FFA,Row2FA,[ host('dbpedia.org'), path('/sparql/')]),
			Row2FA=false
			);
			(
				concat("SELECT * WHERE {",S2,St2F),concat(St2F," }",St2FF),		 
			 		   
					sparql_query(St2FF,Row2F,[ host('dbpedia.org'), path('/sparql/')])->
			 		(
					  Row2F=..[row|LRow2F],
			 		  term_variables(Pattern2,VarsC2),
			 		  VarsC2=LRow2F,
					 concat("SELECT * WHERE {",S3,St3F),concat(St3F," }",St3FF),
					  
					 (sparql_query(St3FF,Row3F,[ host('dbpedia.org'), path('/sparql/')])->
					 (Row3F=..[row|LRow3F],
			 		  term_variables(Pattern3,VarsC3),
			 		  VarsC3=LRow3F,
					  (Const3\=[]->solven(Const3,_);false));true)
					)
					;true
				 
			))).


 


%%%%%%%%%%%PRINT SOLUTION%%%%%%%%%%%%%%%%%%%%%%%%%%%

recommended(C,St):-recommended_list(C,"",St).

recommended_list(C,I,O):-C=..[',',A,RA],!,
				recommended_rdf_string(A,SA),
				concat(I,SA,IA),
				recommended_list(RA,IA,O).
recommended_list(C,I,IC):-recommended_rdf_string(C,SC),
				concat(I,SC,IC).

recommended_rdf_string(rdf(A,B,C),Rdf2):-!,rdfterm_string(A,SA),
						concat(SA," ",SSA),
						rec_rdfterm_string(B,SB),
						concat(SSA,SB,AB),
						rec_rdfterm_string(C,SC),
						concat(" ",SC,SSC),
						concat(AB,SSC,Rdf),
						concat(Rdf," . ",Rdf2).
recommended_rdf_string(Lit,""):-Lit=..[Op,_,_],string_concat('http://www.w3.org/2001/XMLSchema#',_,Op),!.
recommended_rdf_string(_,"").
						
rec_rdfterm_string(A,SA):-var(A),!,
				term_string(A,SV),
				concat("?",SV,SA).
				
rec_rdfterm_string(A,A):-integer(A),!.
rec_rdfterm_string(A,A):-float(A),!.
rec_rdfterm_string(literal(type(_,V)),VV):-!,atom_number(V,VV).
rec_rdfterm_string(Op,SSOp):-Op=..[F,A,B],!,
				rec_rdfterm_string(A,SA),
				rec_rdfterm_string(B,SB),
				concat(SA,F,SOp),
				concat(SOp,SB,SSOp).
rec_rdfterm_string(A,Z):-rdf_global_id(X:Y,A),
				concat(X,':',XC),
				concat(XC,Y,Z).		

show_variables([],[],[]):-!.
show_variables([X|RX],[SX|SRX],[X|RVS]):-show_variable(X,SX),!,show_variables(RX,SRX,RVS).
show_variables([_|RX],SRX,RVS):-show_variables(RX,SRX,RVS).


show_variable(X,TFX):-fd_dom(X,DX),DX=inf..sup,term_string(X,SX),concat("?",SX,ISX),
				 dump([X],[ISX],[TX]),!,
				 maplist(convert_comma, [TX], [TFX]). 
show_variable(X,TX):-fd_dom(X,DX),DX\=inf..sup,term_string(X,SX),concat("?",SX,ISX),term_string(DX,SDX),concat(ISX," in ",SXIN),concat(SXIN,SDX,TX).	
 
show_replacements(_,[],[]):-!.
show_replacements(_,[replaced(X,Y)|_],_):-X==Y,!,fail.
show_replacements(VARS,[replaced(X,Y)|RR],[replaced(X,SY)|RS]):-var(Y),syntactic_member(Y,VARS),!,term_string(Y,SV),
				concat("?",SV,SY),show_replacements(VARS,RR,RS).
show_replacements(VARS,[replaced(_,Y)|RR],RS):-var(Y),!,show_replacements(VARS,RR,RS).
show_replacements(VARS,[replaced(X,Y)|RR],[replaced(X,Y)|RS]):-show_replacements(VARS,RR,RS).
					
convert_comma(Exp,Exp) :- atom(Exp),!.
convert_comma(Exp,Exp) :- var(Exp),!.
convert_comma(R,F):-number(R),F is float(R),!.
convert_comma(Exp,FExp) :- Exp =..[Op|L],convert_comma_list(L,FL),FExp=..[Op|FL].

convert_comma_list([],[]).
convert_comma_list([E|RE],[FE|RFE]):-convert_comma(E,FE),convert_comma_list(RE,RFE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conjunction_sparql([],_,_,[],[],"").
conjunction_sparql([F|RF],N,Free,Pattern3,Const3,StS):-	
			rule(N,Q,C), 
			F=..[H|Args],
			Q=..[H|All],
			append(Args,Free,All),!,
			launched(C,StF,Pattern,Const),
			concat(" { ",StF,StFP),
			concat(StFP," } ",StFPP), 	
			conjunction_sparql(RF,N,Free,Pattern2,Const2,StRF),
			(RF=[] -> concat(StFPP,StRF,StS),Pattern3=Pattern,Const3=Const;
			(concat(StFPP,StRF,StS),
			 append(Pattern,Pattern2,Pattern3),
			 append(Const,Const2,Const3))).


union_sparql([],_,_,[],[],"").
union_sparql([F|RF],N,Free,Pattern3,Const3,StS):-	
			rule(N,Q,C), 
			F=..[H|Args],
			Q=..[H|All],
			append(Args,Free,All),!,
			launched(C,StF,Pattern,Const),
			concat(" { ",StF,StFP),
			concat(StFP," } ",StFPP), 
			union_sparql(RF,N,Free,Pattern2,Const2,StRF),
			(RF=[] -> concat(StFPP,StRF,StS),Pattern3=Pattern,Const3=Const;
			(concat(" UNION ",StRF,StRFU),
			concat(StFPP,StRFU,StS),
			append(Pattern,Pattern2,Pattern3),
			append(Const,Const2,Const3))).
			
 
launched(C,St,Pattern,Const):-launched_list(C,"",St,Pattern,Const).

launched_list(C,I,O,Pattern,Const):-C=..[',',A,RA],!,
				launched_rdf_string(A,SA,PatternA,ConstA),
				concat(I,SA,IA),
				launched_list(RA,IA,O,PatternRA,ConstRA),
				append(PatternA,PatternRA,Pattern),
				append(ConstA,ConstRA,Const).

launched_list(C,I,IC,Pattern,Const):-launched_rdf_string(C,SC,Pattern,Const),
				concat(I,SC,IC).

launched_rdf_string(rdf(A,B,C),Rdf2,[rdf(A,B,C)],[]):-!,rdfterm_string(A,SA),
						rdfterm_string(B,SB),
						concat(SA,SB,AB),
						rdfterm_string(C,SC),
						concat(AB,SC,Rdf),
						concat(Rdf," . ",Rdf2).

launched_rdf_string(Op,"",[],[Op]).

rdfterm_string(A,SA):-var(A),!,term_string(A,SV),concat("?",SV,SA).
rdfterm_string(A,A):-integer(A),!.
rdfterm_string(A,A):-float(A),!.
rdfterm_string(V^^_,V):-!.
rdfterm_string(literal(type(_,V)),VV):-!,atom_number(V,VV).
rdfterm_string(A,SA):-concat(" <",A,A1),concat(A1,"> ",SA).	

%%%%%%%%%%%%%%%%%CONSTRAINT SOLVER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solvep([],[]).
solvep([C|RC],[NC|RNC]):- transp(C,NC),  call(NC), solvep(RC,RNC).

solven([],[]).
solven([C|RC],[NC|RNC]):- transn(C,NC),  call(NC), solven(RC,RNC).

transp(C,true):-C=..[Op,A,B],string_concat('http://www.w3.org/2001/XMLSchema#',_,Op),!,A=B.
transp(C,NCC):-C=..[Op,A,B],trans_lit(A,LA,TA),trans_lit(B,LB,TB),TA\=real,TB\=real,!,concat('#',Op,OpS),NCC=..[OpS,LA,LB].
transp(C,NCC):-C=..[Op,A,B],trans_lit(A,LA,real),trans_lit(B,LB,_),!,NC=..[Op,LA,LB],NCC=clpq:{NC}.
transp(C,NCC):-C=..[Op,A,B],trans_lit(A,LA,_),trans_lit(B,LB,real),!,NC=..[Op,LA,LB],NCC=clpq:{NC}.
transn(C,true):-C=..[Op,A,B],string_concat('http://www.w3.org/2001/XMLSchema#',_,Op),!,A=B.
transn(C,NCC):-C=..[Op,A,B],inv(Op,IOp),trans_lit(A,LA,TA),trans_lit(B,LB,TB),TA\=real,TB\=real,!,concat('#',IOp,OpS),NCC=..[OpS,LA,LB].
transn(C,NCC):-C=..[Op,A,B],inv(Op,IOp),trans_lit(A,LA,real),trans_lit(B,LB,_),!,NC=..[IOp,LA,LB],NCC=clpq:{NC}.
transn(C,NCC):-C=..[Op,A,B],inv(Op,IOp),trans_lit(A,LA,_),trans_lit(B,LB,real),NC=..[IOp,LA,LB],NCC=clpq:{NC}.

trans_lit(A,A,var):-var(A),!.
trans_lit(A,A,var):-atomic(A),!.
trans_lit(literal(type(_, V)),V,integer):-integer(V),!.
trans_lit(literal(type(_, V)),V,real):-float(V),!.
trans_lit(literal(type(T, V)),VV,integer):-is_integer(T),!,atom_number(V,VV).
trans_lit(literal(type(_, V)),VV,real):-!,atom_number(V,VV).
trans_lit(C,D,integer):-C=..[Agg,A,B],trans_lit(A,LA,integer),trans_lit(B,LB,integer),!,D=..[Agg,LA,LB].
trans_lit(C,D,integer):-C=..[Agg,A,B],trans_lit(A,LA,var),trans_lit(B,LB,integer),!,D=..[Agg,LA,LB].
trans_lit(C,D,integer):-C=..[Agg,A,B],trans_lit(A,LA,integer),trans_lit(B,LB,var),!,D=..[Agg,LA,LB].
trans_lit(C,D,real):-C=..[Agg,A,B],trans_lit(A,LA,_),trans_lit(B,LB,_),D=..[Agg,LA,LB].

inv('>','=<').
inv('<','>=').
inv('=','=\=').
inv('=\=','=').
inv('>=','<').
inv('=<','>').

is_integer('http://www.w3.org/2001/XMLSchema#decimal').
is_integer('http://www.w3.org/2001/XMLSchema#integer').
is_integer('http://www.w3.org/2001/XMLSchema#int').
is_integer('http://www.w3.org/2001/XMLSchema#long').
is_integer('http://www.w3.org/2001/XMLSchema#short').
is_integer('http://www.w3.org/2001/XMLSchema#byte').
is_integer('http://www.w3.org/2001/XMLSchema#unsignedLong').
is_integer('http://www.w3.org/2001/XMLSchema#unsignedInt').
is_integer('http://www.w3.org/2001/XMLSchema#unsignedShort').
is_integer('http://www.w3.org/2001/XMLSchema#unsignedByte').
is_integer('http://www.w3.org/2001/XMLSchema#negativeInteger').
is_integer('http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
is_integer('http://www.w3.org/2001/XMLSchema#positiveInteger').
is_integer('http://www.w3.org/2001/XMLSchema#nonPositiveInteger').

