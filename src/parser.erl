%%====================================================================
%%
%% @author Zach Smith <zd@zdsmith.com>
%% @copyright CC Attribution - 2016
%%
%% @doc This module implements a parser for the New Abbreviations. The purpose
%% is to transform English text into a structured representation of a series of
%% <em>glyphs</em>, which are the abbreviations themselves. Its intention is,
%% insofar as the abbreviations are regular and systematic, to serve as a
%% "reference implementation"; not to describe how they are rendered, of
%% course, but to provide a reference for how any text should be abbreviated.
%% @end
%%====================================================================
-module(parser).

-behaviour(gen_server).

% interface calls
-export([parse/1, start/0, stop/0]).

% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-ifndef(PRINT).
-define(PRINT(Var),
        io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n",
                  [?MODULE, ?LINE, ??Var, Var])).
-endif.

%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

%% Stopping server asynchronously
stop() -> gen_server:cast(?MODULE, shutdown).

%%====================================================================
%% types
%%====================================================================
-type abbreviation_type() :: word | prefix | suffix |
			     suffix_transform | voc | inter.

-type abbreviation_value() :: atom().

-type abbreviation() :: {abbreviation_type(),
			 abbreviation_value()}.

%%====================================================================
%% gen_server callbacks
%%====================================================================
-type init_arg() :: ignore | {ok, _} | {stop, _} |
		    {ok, _, hibernate | infinity | non_neg_integer()}.

-spec init(init_arg()) -> {ok, [trie:trie()]}.

init(_) ->
    process_flag(trap_exit, true),
    {ok, TermList} = file:consult(code:priv_dir(parser) ++
				    "/tna.dict"),
    Categorized = lists:foldl(fun categorize/2,
			      {[], [], []}, TermList),
    MakeTrie = fun (TypedWords) ->
		       Pairs = make_pairs(TypedWords, []), trie:new(Pairs)
	       end,
    Tries = [MakeTrie(TypedWords)
	     || TypedWords <- tuple_to_list(Categorized)],
    {ok, Tries}.

%% @doc Sort abbreviations into stages of application.
-type stages() :: {[abbreviation()], [abbreviation()],
		   [abbreviation()]}.

-spec categorize({abbreviation_type(),
		  [abbreviation_value()]},
		 stages()) -> stages().

categorize({Type, Values}, Acc = {_, _, _}) ->
    Prec = case Type of
	     suffix_transform -> 1;
	     word -> 2;
	     suffix -> 2;
	     prefix -> 3;
	     _ -> 3
	   end,
    Abbreviations = [{Type, Value} || Value <- Values],
    setelement(Prec, Acc,
	       element(Prec, Acc) ++ Abbreviations).

%% @doc Generate pairs of search patterns and abbreviations for populating
%% tries.
-type trie_entry() :: {string(), abbreviation()}.

-spec
     make_trie_entries(abbreviation()) -> [trie_entry()].

make_trie_entries(Abbreviation = {Type, Name}) ->
    String = atom_to_list(Name),
    Patterns = case Type of
		 suffix_transform -> [[$* | String]];
		 suffix -> [[$* | String]];
		 prefix -> [String ++ "*"];
		 inter ->
		     ["*" ++ String, "*" ++ String ++ "*", String ++ "*"];
		 voc -> ["*" ++ String, "*" ++ String ++ "*"];
		 _ -> [String]
	       end,
    [{Pattern, Abbreviation} || Pattern <- Patterns].

-spec make_pairs([abbreviation()],
		 [trie_entry()]) -> [trie_entry()].

make_pairs(Abbreviations, Pairs) ->
    NewPairs =
	lists:flatten([make_trie_entries(Abbreviation)
		       || Abbreviation <- Abbreviations]),
    NewPairs ++ Pairs.

%% Synchronous, possible return values
% {reply,Reply,NewState}
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState}
% {stop,Reason,NewState}
%% @private
handle_call({parse, S}, _, Tries) ->
    Tokens = string:tokens(string:to_lower(S), " ,."),
    Reply = [parse_token(Token, Tries) || Token <- Tokens],
    {reply, Reply, Tries}.

%% @doc The main entrypoint for parsing. Since every word can be assumed to be
%% abbreviated independently of the words around it, the parsing logic can be
%% seen as applying parse_token/2 to every token in the stream.
-type local_abbreviation() :: {TokenIndex :: integer(),
			       abbreviation_type() | token, atom() | string()}.

-type abbreviated_token() :: [local_abbreviation()].

-type abnode() :: {string(), abbreviated_token()}.

-spec parse_token(string(), [trie:trie()]) -> abnode().

parse_token(Token, Tries) ->
    Abbreviated = lists:foldl(fun (Trie, T) ->
				      abbreviate(Token, T, [], Trie)
			      end,
			      [{0, token, Token}], Tries),
    {Token, Abbreviated}.

%% @doc Match into the trie on a token. If the match is a pattern with
%% remainder, recurse until all remainders have been matched or marked as seen.
-spec abbreviate(string(), [local_abbreviation()],
		 [local_abbreviation()],
		 trie:trie()) -> abbreviated_token().

abbreviate(Headword,
	   [{_, token, Token} = FirstPart | Rest], Acc, Trie) ->
    {Rest2, Acc2} = case catch trie:find_match(Token, Trie)
			of
		      {ok, Key, {Type, Value}} ->
			  F = fun (Segment, {Unparsed, Parsed}) ->
				      case Segment of
					{exact, Match} ->
					    Index = string:str(Headword, Match),
					    Parsed2 = [{Index, Type, Value}
						       | Parsed],
					    {Unparsed, Parsed2};
					Remainder ->
					    Index = string:str(Headword,
							       Remainder),
					    Unparsed2 = [{Index, token,
							  Remainder}
							 | Unparsed],
					    {Unparsed2, Parsed}
				      end
			      end,
			  case trie:is_pattern(Key) of
			    true ->
				Segments = trie:pattern_parse(Key, Token,
							      expanded),
				{Unparsed, Parsed} = lists:foldl(F, {[], []},
								 Segments),
				{Unparsed ++ Rest, Parsed ++ Acc};
			    _ ->
				Index = string:str(Token, Key),
				NewNode = {Index, Type, Value},
				{Rest, [NewNode | Acc]}
			  end;
		      _ -> {Rest, [FirstPart | Acc]}
		    end,
    abbreviate(Headword, Rest2, Acc2, Trie);
abbreviate(Headword, [FirstPart | Rest], Acc, Trie) ->
    % If we encounter a non-token, it has already been parsed by a previous
    % pass.
    abbreviate(Headword, Rest, [FirstPart | Acc], Trie);
abbreviate(_, [], Acc, _) -> lists:sort(Acc).

%% @doc Parse a string of English text into a sequence of <em>Abbreviation
%% Nodes</em>, structured data representing the application of abbreviation
%% rules onto the text.
-spec parse(string()) -> [abnode()].

parse(S) -> gen_server:call(?MODULE, {parse, S}).

%% Asynchronous, possible return values
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
%% @private
handle_cast(shutdown, State) -> {stop, normal, State};
%% generic async handler
%% @private
handle_cast(_, State) -> {noreply, State}.

%% Informative calls
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% @private
handle_info(_Message, Server) -> {noreply, Server}.

%% Server termination
%% @private
terminate(_Reason, _Server) -> ok.

%% Code change
%% @private
code_change(_OldVersion, Server, _Extra) ->
    {ok, Server}.
