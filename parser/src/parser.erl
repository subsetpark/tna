-module(parser).
%%====================================================================
%%
%% @author Zach Smith <zd@zdsmith.com>
%% @copyright CC Attribution - 2013
%%
%% @doc This module implements a parser for the New Abbreviations. The purpose
%% is to transform English text into a structured representation of a series of
%% <em>glyphs</em>, which are the abbreviations themselves. Its intention is,
%% insofar as the abbreviations are regular and systematic, to serve as a
%% "reference implementation"; not to describe how they are rendered, of
%% course, but to provide a reference for how any text should be abbreviated.
%%
%%====================================================================
-behaviour(gen_server).

% interface calls
-export([start/0, stop/0, parse/1]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.
%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start() ->
    io:format("Starting~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Stopping server asynchronously
stop() ->
    io:format("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).

parse(S) -> gen_server:call(?MODULE, {parse, S}).
%%====================================================================
%% gen_server callbacks
%%====================================================================
init(_Args) ->
    io:format("Initializing~n"),
    process_flag(trap_exit, true),
    {ok, TermList} = file:consult(code:priv_dir(parser) ++ "/tna.dict"),
    TransformTerm={suffix_transform, _} = lists:keyfind(suffix_transform, 1, TermList),
    TransformPairs = make_pairs(TransformTerm, []),
    TransformTrie = trie:new(TransformPairs),
    AllPairs = lists:foldl(fun make_pairs/2, [], TermList),
    Trie = trie:new(AllPairs),
    {ok, [TransformTrie, Trie]}.

make_pattern(suffix_transform, Name) -> [$*|Name];
make_pattern(suffix, Name) -> [$*|Name];
make_pattern(prefix, Name) -> Name ++ "*";
make_pattern(_, Name) -> Name.

make_pairs(Term, Pairs) ->
    {Type, Glyphs} = Term,
    Prec = case Type of
               suffix -> 0;
               word -> 1;
               _ -> 2
           end,
    NewPairs = [{make_pattern(Type, atom_to_list(Name)), {Name, Type, Prec}}|| Name <- Glyphs],
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
handle_call({parse, S}, _, Tries) ->
    Tokens = string:tokens(string:to_lower(S), " ,."),
    Reply = [parse_token(Token, Tries) || Token <- Tokens],
    {reply, Reply, Tries}.

%% @doc The main entrypoint for parsing. Since every word can be assumed to be
%% abbreviated independently of the words around it, the parsing logic can be
%% seen as applying parse_token/2 to every token in the stream.
parse_token(Token, Tries) ->
    Token1 = lists:foldl(fun (Trie, T) -> parse_token_parts(T, [], Trie) end,
                         [{0, token, Token}],
                         Tries),
    {Token, Token1}.
%% @doc Match into the trie on a token. If the match is a pattern with
%% remainder, recurse until all remainders have been matched or marked as seen.
parse_token_parts([{_, token, Token}=FirstPart|Rest], Acc, Trie) ->
    {Rest2, Acc2} = case catch trie:find_match(Token, Trie) of
                        {ok, Key, {Glyph, Type, _}} ->
                            case trie:is_pattern(Key) of
                                true ->
                                    Segments = trie:pattern_parse(Key, Token, expanded),
                                    SegmentProcessor = fun (Segment, {Unparsed, Parsed}) ->
                                                               case Segment of
                                                                   {exact, Match} ->
                                                                       Index = string:str(Token, Match),
                                                                       Parsed2 = [{Index, Type, Glyph}|Parsed],
                                                                       {Unparsed, Parsed2};
                                                                   Remainder ->
                                                                       Index = string:str(Token, Remainder),
                                                                       Unparsed2 = [{Index, token, Remainder}|Unparsed],
                                                                       {Unparsed2, Parsed}
                                                               end
                                                       end,
                                    {Unparsed, Parsed} = lists:foldl(SegmentProcessor, {[],[]}, Segments),
                                    {Unparsed ++ Rest, Parsed ++ Acc};
                                _ ->
                                    Index = string:str(Token, Key),
                                    NewNode = {Index, Type, Glyph},
                                    {Rest, [NewNode|Acc]}
                            end;
                        _ -> {Rest, [FirstPart|Acc]}
                    end,
    parse_token_parts(Rest2, Acc2, Trie);
parse_token_parts([FirstPart|Rest], Acc, Trie) ->
    % If we encounter a non-token, it has already been parsed by a previous
    % pass.
    parse_token_parts(Rest, [FirstPart|Acc], Trie);
parse_token_parts([], Acc, _) -> lists:sort(Acc).
%% Asynchronous, possible return values
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};
%% generic async handler
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

%% Informative calls
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info(_Message, _Server) ->
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

%% Server termination
terminate(_Reason, _Server) ->
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.
