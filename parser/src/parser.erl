-module(parser).
%%====================================================================
%%
%% @author Zach Smith
%% @copyright CC Attribution - 2013
%%
%% Parser for the New Abbreviations.
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
    AllPairs = lists:foldl(fun make_pairs/2, [], TermList),
    Trie = trie:new(AllPairs),
    {ok, Trie}.

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
handle_call({parse, S}, _, Trie) ->
    Tokens = string:tokens(string:to_lower(S), " ,."),
    Reply = [parse_token(Token, Trie) || Token <- Tokens],
    {reply, Reply, Trie}.

parse_token(Token, Trie) -> parse_token_parts(Token, [{0, token, Token}], [], Trie).
parse_token_parts(_, [{N, token, Token}=FirstPart|Rest], Acc, Trie) ->
    {Rest2, Acc2} = case catch trie:find_match(Token, Trie) of
        {ok, Key, {Glyph, Type, _}} ->
                      NewNode = {N, Type, Glyph},
                      case trie:is_pattern(Key) of
                          true ->
                              {[Remainder], Suffix} = trie:pattern_parse(Key, Token, with_suffix),
                              % This is a hack.
                              {SortGlyph, SortRem} = case Suffix of
                                                        % We know the glyph is a prefix; sort it first.
                                                        [] -> {N, N+1};
                                                        _ -> {N+1, N}
                                                    end,
                              {[{SortRem, token, Remainder}|Rest],
                               [setelement(1, NewNode, SortGlyph)|Acc]};
                          _ ->
                              {Rest, [NewNode|Acc]}
                      end;
        _ -> {Rest, [FirstPart|Acc]}
    end,
    parse_token_parts(Token, Rest2, Acc2, Trie);
parse_token_parts(_, [], Acc, _) -> lists:sort(Acc).
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
