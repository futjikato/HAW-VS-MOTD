%%%-------------------------------------------------------------------
%%% @author moritzspindelhirn
%%% @doc MODT Server
%%%
%%% @end
%%% Created : 17. Mai 2014 12:40
%%%-------------------------------------------------------------------
-module(server).
-author("moritzspindelhirn").

%% API
-export([start/0]).

%% Imports
-import(werkzeug, [get_config_value/2,lengthSL/1]).
-import(erlang, [append/2]).

%%%-------------------------------------------------------------------
%%% Start the server
%%%-------------------------------------------------------------------
start() ->
  H = [],
  D = [],
  ServerPid = spawn(fun() -> loop(H,D,0) end),
  log("Server started with PID ~p\n", [ServerPid]),
  {ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
  global:register_name(Servername,ServerPid).

%%%-------------------------------------------------------------------
%%% Server loop
%%%-------------------------------------------------------------------
loop(H,D,Uid) ->
  receive
    {query_messages, Client} ->
      log("query_messages"),
      {Number, Nachricht} = getMessage(H,D),
      Client ! { message, Number,Nachricht,isTerminat(werkzeug:lengthSL(D))},
      loop(H,D,Uid);

    {new_message, {Nachricht, Number}} ->
      log("new_message"),
      % save message if id is unique
      saveMessage(H, D, Number, Nachricht, werkzeug:findSL(D, Number), werkzeug:findSL(H, Number)),
      loop(H,D,Uid);

    {query_msgid, Client} ->
      log("query_msgid"),
      Client ! { msgid, Uid},
      loop(H,D,Uid + 1)
  end.

isTerminat(0) ->
  1;
isTerminat(L) ->
  0.

%%%-------------------------------------------------------------------
%%% Save a new message
%%%-------------------------------------------------------------------
saveMessage(H, D, Number, Nachricht, {-1,nok}, {-1,nok}) ->
  DeliverQueueLimit = getConfigOption(dlqlimit),
  % push message into deliverqueue
  werkzeug:pushSL(D, {Number, Nachricht}),
  % get length of deliverqueue
  Dlength = lengthSL(D),
  % check if D is greater then allowed if so pop last elem into Holdqueue.
  if
    Dlength > DeliverQueueLimit ->
      werkzeug:pushSL(H, werkzeug:popfiSL(D))
  end;
% if message id already exists in the H or D queue
saveMessage(H, D, Number, Nachricht, {In1, St1}, {In2, St2}) ->
  noop.

%%%-------------------------------------------------------------------
%%% Return the next message to deliver
%%%-------------------------------------------------------------------
getMessage(H, D) ->
  {Number, Nachricht} = getSendingMessage(D, werkzeug:minNrSL(D)),
  rearrengeMsg(D,H,werkzeug:minNrSL(H)),
  {Number, Nachricht}.
getSendingMessage(D, -1) ->
  {-1, "Empty message"};
getSendingMessage(D, Nr) ->
  werkzeug:findSL(D, Nr).

%%%-------------------------------------------------------------------
%%% Put a message into the given queue
%%%-------------------------------------------------------------------
rearrengeMsg(D,H,-1) ->
  noop;
rearrengeMsg(D,H,Nr) ->
  MoveBlock = werkzeug:findSL(H, Nr),
  werkzeug:popSL(H),
  werkzeug:pushSL(D, MoveBlock).

%%%-------------------------------------------------------------------
%%% Read a config option
%%%-------------------------------------------------------------------
getConfigOption(Configname) ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Configvalue} = get_config_value(Configname, ConfigListe),
  Configvalue.

%%%-------------------------------------------------------------------
%%% Logging
%%%
%%% Logs in file and on console
%%%-------------------------------------------------------------------
log(Msg) ->
  Logfilename = io_lib:format("NServer@~p.log", [self()]),
  erlang:append(Msg, "\n"),
  werkzeug:logging(Logfilename, Msg).
log(Msg, Params) ->
  log(io_lib:format(Msg, Params)).