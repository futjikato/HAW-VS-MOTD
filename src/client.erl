%%%-------------------------------------------------------------------
%%% @author moritzspindelhirn
%%% @doc MOTD Client
%%%
%%% @end
%%% Created : 17. Mai 2014 13:19
%%%-------------------------------------------------------------------
-module(client).
-author("moritzspindelhirn").

%% API
-export([start/0]).

%% Imports
-import(werkzeug, [get_config_value/2]).
-import(timer, [apply_after/4]).

%% Defines
-define(TEAMNR, 99).
-define(PRAKNR, 3).

%%%-------------------------------------------------------------------
%%% Start client
%%% first set lifetime timer
%%% then start loop
%%%-------------------------------------------------------------------
start() ->
  Clientcount = getClientcount(),
  start(Clientcount).
start(0) ->
  noop;
start(Clientcount) ->
  spawn(fun() -> startSingle(io_lib:format("client-~p", [Clientcount])) end),
  start(Clientcount - 1).

startSingle(Name) ->
  Sendintervall = getSendinterval(),
  SendintervallMs = Sendintervall * 1000,
  Lifetime = getLifetime(),
  LifetimeMs = Lifetime * 1000,
  timer:exit_after(LifetimeMs, "Time to say goodbye."),
  sendloop(Name, SendintervallMs, 0, 0, []).

%%%-------------------------------------------------------------------
%%% Loop for sending messages
%%%-------------------------------------------------------------------
sendloop(Name, Timeout, BatchNum, Count, SendMessageSL) ->
  % change timeout time after 5 messages
  if
    BatchNum > 5 ->
      Half = round(Timeout / 2),
      Diff = random:uniform(Half),
      % change must be at least 1 sek
      if
        Diff < 1000 -> NewDiff = 1000;
        true -> NewDiff = Diff
      end,
      % random add or sub
      UpOrDown = random:uniform(),
      if
        UpOrDown >= 0.5 -> NewTimeout = Timeout + NewDiff;
        true -> NewTimeout = Timeout - NewDiff
      end,
      % check new timeout is at least 2000
      if
        Timeout < 2000 -> CheckedTimeout = 2000;
        true -> CheckedTimeout = NewTimeout
      end,
      log(Name, "New intervall is ~p~n", [Timeout]),
      % get unique message without send message ( Requirement 11. )
      getMessageId(Name, fun(Number) ->
        log(Name, "Requested but forgot to send message with ID ~p~n", [Number])
      end),
      % now read all messages
      getMessages(0, Name, SendMessageSL),
      % send after time
      timer:sleep(CheckedTimeout),
      prepAndSendMsg(Name, CheckedTimeout, 0, Count + 1, SendMessageSL);
    true ->
      timer:sleep(Timeout),
      prepAndSendMsg(Name, Timeout, BatchNum + 1, Count + 1, SendMessageSL)
  end.

%%%-------------------------------------------------------------------
%%% Build the real message string and calls sendMessageWithId
%%%-------------------------------------------------------------------
prepAndSendMsg(Name, Timeout, BatchNr, Count, SendMessageSL) ->
  % Misterious: C 769 (22)
  % 0-client@lab18-<0.1313.0>-C-1-03: 22te_Nachricht. Sendezeit: 16.05 18:01:30,769|(22)
  Msg = io_lib:format("~s@~s-~p-C-~p-~p: ~pte_Nachricht. Sendezeit: ~p", [Name, "Hostname", self(), ?PRAKNR, ?TEAMNR, Count, date()]),
  sendMessageWithId(Name, Msg, SendMessageSL, Timeout, BatchNr, Count),
  true.

%%%-------------------------------------------------------------------
%%% Send a message
%%% First get a new message ID from server
%%%-------------------------------------------------------------------
sendMessageWithId(Name, Msg, SendMessageSL, Timeout, BatchNr, Count) ->
  Servername = getServerName(),
  Server = global:whereis_name(Servername),
  getMessageId(Name, fun(Number) ->
    Server ! {new_message, {Msg, Number}},
      werkzeug:pushSL(SendMessageSL, {Msg, Number}),
      log(Name, "Sending message ~s ( ID: ~p )~n", [Msg, Number]),
      sendloop(Name, Timeout, BatchNr, Count + 1, SendMessageSL)
    end).

%%%-------------------------------------------------------------------
%%% Request a new message ID from teh server
%%%-------------------------------------------------------------------
getMessageId(Name, Callback) ->
  Servername = getServerName(),
  Server = global:whereis_name(Servername),
  % request new message id
  Server ! { query_msgid, self()},
  % receive new message and callback with it
  receive { msgid, Number} ->
    log(Name, "Received message number ~p~n", [Number]),
    Callback(Number)
  end.

%%%-------------------------------------------------------------------
%%% Get messages
%%%-------------------------------------------------------------------
getMessages(0, Name, SendMessageSL) ->
  Servername = getServerName(),
  Server = global:whereis_name(Servername),
  Server ! { query_messages, self()},
  receive
    { message, Number,Nachricht,Terminated} ->
      % Requirement 12. -> append ***** on messages send by this client
      printMessage(Name, Nachricht, Number, werkzeug:findSL(SendMessageSL, Number)),
      getMessages(Terminated, Name, SendMessageSL)
  end;
getMessages(1, Name, _) ->
  log(Name, "All messages received.~n"),
  noop.

printMessage(Name, Nachricht, Number, {-1, nok}) ->
  log(Name, "Received: ~s ( ID: ~p )~n", [Nachricht, Number]);
printMessage(Name, Nachricht, Number, {_Index, ok}) ->
  erlang:append(Nachricht, "*******"),
  log(Name, "Received: ~s ( ID: ~p )~n", [Nachricht, Number]).

%%%-------------------------------------------------------------------
%%% Get Server name from config
%%%-------------------------------------------------------------------
getServerName() ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Servername} = get_config_value(servername, ConfigListe),
  Servername.

%%%-------------------------------------------------------------------
%%% Get sendinterval from config
%%%-------------------------------------------------------------------
getSendinterval() ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Sendintervall} = get_config_value(sendeintervall, ConfigListe),
  Sendintervall.

%%%-------------------------------------------------------------------
%%% Get livetime from config
%%%-------------------------------------------------------------------
getLifetime() ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Lifetime} = get_config_value(lifetime, ConfigListe),
  Lifetime.

%%%-------------------------------------------------------------------
%%% Get number of clients from config
%%%-------------------------------------------------------------------
getClientcount() ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Clientcount} = get_config_value(clients, ConfigListe),
  Clientcount.

%%%-------------------------------------------------------------------
%%% Logging
%%%-------------------------------------------------------------------
log(Name, Msg) ->
  Logfilename = io_lib:format("~s@~p.log", [Name, self()]),
  werkzeug:logging(Logfilename, Msg).
log(Name, Msg, Params) ->
  log(Name, io_lib:format(Msg, Params)).