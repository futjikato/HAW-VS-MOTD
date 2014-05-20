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
  startSingle(io_lib:format("client-~p", [Clientcount])),
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
        UpOrDown >= 0.5 -> NewTimeout = Timeout + Diff;
        true -> NewTimeout = Timeout - Diff
      end,
      % check new timeout is at least 2000
      if
        Timeout < 2000 -> CheckedTimeout = 2000;
        true -> CheckedTimeout = NewTimeout
      end,
      log(Name, "New intervall is ~p", [Timeout]),
      % get unique message without send message ( Requirement 11. )
      getMessageId(Name, fun(Number) ->
        log(Name, "Requested but forgot to send message with ID ~p", [Number])
      end),
      % now read all messages
      getMessages(0, Name, SendMessageSL),
      % send after time
      apply_after(Timeout, client, prepAndSendMsg, [Name, Timeout, 0, Count, SendMessageSL]);
    true ->
      apply_after(Timeout, client, prepAndSendMsg, [Name, Timeout, BatchNum + 1, Count, SendMessageSL])
  end.

%%%-------------------------------------------------------------------
%%% Build the real message string and calls sendMessageWithId
%%%-------------------------------------------------------------------
prepAndSendMsg(Name, Timeout, BatchNr, Count, SendMessageSL) ->
  {ok, Hostname} = inet:gethostname(),
  SendParams = [Name, Hostname, self(), ?PRAKNR, ?TEAMNR, Count, date()],
  % Misterious: C 769 (22)
  % 0-client@lab18-<0.1313.0>-C-1-03: 22te_Nachricht. Sendezeit: 16.05 18:01:30,769|(22)
  FormatStr = "~s@~s-~p-C-~d-~d: ~dte_Nachricht. Sendezeit: ~p",
  Msg = io_lib:format(FormatStr, SendParams),
  io:format(Msg),
  sendMessageWithId(Name, Msg, SendMessageSL, Timeout, BatchNr, Count).

%%%-------------------------------------------------------------------
%%% Send a message
%%% First get a new message ID from server
%%%-------------------------------------------------------------------
sendMessageWithId(Name, Msg, SendMessageSL, Timeout, BatchNr, Count) ->
  Servername = getServerName(),
  Server = global:whereis_name(Servername),
  getMessageId(Name, fun(Number) ->
      Server ! {new_message, {Msg, Number}},
      getMessageId(Name, fun(Number) ->
        Server ! {new_message, {Msg, Number}},
        werkzeug:pushSL(SendMessageSL, {Msg, Number}),
        log(Name, "Sending message ~s ( ID: ~p )", [Msg, Number]),
        sendloop(Name, Timeout, BatchNr, Count + 1, SendMessageSL)
      end)
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
    log(Name, "Received message number ~p", [Number]),
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
getMessages(1, Name, SendMessageSL) ->
  log(Name, "All messages received."),
  noop.

printMessage(Name, Nachricht, Number, {-1, nok}) ->
  log(Name, "Received: ~s ( ID: ~p )", [Nachricht, Number]);
printMessage(Name, Nachricht, Number, {Index, ok}) ->
  erlang:append(Nachricht, "*******"),
  log(Name, "Received: ~s ( ID: ~p )", [Nachricht, Number]).

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
  erlang:append(Msg, "\n"),
  werkzeug:logging(Logfilename, Msg).
log(Name, Msg, Params) ->
  log(Name, io_lib:format(Msg, Params)).