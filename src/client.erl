%%%-------------------------------------------------------------------
%%% @author moritzspindelhirn
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mai 2014 13:19
%%%-------------------------------------------------------------------
-module(client).
-author("moritzspindelhirn").

%% API
-export([startEditorial/1]).

%% Imports
-import(werkzeug, [get_config_value/2]).
-import(timer, [apply_after/4]).

%% Defines
-define(TEAMNR, 99).
-define(PRAKNR, 3).

%%%-------------------------------------------------------------------
%%% Start editor client ( sending text messages )
%%%-------------------------------------------------------------------
startEditorial(Name) ->
  Sendintervall = getSendinterval(),
  SendintervallMs = Sendintervall * 1000,
  sendloop(Name, SendintervallMs, 0, 0).

%%%-------------------------------------------------------------------
%%% Loop for sending messages
%%%-------------------------------------------------------------------
sendloop(Name, Timeout, BatchNum, Count) ->
  % send after time
  apply_after(Timeout, client, prepAndSendMsg, [Name, Count]),
  BatchNum = BatchNum + 1,
  Count = Count + 1,
  % change timeout time after 5 messages
  if
    BatchNum > 5 ->
      Half = round(Timeout / 2),
      Diff = random:uniform(Half),
      % change must be at least 1 sek
      if
        Diff < 1000 -> Diff = 1000
      end,
      % random add or sub
      UpOrDown = random:uniform(),
      if
        UpOrDown >= 0.5 -> Timeout = Timeout + Diff;
        true -> Timeout = Timeout - Diff
      end,
      % check new timeout is at least 2000
      if
        Timeout < 2000 -> Timeout = 2000
      end,
      % reset batch counter
      BatchNum = 0
  end,
  sendloop(Name, Timeout, BatchNum, Count).

%%%-------------------------------------------------------------------
%%% Build the real message string and calls sendMessageWithId
%%%-------------------------------------------------------------------
prepAndSendMsg(Name, Nr) ->
  {ok, Hostname} = inet:gethostname(),
  SendParams = [Name, Hostname, self(), ?PRAKNR, ?TEAMNR, Nr, date()],
  % Misterious: C 769 (22)
  % 0-client@lab18-<0.1313.0>-C-1-03: 22te_Nachricht. Sendezeit: 16.05 18:01:30,769|(22)
  FormatStr = "~s@~s-~p-C-~d-~d: ~dte_Nachricht. Sendezeit: ~p",
  Msg = io_lib:format(FormatStr, SendParams),
  sendMessageWithId(Msg).

%%%-------------------------------------------------------------------
%%% Send a message
%%% First get a new message ID from server
%%%-------------------------------------------------------------------
sendMessageWithId(Msg) ->
  Servername = getServerName(),
  Server = global:whereis_name(Servername),
  Server ! { query_msgid, self()},
  receive { msgid, Number} ->
    Server ! {new_message, {Msg, Number}}
  end.

%%%-------------------------------------------------------------------
%%% Get messages
%%%-------------------------------------------------------------------
getMessage() ->
  Servername = getServerName(),
  Servername ! { query_messages, self()},
  receive
    { message, Number,Nachricht,Terminated} ->
      io:format("Received: ~s ( ID: ~p )", [Nachricht, Number]),
      if
        not Terminated ->
          getMessage()
      end
  end.

%%%-------------------------------------------------------------------
%%% Get Server name from config
%%%-------------------------------------------------------------------
getServerName() ->
  {ok, ConfigListe} = file:consult("server.cfg"),
  {ok, Servername} = get_config_value(servername, ConfigListe),
  Servername.

%%%-------------------------------------------------------------------
%%% Get sendinterval from config
%%%-------------------------------------------------------------------
getSendinterval() ->
  {ok, ConfigListe} = file:consult("server.cfg"),
  {ok, Sendintervall} = get_config_value(sendeintervall, ConfigListe),
  Sendintervall.

%%%-------------------------------------------------------------------
%%% Logging
%%%-------------------------------------------------------------------
log(Msg) ->
  Logfilename = io_lib:format("Client@~p.log", [self()]),
  werkzeug:logging(Logfilename, Msg).
log(Msg, Params) ->
  log(io_lib:format(Msg, Params)).