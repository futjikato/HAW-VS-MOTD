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
-export([getMsgIdFromServer/0,getMessage/0]).

%% Imports
-import(werkzeug, [get_config_value/2]).

%%%-------------------------------------------------------------------
%%% API - get message id
%%%-------------------------------------------------------------------
getMsgIdFromServer() ->
  Servername = getServerName(),
  io:format("Sending request to server ~p\n", [Servername]),
  Server = global:whereis_name(Servername),
  Server ! { query_msgid, self()},
  receive { msgid, Number} ->
    io:format("Received: ~p\n", [Number])
  end.

%%%-------------------------------------------------------------------
%%% API - get messages
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
%%% API - send new message
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Get Server name from config
%%%-------------------------------------------------------------------
getServerName() ->
  {ok, ConfigListe} = file:consult("server.cfg"),
  {ok, Servername} = get_config_value(servername, ConfigListe),
  Servername.

%%%-------------------------------------------------------------------
%%% Logging
%%%-------------------------------------------------------------------
log(Msg) ->
  Logfilename = io_lib:format("Client@~p.log", [self()]),
  werkzeug:logging(Logfilename, Msg).
log(Msg, Params) ->
  log(io_lib:format(Msg, Params)).