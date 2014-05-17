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

%%%-------------------------------------------------------------------
%%% API - get message id
%%%-------------------------------------------------------------------
getMsgIdFromServer() ->
  Servername = getServerName(),
  Servername ! { query_msgid, self()},
  receive { msgid, Number} ->
    io:format("Received: ~s", [Number])
  end.

%%%-------------------------------------------------------------------
%%% API - get messages
%%%-------------------------------------------------------------------
getMessage() ->
  Servername = getServerName(),
  Servername ! { query_messages, self()},
  receive
    { message, Number,Nachricht,Terminated} ->
      io:format("Received: ~s ( ID: ~B )", [Nachricht, Number]),
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