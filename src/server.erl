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
-import(werkzeug, [logging/2,get_config_value/2,lengthSL/1]).

%%%-------------------------------------------------------------------
%%% Start the server
%%%-------------------------------------------------------------------
start() ->
  H = [],
  D = [],
  ServerPid = spawn(fun() -> loop(H,D) end),
  logging("NServer.log", io_lib:format("Server started with PID ~p\n", [ServerPid])),
  {ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
  register(Servername,ServerPid),
  ServerPid.

%%%-------------------------------------------------------------------
%%% Server loop
%%%-------------------------------------------------------------------
loop(H,D) ->
  receive
    {query_messages, Client} ->
      logging("NServer.log", "query_messages"),
      Client ! { message, 1,"TEST",1};
    {new_message, {Nachricht, Number}} ->
      logging("NServer.log", "new_message"),
      {ok, ConfigListe} = file:consult("server.cfg"),
      {ok, DeliverQueueLimit} = get_config_value(dlqlimit, ConfigListe),
      if
        lengthSL(D) > DeliverQueueLimit -> append(H, Nachricht);
        true -> append(D, Nachricht)
      end;
    {query_msgid, Client} ->
      logging("NServer.log", "query_msgid"),
      Client ! { msgid, lengthSL(D)+lengthSL(H)+1}
  end.