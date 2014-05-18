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
      Client ! { message, 1,"TEST",1};

    {new_message, {Nachricht, Number}} ->
      logging("NServer.log", "new_message"),
      {ok, ConfigListe} = file:consult("server.cfg"),
      {ok, DeliverQueueLimit} = get_config_value(dlqlimit, ConfigListe),
      % check if Number is unique
      if
        werkzeug:findSL(D, Number) = {-1,nok} and werkzeug:findSL(H, Number) = {-1, nok} ->
          Dlength = lengthSL(D),
          % check where to add the message to Holdqueue or Deliveryqueue
          if
            Dlength > DeliverQueueLimit -> werkzeug:pushSL(H, {Number, Nachricht});
            true -> werkzeug:pushSL(D, {Number, Nachricht})
          end
      end;

    {query_msgid, Client} ->
      logging("NServer.log", "query_msgid"),
      Client ! { msgid, Uid},
      Uid = Uid + 1
  end,
  loop(H,D,Uid).

%%%-------------------------------------------------------------------
%%% Logging
%%%
%%% Logs in file and on console
%%%-------------------------------------------------------------------
log(Msg) ->
  Logfilename = io_lib:format("NServer@~p.log", [self()]),
  werkzeug:logging(Logfilename, Msg).
log(Msg, Params) ->
  log(io_lib:format(Msg, Params)).