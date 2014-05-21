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
  ServerPid = spawn(fun() -> loop([],[],[],0) end),
  log("Server started with PID ~p~n", [ServerPid]),
  {ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
  global:register_name(Servername,ServerPid).

%%%-------------------------------------------------------------------
%%% Server loop
%%%-------------------------------------------------------------------
loop(H,D,C,Uid) ->
  receive
    {query_messages, Client} ->
      {Number, Nachricht} = getMessage(Client,C,D),
      Client ! { message, Number,Nachricht,isTerminat(Number, D)},
      log("Nachricht ~p an ~p gesendet~n", [Nachricht, Client]),
      loop(H,D,C,Uid);

    {new_message, {Nachricht, Number}} ->
      log("Nachricht ~p bekommen : ~p~n", [Number, Nachricht]),
      % save message if id is unique
      saveMessage(H, D, Number, Nachricht),
      loop(H,D,C,Uid);

    {query_msgid, Client} ->
      log("Nachrichtennummer ~p gesendet ~p~n", [Uid + 1, Client]),
      Client ! { msgid, Uid},
      loop(H,D,C,Uid + 1)
  end.

isTerminat(Nr, [{LastNr, _LastMsg}|_Tail]) ->
  if
    Nr == LastNr ->
      1;
    true ->
      0
  end.

%%%-------------------------------------------------------------------
%%% Save a new message
%%%-------------------------------------------------------------------
saveMessage(H, D, Number, Nachricht) ->
  % push message into holdbackqueu
  werkzeug:pushSL(H, {Number, Nachricht}),
  % get length of deliverqueue
  Hlength = lengthSL(H),
  % check if D is greater then allowed if so pop last elem into Holdqueue.
  DeliverQueueLimit = getConfigOption(dlqlimit),
  if
    Hlength >= (DeliverQueueLimit div 2) ->
      rearrengeList(D,H);
    true ->
      noop
  end.

%%%-------------------------------------------------------------------
%%% Return the next message to deliver
%%%-------------------------------------------------------------------
getMessage(Client,C,D) ->
  NextMsgNr = getLastMsgSendToClient(Client,C,C),
  {MsgNr,Msg} = werkzeug:findneSL(D, NextMsgNr),
  setLastMsgSendToClient(Client,MsgNr,C,C),
  {MsgNr, Msg}.

%%%-------------------------------------------------------------------
%%% Put a message into the given queue
%%%-------------------------------------------------------------------
rearrengeList(_,[]) ->
  noop;
rearrengeList(D,[{MsgNr, Msg}]) ->
  DeliverQueueLimit = getConfigOption(dlqlimit),
  Dlength = werkzeug:lengthSL(D),
  if
    Dlength >= DeliverQueueLimit ->
      werkzeug:popSL(D);
    true ->
      noop
  end,
  werkzeug:pushSL(D, {MsgNr, Msg});
rearrengeList(D,[{MsgNr, Msg}|Tail]) ->
  {LastDNr, _} = werkzeug:findSL(D, werkzeug:maxNrSL(D)),
  if
    MsgNr == LastDNr + 1 ->
      DeliverQueueLimit = getConfigOption(dlqlimit),
      Dlength = werkzeug:lengthSL(D),
      if
        Dlength >= DeliverQueueLimit ->
          werkzeug:popSL(D);
        true ->
          noop
      end,
      werkzeug:pushSL(D, {MsgNr, Msg}),
      rearrengeList(D, Tail)
  end.

%%%-------------------------------------------------------------------
%%% Read a config option
%%%-------------------------------------------------------------------
getConfigOption(Configname) ->
  {ok, ConfigListe} = file:consult("server.cfg"),
  {ok, Configvalue} = get_config_value(Configname, ConfigListe),
  Configvalue.

%%%-------------------------------------------------------------------
%%% Logging
%%%
%%% Logs in file and on console
%%%-------------------------------------------------------------------
log(Msg) ->
  Logfilename = io_lib:format("NServer@~p.log", [self()]),
  werkzeug:logging(Logfilename, io_lib:format(Msg, [])).
log(Msg, Params) ->
  log(io_lib:format(Msg, Params)).

getLastMsgSendToClient(Client, [{Process,Nr,_}], List) ->
  if
    Process == Client ->
      Nr;
    true ->
      erlang:append(List, {Client,0,now()})
  end;
getLastMsgSendToClient(Client, [{Process,Nr,_}|Tail], List) ->
  if
    Process == Client ->
      Nr;
    true ->
      getLastMsgSendToClient(Client, Tail, List)
  end;
getLastMsgSendToClient(_, [], _) ->
  0.

setLastMsgSendToClient(Client, Nr, [{Process,OldNr,OldTs}], List) ->
  if
    Process =:= Client ->
      Ts = now(),
      Lifetime = getConfigOption(clientlifetime),
      timer:apply_after(Lifetime * 1000, client, deleteClient, [Client, Ts, List]),
      [{Process,Nr,Ts}];
    true ->
      [{Process,OldNr,OldTs}]
  end;
setLastMsgSendToClient(Client, Nr, [{Process,OldNr,OldTs}|Tail], List) ->
  if
    Process =:= Client ->
      Ts = now(),
      Lifetime = getConfigOption(clientlifetime),
      timer:apply_after(Lifetime * 1000, client, deleteClient, [Client, Ts, List]),
      [{Process,Nr,Ts}|Tail];
    true ->
      [{Process,OldNr,OldTs}|setLastMsgSendToClient(Client, Nr, Tail, List)]
  end;
setLastMsgSendToClient(_Client, _Nr, [], _List) ->
  [].

deleteClient(Client,Ts,[{Process,_Nr,STs}]) ->
  if
    Process =:= Client andalso Ts =:= STs ->
      [];
    true ->
      [{Process,_Nr,STs}]
  end;
deleteClient(Client,Ts,[{Process,_Nr,STs}|Tail]) ->
  if
    Process =:= Client andalso Ts =:= STs ->
      [Tail];
    true ->
      [{Process,_Nr,STs}|deleteClient(Client, Ts, Tail)]
  end;
deleteClient(_Client,_Ts,[]) ->
 [].