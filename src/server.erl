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
      NewC = setLastMsgSendToClient(Client, Number, C),
      log("Nachricht Nr ~p an ~p gesendet~n", [Number, Client]),
      loop(H,D,NewC,Uid);

    {new_message, {Nachricht, Number}} ->
      log("Nachricht ~p bekommen : ~p~n", [Number, Nachricht]),
      % save message if id is unique
      {NewH, NewD} = saveMessage(H, D, Number, Nachricht),
      loop(NewH,NewD,C,Uid);

    {query_msgid, Client} ->
      log("Nachrichtennummer ~p gesendet ~p~n", [Uid + 1, Client]),
      Client ! { msgid, Uid},
      loop(H,D,C,Uid + 1);

    {remove_client, Client} ->
      log("Going to remove client ~p from list ~p~n", [Client, C]),
      NewC = lists:keydelete(Client, 1, C),
      loop(H,D,NewC,Uid)
  end.

isTerminat(_Nr, []) ->
  true;
isTerminat(Nr, D) ->
  MaxNr = werkzeug:maxNrSL(D),
  log("Max delivery queue ~p~n", [MaxNr]),
  if
    Nr =:= MaxNr ->
      true;
    true ->
      false
  end.

%%%-------------------------------------------------------------------
%%% Save a new message
%%%-------------------------------------------------------------------
saveMessage(H, D, Number, Nachricht) ->
  % push message into holdbackqueu
  NewH = werkzeug:pushSL(H, {Number, Nachricht}),
  % get length of deliverqueue
  Hlength = erlang:length(NewH),
  % check if D is greater then allowed if so pop last elem into Holdqueue.
  DeliverQueueLimit = getConfigOption(dlqlimit),
  log("~p >= ~p~n", [Hlength, DeliverQueueLimit div 2]),
  log("Delivery queue: ~p~n", [erlang:length(D)]),
  if
    Hlength >= (DeliverQueueLimit div 2) ->
      log("Nachricht umschichten.~n"),
      {NewNewH, NewD} = rearrengeList(D,NewH),
      {NewNewH, NewD};
    true ->
      {NewH, D}
  end.

%%%-------------------------------------------------------------------
%%% Return the next message to deliver
%%%-------------------------------------------------------------------
getMessage(Client,C,D) ->
  LastMsgNr = getLastMsgSendToClient(Client,C,D),
  NextMsgNr = LastMsgNr + 1,
  SaveMsgNr = lists:max([NextMsgNr, werkzeug:minNrSL(D)]),
  log("Nachricht die ausgeliefert werden soll ~p~n", [SaveMsgNr]),
  {MsgNr,Msg} = werkzeug:findneSL(D, SaveMsgNr),
  {MsgNr, Msg}.

%%%-------------------------------------------------------------------
%%% Put a message into the given queue
%%%-------------------------------------------------------------------
rearrengeList(D,[]) ->
  {D, []};
rearrengeList(D,[{MsgNr, Msg}]) ->
  DeliverQueueLimit = getConfigOption(dlqlimit),
  Dlength = werkzeug:lengthSL(D),
  if
    Dlength >= DeliverQueueLimit ->
      werkzeug:popSL(D);
    true ->
      noop
  end,
  NewD = werkzeug:pushSL(D, {MsgNr, Msg}),
  {NewD, []};
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
      NewD = werkzeug:pushSL(D, {MsgNr, Msg}),
      {SuperNewD, NewTail} = rearrengeList(NewD, Tail),
      {SuperNewD, NewTail};
    true ->
      {D,[{MsgNr, Msg}|Tail]}
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

%%%-------------------------------------------------------------------
%%% Receive the ID of the last send message
%%%-------------------------------------------------------------------
getLastMsgSendToClient(Client, [{Process,Nr,_}], D) ->
  if
    Process =:= Client ->
      log("Found client ~p Return ~p~n", [Client, Nr]),
      Nr;
    true ->
      werkzeug:minNrSL(D)
  end;
getLastMsgSendToClient(Client, [{Process,Nr,_}|Tail], D) ->
  if
    Process =:= Client ->
      log("Found2 client ~p Return ~p~n", [Client, Nr]),
      Nr;
    true ->
      getLastMsgSendToClient(Client, Tail, D)
  end;
getLastMsgSendToClient(_, [], D) ->
  werkzeug:minNrSL(D).

%%%-------------------------------------------------------------------
%%% Set the ID of the last send message for a given client
%%%-------------------------------------------------------------------
setLastMsgSendToClient(Client, Nr, [{Process,OldNr,OldTimer}]) ->
  if
    Process =:= Client ->
      Lifetime = getConfigOption(clientlifetime),
      NewTimer = werkzeug:reset_timer(OldTimer, Lifetime, {remove_client, Client}),
      [{Process,Nr,NewTimer}];
    true ->
      [{Process,OldNr,OldTimer}|setLastMsgSendToClient(Client, Nr, [])]
  end;
setLastMsgSendToClient(Client, Nr, [{Process,OldNr,OldTimer}|Tail]) ->
  if
    Process =:= Client ->
      Lifetime = getConfigOption(clientlifetime),
      NewTimer = werkzeug:reset_timer(OldTimer, Lifetime, {remove_client, Client}),
      [{Process,Nr,NewTimer}|Tail];
    true ->
      [{Process,OldNr,OldTimer}|setLastMsgSendToClient(Client, Nr, Tail)]
  end;
setLastMsgSendToClient(Client, Nr, []) ->
  Lifetime = getConfigOption(clientlifetime),
  {ok, Timer} = timer:send_after(Lifetime * 1000, {remove_client, Client}),
  [{Client,Nr,Timer}].

%%%-------------------------------------------------------------------
%%% WHY DOES THIS FAIL !?!?!?!?!
%%% Solution: use lists:deletekey
%%%
%%% =ERROR REPORT==== 27-May-2014::11:05:38 ===
%%% Error in process <0.46.0> on node 'server@ws-67-9' with exit value: {function_clause,[{server,deleteClient,[<9542.20624.0>,[[{<9542.20620.0>,59,{1401181538895159,#Ref<0.0.0.9354>}},{<9542.20624.0>,59,{1401181538894549,#Ref<0.0.0.9333>}},{<9542.20623.0>,59,{1401181538902886,#Ref<0.0.0.9480>}}]]],[{file,"server.erl"},{line,194}]},{server,deleteClient,2,[{file,"server.erl"},{line,206}]},{server,deleteClient,2,[{file,"server.erl"},{line,206}]},{server,loop,4,[{file...
%%%-------------------------------------------------------------------
deleteClient(Client,[{Process,Nr,Timer}]) ->
  if
    Process =:= Client ->
      [];
    true ->
      [{Process,Nr,Timer}]
  end;
deleteClient(Client,[{Process,Nr,Timer}|Tail]) ->
  if
    Process =:= Client ->
      [Tail];
    true ->
      [{Process,Nr,Timer}|deleteClient(Client, Tail)]
  end;
deleteClient(_Client,[]) ->
 [].