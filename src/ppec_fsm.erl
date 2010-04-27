%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@pango.lan>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% fsn for the Express Checkout Flow
%%% @end
%%% Created : 24 Apr 2010 by Russell Brown <russell@pango.lan>
%%% TODO: Write done, failed and cancelled
%%%-------------------------------------------------------------------
-module(ppec_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0, set/4, get/1, do/2]).

%% gen_fsm callbacks
-export([init/1, ready/2, ready/3, setted/2, setted/3, getted/2, getted/3, handle_event/3,
		 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {set_response, get_response, do_response}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Start of the process by calling SetExpressCheckout
%%
%% @spec set(pid(), money(), url(), url()) -> {ok, paypalresponse()} | {error, paypalresponse()} | {error, reason}
%% @end
%%--------------------------------------------------------------------
set(Pid, Amt, ReturnUrl, CancelUrl) ->
	gen_fsm:sync_send_event(Pid, {set, Amt, ReturnUrl, CancelUrl}).


%%--------------------------------------------------------------------
%% @doc
%% Move along by calling get
%%
%% @spec get(pid()) -> {ok, paypalresponse()} | {error, paypalresponse()} | {error, reason}
%% @end
%%--------------------------------------------------------------------
get(Pid) ->
	gen_fsm:sync_send_event(Pid, {get}).

%%--------------------------------------------------------------------
%% @doc
%% Finalise by calling do
%%
%% @spec do(pid(), PayerId, Amt) -> {ok, paypalresponse()} | {error, paypalresponse()} | {error, reason}
%% @end
%%--------------------------------------------------------------------
do(Pid, Amt) ->
	gen_fsm:sync_send_event(Pid, {do, Amt}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	{ok, ready, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
ready(_Event, State) ->
	{next_state, ready, State}.

setted(_Event, State) ->
	{next_state, setted, State}.

getted(_Event, State) ->
	{next_state, getted, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
ready({set, Amt, ReturnUrl, CancelUrl}, _From, State) ->
	case ppec:set(Amt, ReturnUrl, CancelUrl) of
		{ok, Resp} ->
			NewState = setted;
		{error, Resp} ->
			NewState = failed
	end,
	{reply, Resp, NewState, State#state{set_response=Resp}};
ready(_Event, _From,  State) ->
	Reply = {error, invalid_message},
	{reply, Reply, ready, State}.

setted({get}, _From, State) ->
	Token = proplists:get_value(token, State#state.set_response),
	case ppec:get(Token) of
		{ok, Resp} ->
			%% Check state of purchase
			NewState = get_checkout_status(proplists:get_value(checkoutstatus, Resp));
		{error, Resp} ->
			NewState = setted
	end,
	{reply, Resp, NewState, State#state{get_response=Resp}};
setted(_Event, _From, State) ->
	Reply = {error, invalid_message},
	{reply, Reply, setted, State}.

getted({do, Amt}, _From, State) ->
	Token = proplists:get_value(token, State#state.get_response),
	PayerId = proplists:get_value(payerId, State#state.get_response),
	case ppec:do(Token, PayerId, Amt) of
		{ok, Resp} ->
			NewStatus = get_paid_status(proplists:get_value(paymentstatus, Resp));
		{error, Resp} ->
			NewStatus = getted
	end,
	{reply, Resp, NewStatus, State#state{do_response=Resp}};
getted(_Event, _From, State) ->
	Reply = {error, invalid_message},
	{reply, Reply, getted, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
	Reply = ok,
	{reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_checkout_status("PaymentActionNotInitiated") ->
	setted;
get_checkout_status("PaymentActionFailed") ->
	failed;
get_checkout_status("PaymentActionInProgress") ->
	setted;
get_checkout_status("PaymentCompleted") ->
	getted.


get_paid_status("Completed") ->
	done;
get_paid_status(_) ->
	getted.
