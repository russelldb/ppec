%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@pango.lan>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% PayPal Express Checkout gen_server
%%% @end
%%% Created : 15 Apr 2010 by Russell Brown <russell@pango.lan>
%%%-------------------------------------------------------------------
-module(ppec).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).
-export([set/6, get/4, do/6]).
-define(SERVER, ?MODULE). 
-define(VSN,  "62.0").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	ensure_started(inets),
	ensure_started(ssl),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Calls paypal's SetExpressCheckout API
%%
%% @spec set() -> 
%% @end
%%---------------------------------------------------------------------
set(Uname, Pword, Sig, Amt, ReturnUrl, CancelUrl) ->
	Method = "SetExpressCheckout",
	Auth = [{'USER', Uname}, {'PWD', Pword}, {'SIGNATURE', Sig}],
	Params = [{'AMT', Amt}, {'RETURNURL', ReturnUrl}, {'CANCELURL', CancelUrl}, {'PAYMENTACTION', "Sale"}],
	gen_server:call(?SERVER, {invoke, Method, Auth, Params}).

%%--------------------------------------------------------------------
%% @doc
%% Calls paypal's GetExpressCheckoutDetails API
%%
%% @spec get() -> 
%% @end
%%---------------------------------------------------------------------
get(Uname, Pword, Sig, Token) ->
	Method = "GetExpressCheckoutDetails",
	Auth = [{'USER', Uname}, {'PWD', Pword}, {'SIGNATURE', Sig}],
	Params = [{'TOKEN', Token}],
	gen_server:call(?SERVER, {invoke, Method, Auth, Params}).

%%--------------------------------------------------------------------
%% @doc
%% Calls paypal's DoExpressCheckout API
%%
%% @spec do() -> 
%% @end
%%---------------------------------------------------------------------
do(Uname, Pword, Sig, Token, PayerId, Amt) ->
	Method = "DoExpressCheckoutPayment",
	Auth = [{'USER', Uname}, {'PWD', Pword}, {'SIGNATURE', Sig}],
	Params = [{'TOKEN', Token}, {'PAYMENTACTION', "Sale"}, {'PAYERID', PayerId}, {'AMT', Amt}],
	gen_server:call(?SERVER, {invoke, Method, Auth, Params}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({invoke, Method, Auth, Params}, _From, State) ->
	Url = "https://api-3t.sandbox.paypal.com/nvp",
	Body = make_post_body(Method, Auth, Params),
	{ok, {{_, 200, _}, _, Result}} = httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Body}, [], [{sync, true}]),
	Response = util:parse_result(Result),
	Reply = case proplists:get_value("ACK", Response, "No ACK") of
				"Success"++_ ->
					{ok, Response};
				_ ->
					{error, Response}
			end,
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_post_body(Method, Auth, Params) ->
	tl(lists:flatten([util:params(Auth), util:params([{'VERSION', ?VSN}, {'METHOD', Method}]), util:params(Params)])).


ensure_started(App) ->
    case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
    end.
