%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(deployment). 
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("deployment.hrl").



%% API

-export([
	
	 is_repo_updated/0,
	 update_repo/0,
	 clone_repo/0
	]).

-export([
	 get_maps/0,
	 get_map/1,
	 get_deployments/0,
	 get_info/2
	 
	]).

%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		
		main_dir,
	        repo_dir,
		spec_maps,
	        repo_git
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%********************* Appl *****************************************

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_info(Key:: atom(),DeploymentId :: string()) -> 
	  {ok,Value:: term()} | {error, Reason :: term()}.
get_info(Key,DeploymentId) ->
    gen_server:call(?SERVER,{get_info,Key,DeploymentId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_deployments() -> 
	  {ok,DeploymentNamesList :: term()} | {error, Reason :: term()}.
get_deployments() ->
    gen_server:call(?SERVER,{get_deployments},infinity).
%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_maps() -> 
	  {ok,MapsList :: term()} | {error, Reason :: term()}.
get_maps() ->
    gen_server:call(?SERVER,{get_maps},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_map(DeploymentId :: string()) -> 
	  {ok, Map :: map()} | {error, Reason :: term()}.
get_map(DeploymentId) ->
    gen_server:call(?SERVER,{get_map,DeploymentId},infinity).


%%********************* Repo ************************************

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_repo_updated() -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_repo_updated() ->
    gen_server:call(?SERVER,{is_repo_updated},infinity).

%%--------------------------------------------------------------------
%% @doc
%% repo   
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_repo() -> 
	  ok | {error, Reason :: term()}.
update_repo() ->
    gen_server:call(?SERVER,{update_repo},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_repo() -> 
	  ok | {error, Reason :: term()}.
clone_repo() ->
    gen_server:call(?SERVER,{clone_repo},infinity).


%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    
    
%    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    main_dir=?MainDir,
	    repo_dir=?RepoDir,
	    spec_maps=[],
	    repo_git=?RepoGit
	  
	    
	   },0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

%%********************* Deployment *****************************************
handle_call({get_maps}, _From, State) ->
    Reply=State#state.spec_maps,
    {reply, Reply, State};

handle_call({get_deployments}, _From, State) ->
    Reply= [maps:get(id,Map)||Map<-State#state.spec_maps],
    {reply, Reply, State};

handle_call({get_map,DeploymentId}, _From, State) ->
    Reply=case [Map||Map<-State#state.spec_maps,
		     DeploymentId==maps:get(id,Map)] of
	      []->
		  {error,["DeploymentId doesn't exists",DeploymentId]};
	      [Map]->
		  {ok,Map}
	  end,
    {reply, Reply, State};

handle_call({get_info,Key,DeploymentId}, _From, State) ->
    SpecMaps=State#state.spec_maps,
    Result=try lib_deployment:get_info(Key,DeploymentId,SpecMaps) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Value}->
		  {ok,Value};
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};


%%********************* Repo ************************************
    
handle_call({is_repo_updated}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_deployment:is_repo_updated(RepoDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		  NewState=State,
		  IsUpdated;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({update_repo}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_deployment:update_repo(RepoDir) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  %io:format("UpdateResult ~p~n",[{UpdateResult,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({clone_repo}, _From, State) ->
    RepoDir=State#state.repo_dir,
    RepoGit=State#state.repo_git,
    Result=try lib_deployment:clone_repo(RepoDir,RepoGit) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		%  io:format("CloneResult ~p~n",[{ok,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};
 

%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
    io:format("timeout ~p~n",[{?MODULE,?LINE}]),
    RepoDir=State#state.repo_dir,
    RepoGit=State#state.repo_git,
    Result=try lib_deployment:check_update_repo_return_maps(RepoDir,RepoGit) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    NewState=case Result of
		 {ok,Maps}->
		     io:format("Maps ~p~n",[{Maps,?MODULE,?LINE}]),
		     State#state{spec_maps=Maps};
		 ErrorEvent->
		     io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		     State
	     end,
    
    
    {noreply, NewState};


handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
