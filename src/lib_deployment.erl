%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_deployment).
  
-include("deployment.hrl").

 
%% API
-export([
	 init/2,
	 update/2,
	 timer_to_call_update/1
	]).

-export([

	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
timer_to_call_update(Interval)->
  %  io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(Interval),
    rpc:cast(node(),deployment,update,[]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update(RepoDir,GitPath)->
    io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    case rd:call(git_handler,is_repo_updated,[RepoDir],5000) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=rd:call(git_handler,clone,[RepoDir,GitPath],5000);
	false ->
	    io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
	    ok=rd:call(git_handler,update_repo,[RepoDir],5000);
	true ->
	    ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init(RepoDir,GitPath)->
    case rd:call(git_handler,is_repo_updated,[RepoDir],5000) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=rd:call(git_handler,clone,[RepoDir,GitPath],5000);
	false ->
	    ok=rd:call(git_handler,update_repo,[RepoDir],5000);
	true ->
	    ok
    end,
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
