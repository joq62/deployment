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
	 init/2
	]).

-export([

	]).

%%%===================================================================
%%% API
%%%===================================================================
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
