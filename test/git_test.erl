%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(git_test).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("deployment.hrl").
-define(TestDeployment,"test_c50").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=git_repo(),
 
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(1000),
    init:stop(),
    ok.


    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_repo()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {error,{error,["RepoDir doesnt exists, need to clone"]}}=deployment:is_repo_updated(),
    ok=deployment:clone_repo(),
    true=deployment:is_repo_updated(),
    {error,{error,["Already updated ","catalog/deployment_specs"]}}=deployment:update_repo(),

    [
     {deployments,[{"adder","c50"},{"adder","c50"},{"adder","c50"},{"divi","c50"},{"divi","c50"}]},
     {deployments,[{"adder","c50"},{"divi","c50"}]},
     {id,"test_c50"},
     {id,"test_c50_1"}
    ]=lists:sort(lists:append([maps:to_list(Map)||Map<-deployment:get_maps()])),


    {ok,Deployments}=deployment:get_info(deployments,?TestDeployment),
    [
     {"adder","c50"},
     {"adder","c50"},
     {"adder","c50"},			
     {"divi","c50"},
     {"divi","c50"}
    ]=lists:sort(Deployments),
   
    

    {error,{badkey,glurk},_,_,_}=deployment:get_info(glurk,?TestDeployment),
    {error,["DeploymentId doesn't exists",glurk]}=deployment:get_info(password,glurk),
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    true=filelib:is_dir(?MainDir),
    file:del_dir_r(?MainDir),
    false=filelib:is_dir(?MainDir),
    file:make_dir(?MainDir),
    true=filelib:is_dir(?MainDir),
    false=filelib:is_dir(?RepoDir),

    
    ok.
