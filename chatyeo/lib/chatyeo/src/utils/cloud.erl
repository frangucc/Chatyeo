-module(cloud).
-export([init/0,update/1,update_code/2,compile/1,compile_code/1]).
-import(compile).

node_dir()->"cloud_"++atom_to_list(node())++"/".

%% @doc Update code on local node
%% @spec update_code(Module::filename(), Data::binary()) -> ok

update_code(Module,Data)->
   file:write_file(node_dir()++Module,Data).

%% @doc Compile code on local node
%% @spec compile_code(Module::filename()) -> ok

compile_code(Module)->  compile:file(node_dir()++Module).

%% @doc Compile code of module on all nodes
%% @spec compile(Module::filename()) -> ok

compile(Module)->
   [ rpc:call(Node, cloud, compile_code, [Module])   ||  Node <- [node()|nodes()]].

%% @doc Update module with new code from file on all nodes
%% @spec update(Module::filename()) -> ok

update(Module)->
   {ok,Data} = file:read_file(Module),
   [ rpc:call(Node, cloud, update_code, [Module , Data])   ||  Node <- [node()|nodes()]].

%% @doc Initialize cloud module, must be called before any other function
%% @spec init() -> ok

init()->
  file:make_dir(node_dir()),
  ok.
