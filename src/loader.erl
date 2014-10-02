-module(loader).
-export([load_class/1]).

-include("class.hrl").

%% Stupid class loader which can only load classes from current directory.

classname_to_filename(ClassName) when is_list(ClassName) ->
    lists:map(
      fun($.) ->
	      $/;
	 (X) ->
	      X
      end, ClassName).
	
load_class(ClassName) ->
    FileName = classname_to_filename(ClassName),
    CF = classfile:load_class(FileName),
    #class{classfile = CF}.
