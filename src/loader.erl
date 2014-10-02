-module(loader).
-export([load_class/2]).

-include_lib("class.hrl").
-include_lib("eunit/include/eunit.hrl").

classname_to_filename(ClassName) when is_list(ClassName) ->
    lists:map(
      fun($.) ->
	      $/;
	 (X) ->
	      X
      end, ClassName) ++ ".class".

find_in_classpath([], ClassName) ->
    throw({class_not_found, ClassName});
find_in_classpath([P|Ps], ClassName) -> 
    F = P ++ "/" ++ ClassName,
    %% ?debugFmt("Checking ~s", [F]),
    case file:read_file_info(F) of
	{ok, _} ->
	    F;
	_ ->
	    find_in_classpath(Ps, ClassName)
    end.

load_class(ClassName, ClassPath) ->
    %%?debugFmt("Loading class ~s from ~w~n", [ClassName, ClassPath]),
    Path = find_in_classpath(ClassPath, classname_to_filename(ClassName)),
    %%?debugFmt("Loading class: ~s~n", [Path]),
    CF = classfile:load_classfile(Path),
    #class{classfile = CF}.

find_in_classpath_test() ->
    ?assertEqual("../priv/Test.class", find_in_classpath([".", "..", "../priv"], "Test.class")).

