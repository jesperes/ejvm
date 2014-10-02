-module(jvm).
-export([jvm/1]).

-include_lib("class.hrl").
-include_lib("jvm.hrl").

jvm(InitClass) ->
    _Jvm = #jvm{},
    _Class = loader:load_class(InitClass).
