%%%-------------------------------------------------------------------
%%% @author ranj4711
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jun 2014 3:01 PM
%%%-------------------------------------------------------------------
-author("ranj4711").

-record (serviceinfo, {
  sd                  :: string(),
  folder  = "/"       :: string(),
  service             :: string(),
  cluster = "default" :: string(),
  uploadid            :: string()
}).

-type serviceinfo() :: serviceinfo().


