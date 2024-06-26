-module(rss_reader).
-include("logging.hrl").
-compile(export_all).
-define(RETRIEVE_INTERVAL,200000).

%% @doc start(Url, QPid) запускает новый процесс чтения
%% @spec start(Url :: string(), QPid :: pid()) -> pid()
%% @param Первый аргумент функции это URL ленты RSS который нужно периодически опрашивать
%% @param Второй аргумент PID процесса rss_queue, которому нужно отправить результат
start(Url,QPid)->
  inets:start()
  ,spawn(?MODULE,server,[Url,QPid]).

%% @doc реализует основной цикл процесса rss_reader
server(Url,QPid)->
%% @doc Загружаем ленту с указанного URL
  {ok,{Status={_,Code,_},_,Load}}=httpc:request(Url)

%% @doc Если код ответа равен 200, извлекаем тело ответа и 
%% разбирает его XML содержимое с помощью 
%% функции xmerl_scan:string/1.
  ,case Code of 
    200 ->
       {Feed,_} = xmerl_scan:string(Load)

%% @doc Когда информация извлечена из тела запроса, 
%% проверяем, что получена лента в формате RSS 2.0.
%% @see rss_parse:is_rss2_feed
       ,case rss_parse:is_rss2_feed(Feed) of
        ok -> 

%% @doc отправляем все элементы ленты в очередь, 
%% которая стоит в паре с этим процессом чтения
%% @see rss_queue:add_feed
          rss_queue:add_feed(QPid,Feed)
          ,receive
%% @doc Ждем заданное время, затем возвращаемся к началу и продолжаем все заново 
%% @see constant RETRIEVE_INTERVAL
            after ?RETRIEVE_INTERVAL -> 
              server(Url,QPid)
            end;
        _ -> {error,not_rss2_feed}
        end ; 
    _ -> {error,Code}
   end.
