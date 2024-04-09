% A template for implementing gen_server callback modules.


-module(genserver_template).

% The server implements the gen_server behavior.
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% Дополнительные вспомогательные функции, экспортируемые модулем обратного вызова.
-export([start/1]).


% TODO:  Реализация вспомогательных функций модуля.

start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).


% TODO:  Поведенческие функции обратного вызова. Реализуем соответствующие функции обратного вызова, необходимые для нашего сервера


$$ Функция handle_call/3 обрабатывает синхронные запросы к серверу процесса.
$$ Функция handle_cast/2 обрабатывает асинхронные запросы к серверу процесса.
%% Функция handle_info/2 обрабатывает информационные сообщения, которые не являются синхронными или асинхронными запросами.
%% Функция terminate/2 выполняет очистку перед завершением сервера процесса.
%% Функция code_change/3 обновляет внутреннее состояние сервера процесса при обновлении/откате версии.


init([]) -> {ok, todo_state}.

handle_call(_Request, _From, State) -> {reply, todo_reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


