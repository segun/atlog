%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2015 12:45 PM
%%%-------------------------------------------------------------------
-module(diebold).
-author("aardvocate").

%% API
-export([parse/2, parse_json/2]).

-include("atlogheader.hrl").

parse_json(File, Lines) ->
  io:format("Parsing File: ~p~n", [File]),
  InsertId = 1,
  TransactionBlocks = get_transaction_blocks(Lines, []),
  TransactionRecords = get_session_records(TransactionBlocks, []),
  list_to_binary("{\"sessions\": [" ++ to_json_no_dao(records, InsertId, TransactionRecords, []) ++ "{}]}").
  %list_to_binary("[" ++ to_json_no_dao(records, InsertId, TransactionRecords, []) ++ "{}]").

parse(File, Lines) ->
  io:format("Parsing File: ~p~n", [File]),
  InsertId = case re:run(File, "\\d{8}", [{capture, all, list}]) of
               {match, [TimeStamp]} ->
                 ATMType = ?DIEBOLD,
                 FieldsList = "time_stamp, transactions, atm_type, file_status, file_name",
                 ValuesList = to_sql_list([TimeStamp, "0", ATMType, "0", File], []),
                 {ok, Id} = dao:insert(file, FieldsList, ValuesList),
                 Id;
               _ ->
                 -1
             end,

  TransactionBlocks = get_transaction_blocks(Lines, []),
  TransactionRecords = get_session_records(TransactionBlocks, []),
  JSON = "[" ++ to_json(records, InsertId, TransactionRecords, []) ++ "{}]",
  TransactionCount = io_lib:format("~p", [length(TransactionBlocks)]),
  FileStatus = case JSON of
                 "[{}]" ->
                   "0";
                 _ ->
                   "1"
               end,

  case re:run(File, "\\d{8}", [{capture, all, list}]) of
    {match, [TimeStamp2]} ->
      ATMType2 = ?WINCOR,
      FieldsValuesList = "time_stamp='" ++ TimeStamp2 ++ "', " ++
        "transactions = '" ++ TransactionCount ++ "', " ++
        "atm_type = '" ++ ATMType2 ++ "', " ++
        "file_status = '" ++ FileStatus ++ "'",
      dao:update(file, FieldsValuesList, "id = '" ++ io_lib:format("~p", [InsertId]) ++ "'");
    _ ->
      ok end,
  JSON.

get_actions([Line | TransactionBlock], Acc) ->
  case string:str(Line, "_PIN ENTER") of
    0 ->
      get_actions(TransactionBlock, Acc);
    _X ->
      {Action, TransactionBlock2} = get_action(TransactionBlock, []),
      get_actions(TransactionBlock2, [Action | Acc])
  end;

get_actions([], Acc) ->
  lists:reverse(Acc).

get_action([Line | TB], Acc) ->
  case string:str(Line, "_PIN ENTER") of
    0 ->
      get_action(TB, [Line | Acc]);
    _X ->
      {lists:reverse(Acc), [Line | TB]}
  end;

get_action([], Acc) ->
  {lists:reverse(Acc), []}.

get_time_stamp(TransactionBlock) ->
  erlog:info("       ~p -- Getting TimeStamp", [diebold]),
  case re:run(TransactionBlock, "\\s+(\\d+/\\d+/\\d+)\\s+(\\d+:\\d+)\\s+(\\d+)", [{capture, all_but_first, list}]) of
    nomatch ->
      {{time_stamp, ""}, {atm_id, ""}};
    {match, [Date, Time, ATMId]} ->
      {{time_stamp, Date ++ " " ++ Time}, {atm_id, ATMId}}
  end.

get_account_number(TransactionBlock) ->
  erlog:info("       ~p -- Getting Account Number", [diebold]),
  case re:run(TransactionBlock, "FROM\\s+.*(\\d{4})", [{capture, all_but_first, list}]) of
    nomatch ->
      {account_no, ""};
    {match, [AccountNo]} ->
      {account_no, AccountNo}
  end.

get_card_number(TransactionBlock) ->
  erlog:info("       ~p -- Getting Card Number", [diebold]),
  case re:run(TransactionBlock, ".*_PIN ENTER\\s*Card\\s*(\\d{4}.*\\d{4})", [{capture, all_but_first, list}]) of
    nomatch ->
      {card_number, ""};
    {match, [CardNumber]} ->
      {card_number, CardNumber}
  end.

get_stan(Actions) ->
  erlog:info("       ~p -- Getting Stan, Type, Remark, Status", [diebold]),
  case re:run(Actions, "\\d+\\s+(\\d{4}).*\n(INQUIRY)", [{capture, all_but_first, list}]) of
  %No
    nomatch ->
      % does it have Stan and Remark?
      case re:run(Actions, "\\d+\\s+(\\d{4}).*\n([A-Z ]+)", [{capture, all_but_first, list}]) of
      % No
        nomatch ->
          % does it have Stan, Type and Amount?
          case re:run(Actions, "\\d+\\s+(\\d{4}).*\n([A-Z ]+)NGN\\d+.00", [{capture, all_but_first, list}]) of
          % No
            nomatch ->
              % it's invalid transaction
              {{stan, ""}, {type, ""}, {remark, ""}, {status, "0"}};
          % Yes
            {match, [Stan, Type]} ->
              {{stan, Stan}, {type, Type}, {remark, ""}, {status, "1"}}
          end;
      % Yes
        {match, [Stan, Remark]} ->
          {{stan, Stan}, {type, ""}, {remark, Remark}, {status, "1"}}
      end;
  % Yes
    {match, [Stan, _Type]} ->
      {{stan, Stan}, {type, "Balance"}, {remark, ""}, {status, "1"}}
  end.

get_card_taken(Actions) ->
  erlog:info("       ~p -- Getting isCardTaken", [diebold]),
  case re:run(Actions, ".*_Card ejected", [{capture, all_but_first, list}]) of
    nomatch ->
      {card_taken, "0"};
    {match, _X} ->
      {card_taken, "1"}
  end.

get_cash_taken(Actions) ->
  erlog:info("       ~p -- Getting isCashTaken", [diebold]),
  case re:run(Actions, ".*_CASH TAKEN", [{capture, all_but_first, list}]) of
    nomatch ->
      {cash_taken, "0"};
    {match, _M} ->
      {cash_taken, "1"}
  end.

get_cash_presented(Actions) ->
  erlog:info("       ~p -- Getting isCashPresented", [diebold]),
  case re:run(Actions, ".*_CASH PRESENTED", [{capture, all_but_first, list}]) of
    nomatch ->
      {cash_presented, "0"};
    {match, _X} ->
      {cash_presented, "1"}
  end.

get_notes(Actions) ->
  erlog:info("       ~p -- Getting Notes", [diebold]),
  case re:run(Actions, ".*Pickup ATM\\s*(\\d\\s\\d\\s\\d\\s\\d)", [{capture, all_but_first, list}]) of
    nomatch ->
      {notes, ""};
    {match, [Notes]} ->
      {notes, Notes}
  end.

get_amount(Actions) ->
  erlog:info("       ~p -- Getting Amount", [diebold]),
  case re:run(Actions, "[A-Z ]+NGN(\\d+\.00)", [{capture, all_but_first, list}]) of
    nomatch ->
      {amount, ""};
    {match, [Amount]} ->
      {amount, Amount}
  end.

get_transaction_records([Action | Actions], Acc) ->
  erlog:info("       ~p -- Getting Transaction Records", [diebold]),
  {Stan, Type, Remark, Status} = get_stan(Action),
  erlog:info("       ~p -- STRM ~p", [diebold, {Stan, Type, Remark, Status}]),
  Amount = get_amount(Action),
  erlog:info("       ~p -- Amount ~p", [diebold, Amount]),
  Notes = get_notes(Action),
  erlog:info("       ~p -- Notes ~p", [diebold, Notes]),
  CashPresented = get_cash_presented(Action),
  erlog:info("       ~p -- Cash Presented ~p", [diebold, CashPresented]),
  CashTaken = get_cash_taken(Action),
  erlog:info("       ~p -- Cash Taken ~p", [diebold, CashTaken]),
  CardTaken = get_card_taken(Action),
  erlog:info("       ~p -- Card Taken ~p", [diebold, CardTaken]),
  get_transaction_records(Actions, [{Stan, Type, Remark, Status, Amount, Notes, CashPresented, CashTaken, CardTaken} | Acc]);

get_transaction_records([], Acc) ->
  lists:reverse(Acc).

get_session_records([TB | TransactionBlocks], Acc) ->
  erlog:info("       ~p -- Getting Session Records", [diebold]),
  TransactionBlock = TB,
  erlog:info("-------Transaction Block-------"),
  erlog:alert("~p",[TB]),
  CardNumber = get_card_number(TransactionBlock),
  erlog:info("       ~p -- Card Number ~p", [diebold, CardNumber]),
  AccountNo = get_account_number(TransactionBlock),
  erlog:info("       ~p -- Account Number ~p", [diebold, AccountNo]),
  {TimeStamp, ATMId} = get_time_stamp(TransactionBlock),
  erlog:info("       ~p -- Timestamp,ATMId ~p", [diebold, {TimeStamp, ATMId}]),
  %STAN is not one per transaction, a new stan is generated for each PIN entered
  Actions = get_actions(TransactionBlock, []),
  Records = get_transaction_records(Actions, []),
  get_session_records(TransactionBlocks, [{CardNumber, AccountNo, TimeStamp, ATMId, {transactions, Records}} | Acc]);

get_session_records([], Acc) ->
  lists:reverse(Acc).


get_transaction_blocks([Line | Lines], Acc) ->
  case string:str(Line, "_new transaction") of
    0 ->
      get_transaction_blocks(Lines, Acc);
    _X ->
      {TransactionBlock, Lines2} = get_transaction_block(Lines, []),
      get_transaction_blocks(Lines2, [TransactionBlock | Acc])
  end;


get_transaction_blocks([], Acc) ->
  lists:reverse(Acc).


get_transaction_block([Line | Lines], Acc) ->
  case string:str(Line, "_Card ejected") of
    0 ->
      get_transaction_block(Lines, [Line | Acc]);
    _X ->
      {lists:reverse([Line | Acc]), Lines}
  end;

get_transaction_block([], Acc) ->
  {lists:reverse(Acc), []}.
