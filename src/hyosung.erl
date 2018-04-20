%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Mar 2015 2:42 PM
%%%-------------------------------------------------------------------
-module(hyosung).
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
  InsertId = case re:run(File, ".*(\\d{8}).dat", [{capture, all_but_first, list}]) of
               {match, [TimeStamp]} ->
                 ATMType = ?HYOSUNG,
                 FieldsList = "time_stamp, transactions, atm_type, file_status, file_name",
                 ValuesList = to_sql_list([TimeStamp, "0", ATMType, "0", File], []),
                 {ok, Id} = dao:insert(file, FieldsList, ValuesList),
                 Id;
               _ ->
                 -1
             end,

  erlog:info("Parsing File: ~p With ID: ~p~n", [File, InsertId]),
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

  case re:run(File, ".*(\\d{8}).dat", [{capture, all_but_first, list}]) of
    {match, [TimeStamp2]} ->
      ATMType2 = ?HYOSUNG,
      FieldsValuesList = "time_stamp='" ++ TimeStamp2 ++ "', " ++
        "transactions = '" ++ TransactionCount ++ "', " ++
        "atm_type = '" ++ ATMType2 ++ "', " ++
        "file_status = '" ++ FileStatus ++ "'",
      dao:update(file, FieldsValuesList, "id = '" ++ io_lib:format("~p", [InsertId]) ++ "'");
    _ ->
      ok
  end,
  JSON.

get_session_records([TB | TransactionBlocks], Acc) ->
  erlog:info("       ~p -- Getting Session Records", [hyosung]),
  TransactionBlock = TB,
  CardNumber = get_card_number(TransactionBlock),
  erlog:info("       ~p -- Card Number ~p", [hyosung, CardNumber]),
  AccountNo = get_account_number(TransactionBlock),
  erlog:info("       ~p -- Account Number ~p", [hyosung, AccountNo]),
  {TimeStamp, ATMId} = get_time_stamp(TransactionBlock),
  erlog:info("       ~p -- TimeStamp ~p", [hyosung, TimeStamp]),
  %STAN is not one per transaction, a new stan is generated for each PIN entered
  Actions = get_actions(TransactionBlock, []),
  Records = get_transaction_records(Actions, []),
  erlog:info("Session DATA ====> ~p~n", [{CardNumber, AccountNo, TimeStamp, ATMId}]),
  get_session_records(TransactionBlocks, [{CardNumber, AccountNo, TimeStamp, ATMId, {transactions, Records}} | Acc]);

get_session_records([], Acc) ->
  lists:reverse(Acc).

get_transaction_records([Action | Actions], Acc) ->
  erlog:info("       ~p -- Getting Transaction Records", [hyosung]),
  {Stan, Type, Remark, Status} = get_stan(Action),
  erlog:info("       ~p -- STRM ~p", [hyosung, {Stan, Type, Remark, Status}]),
  Amount = get_amount(Action),
  erlog:info("       ~p -- Amount ~p", [hyosung, Amount]),
  Notes = get_notes(Action),
  erlog:info("       ~p -- Notes ~p", [hyosung, Notes]),
  CashPresented = get_cash_presented(Action),
  erlog:info("       ~p -- Cash Presented ~p", [hyosung, CashPresented]),
  CashTaken = get_cash_taken(Action),
  erlog:info("       ~p -- Cash Taken ~p", [hyosung, CashTaken]),
  CardTaken = get_card_taken(Action),
  erlog:info("       ~p -- Card Taken ~p", [hyosung, CardTaken]),
  get_transaction_records(Actions, [{Stan, Type, Remark, Status, Amount, Notes, CashPresented, CashTaken, CardTaken} | Acc]);

get_transaction_records([], Acc) ->
  lists:reverse(Acc).

get_actions([Line | TransactionBlock], Acc) ->
  case string:str(Line, "PIN ENTERED") of
    0 ->
      get_actions(TransactionBlock, Acc);
    _X ->
      {Action, TransactionBlock2} = get_action(TransactionBlock, []),
      get_actions(TransactionBlock2, [Action | Acc])
  end;

get_actions([], Acc) ->
  lists:reverse(Acc).

get_action([Line | TB], Acc) ->
  case string:str(Line, "PIN ENTERED") of
    0 ->
      get_action(TB, [Line | Acc]);
    _X ->
      {lists:reverse(Acc), [Line | TB]}
  end;

get_action([], Acc) ->
  {lists:reverse(Acc), []}.

get_card_taken(Actions) ->
  erlog:info("       ~p -- Getting isCardTaken", [hyosung]),
  case re:run(Actions, ".*(CARD TAKEN)", [{capture, all_but_first, list}]) of
    nomatch ->
      {card_taken, "0"};
    {match, _X} ->
      {card_taken, "1"}
  end.

get_cash_taken(Actions) ->
  erlog:info("       ~p -- Getting isCashTaken", [hyosung]),
  case re:run(Actions, "\\s*Taken Time\\s+\\[(.+)\\]", [{capture, all_but_first, list}]) of
    nomatch ->
      {cash_taken, "0"};
    {match, [_M]} ->
      {cash_taken, "1"}
  end.

get_cash_presented(Actions) ->
  erlog:info("       ~p -- Getting isCashPresented", [hyosung]),
  case re:run(Actions, "\\s*Present Time\\s+\\[(.+)\\]", [{capture, all_but_first, list}]) of
    nomatch ->
      {cash_presented, "0"};
    {match, [_M]} ->
      {cash_presented, "1"}
  end.

get_notes(Actions) ->
  erlog:info("       ~p -- Getting Notes", [hyosung]),
  case re:run(Actions, "\\s*Dispense Count\\s+\\[(.*)\\]", [{capture, all_but_first, list}]) of
    nomatch ->
      {notes, ""};
    {match, [Notes]} ->
      {notes, Notes}
  end.

get_amount(Actions) ->
  erlog:info("       ~p -- Getting Amount", [hyosung]),
  case re:run(Actions, "[A-Z ]+NGN(\\d+\.00)", [{capture, all_but_first, list}]) of
    nomatch ->
      {amount, ""};
    {match, [Amount]} ->
      {amount, Amount}
  end.

get_stan(Actions) ->
  erlog:info("       ~p -- Getting Stan, Type, Remark, Status", [hyosung]),
  % Is It Inquiry?
  %erlog:info("------------------------------~n~p~n--------------------------------", [Actions]),
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
              % does it have only Stan?
              case re:run(Actions, "\\s*Trans SEQ number\\s+\\[(\\d+)\\]", [{capture, all_but_first, list}]) of
                %NO
                nomatch ->
                  % it's invalid transaction
                  {{stan, ""}, {type, ""}, {remark, ""}, {status, "0"}};
                % Yes
                {match, [Stan]} ->
                  case re:run(Actions, "SEQ NUMB\\s+\\d{4}\nAUTH NUM\\s.*\n\n([A-Z ]+)\\s+(NGN(\\d+\.00))*", [{capture, all_but_first, list}]) of
                    nomatch ->
                      {{stan, Stan}, {type, ""}, {remark, ""}, {status, "1"}};
                    {match, ["INQUIRY"]} ->
                      {{stan, Stan}, {type, "Balance"}, {remark, ""}, {status, "1"}};
                    {match, [Type, _Amount1, _Amount2]} ->
                      {{stan, Stan}, {type, Type}, {remark, ""}, {status, "1"}};
                    {match, [Remark]} ->
                      {{stan, Stan}, {type, ""}, {remark, Remark}, {status, "1"}}
                  end
              end;
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

get_time_stamp(TransactionBlock) ->
  erlog:info("       ~p -- Getting TimeStamp", [hyosung]),
  %erlog:info("Tranaction Block: ====> ~p~n", [TransactionBlock]),
  case re:run(TransactionBlock, "\\s+(\\d+/\\d+/\\d+)\\s+(\\d+:\\d+)\\s+(\\d+)", [{capture, all_but_first, list}]) of
    nomatch ->
      case re:run(TransactionBlock, "(\\d+/\\d+/\\d+)\\s+(\\d+:\\d+:\\d+)\\s+", [{capture, all_but_first, list}]) of
        nomatch ->
          {{time_stamp, ""}, {atm_id, ""}};
        {match, [Date2, Time2]} ->
          {{time_stamp, Date2 ++ " " ++ Time2}, {atm_id, ""}}
      end;
    {match, [Date, Time, ATMId]} ->
      {{time_stamp, Date ++ " " ++ Time}, {atm_id, ATMId}}
  end.

get_account_number(TransactionBlock) ->
  erlog:info("       ~p -- Getting Account Number", [hyosung]),
  case re:run(TransactionBlock, "FROM\\s+.*(\\d{4})", [{capture, all_but_first, list}]) of
    nomatch ->
      {account_no, ""};
    {match, [AccountNo]} ->
      {account_no, AccountNo}
  end.

%DATE & TIME 12/06/2013 03:47:22
%ATMID 00001

get_card_number(TransactionBlock) ->
  erlog:info("       ~p -- Getting Card Number", [hyosung]),
  case re:run(TransactionBlock, "\\s*Card Number\\s+\\[(\\d+.+\\d+)\\]", [{capture, all_but_first, list}]) of
    nomatch ->
      {card_number, ""};
    {match, [CardNumber]} ->
      {card_number, CardNumber}
  end.

get_transaction_blocks([Line | Lines], Acc) ->
  case string:str(Line, "TRANSACTION START") of
    0 ->
      get_transaction_blocks(Lines, Acc);
    _X ->
      {TransactionBlock, Lines2} = get_transaction_block(Lines, []),
      get_transaction_blocks(Lines2, [TransactionBlock | Acc])
  end;


get_transaction_blocks([], Acc) ->
  lists:reverse(Acc).


get_transaction_block([Line | Lines], Acc) ->
  case string:str(Line, "TRANSACTION START") of
    0 ->
      get_transaction_block(Lines, [Line | Acc]);
    _X ->
      {lists:reverse(Acc), [Line | Lines]}
  end;

get_transaction_block([], Acc) ->
  {lists:reverse(Acc), []}.

