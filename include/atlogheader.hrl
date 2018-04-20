%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2014 2:21 PM
%%%-------------------------------------------------------------------
-author("aardvocate").

-define(WINCOR, "0").
-define(DIEBOLD, "1").
-define(HYOSUNG, "2").

to_json_no_dao(records, FId, [TR | TransactionRecords], Acc) ->
  erlog:info("Session in json ===> ~p~n", [TR]),
  {{card_number, CardNumber}, {account_no, AccountNo}, {time_stamp, TimeStamp}, {atm_id, ATMId}, {transactions, Transactions}} = TR,
  case ATMId of
    "" ->
      to_json_no_dao(records, FId, TransactionRecords, Acc);
    _ ->
      InsertId = 1,
      TransactionsJSON = to_json_no_dao(transactions, io_lib:format("~p", [InsertId]), Transactions, []),
      case TransactionsJSON of
        "" ->
          to_json_no_dao(records, FId, TransactionRecords, Acc);
        _ ->
          CardNumberJson = "\"card_number\" : \"" ++ CardNumber ++ "\"",
          TimeStampJson = "\"time_stamp\" : \"" ++ TimeStamp ++ "\"",
          ATMIdJson = "\"atm_id\" : \"" ++ ATMId ++ "\"",
          AccountNoJSON = "\"account_no\":\"" ++ AccountNo ++ "\"",
          TRJson = "{\"session\" : {" ++ CardNumberJson ++ "," ++ AccountNoJSON ++ "," ++ TimeStampJson ++ "," ++ ATMIdJson ++ ", \"transactions\" : [" ++ TransactionsJSON ++ " {}]" ++ "}},",
          to_json_no_dao(records, FId, TransactionRecords, [TRJson | Acc])
      end
  end;

to_json_no_dao(records, _FileId, [], Acc) ->
  lists:reverse(Acc);

to_json_no_dao(transactions, SessionId, [T | Transactions], Acc) ->
  erlog:info("Transaction in json ===> ~p~n", [T]),
  {{stan, Stan}, {type, Type}, {remark, Remark}, {status, Status}, {amount, Amount}, {notes, Notes}, {cash_presented, CashPresented}, {cash_taken, CashTaken}, {card_taken, CardTaken}} = T,
  case Stan of
    "" ->
      to_json_no_dao(transactions, SessionId, Transactions, Acc);
    _ ->
      StanJson = "\"stan\" : \"" ++ Stan ++ "\"",
      TypeJson = "\"type\" : \"" ++ Type ++ "\"",
      RemarkJson = "\"remark\" : \"" ++ Remark ++ "\"",
      AmountJson = "\"amount\" : \"" ++ Amount ++ "\"",
      NotesJson = "\"notes\" : \"" ++ Notes ++ "\"",
      CashPJson = "\"cash_presented\" : \"" ++ CashPresented ++ "\"",
      CashTJson = "\"cash_taken\" : \"" ++ CashTaken ++ "\"",
      StatusJson = "\"status\" : \"" ++ Status ++ "\"",
      CardTJson = "\"card_taken\" : \"" ++ CardTaken ++ "\"",
      TJson = "{\"transaction\": {" ++ StanJson ++ "," ++ TypeJson ++ "," ++ RemarkJson ++ "," ++ StatusJson ++ "," ++ AmountJson ++ "," ++ NotesJson ++ "," ++ CashPJson ++ "," ++ CashTJson ++ "," ++ CardTJson ++ "}},",
      to_json_no_dao(transactions, SessionId, Transactions, [TJson | Acc])
  end;

to_json_no_dao(transactions, _SessionId, [], Acc) ->
  lists:reverse(Acc).

to_json(records, FId, [TR | TransactionRecords], Acc) ->
  erlog:info("Session in json ===> ~p~n", [TR]),
  FileId = io_lib:format("~p", [FId]),
  {{card_number, CardNumber}, {account_no, AccountNo}, {time_stamp, TimeStamp}, {atm_id, ATMId}, {transactions, Transactions}} = TR,
  case ATMId of
    "" ->
      to_json(records, FId, TransactionRecords, Acc);
    _ ->
      {ok, InsertId} = dao:insert(session, "terminal_id, file_id, pan, account_no, time_stamp", to_sql_list([ATMId, FileId, CardNumber, AccountNo, TimeStamp], [])),
      TransactionsJSON = to_json(transactions, io_lib:format("~p", [InsertId]), Transactions, []),
      case TransactionsJSON of
        "" ->
          to_json(records, FId, TransactionRecords, Acc);
        _ ->
          CardNumberJson = "\"card_number\" : \"" ++ CardNumber ++ "\"",
          TimeStampJson = "\"time_stamp\" : \"" ++ TimeStamp ++ "\"",
          ATMIdJson = "\"atm_id\" : \"" ++ ATMId ++ "\"",
          AccountNoJSON = "\"account_no\":\"" ++ AccountNo ++ "\"",
          TRJson = "{\"session\" : {" ++ CardNumberJson ++ "," ++ AccountNoJSON ++ "," ++ TimeStampJson ++ "," ++ ATMIdJson ++ ", \"transactions\" : [" ++ TransactionsJSON ++ " {}]" ++ "}},",
          to_json(records, FId, TransactionRecords, [TRJson | Acc])
      end
  end;

to_json(records, _FileId, [], Acc) ->
  lists:reverse(Acc);

to_json(transactions, SessionId, [T | Transactions], Acc) ->
  erlog:info("Transaction in json ===> ~p~n", [T]),
  {{stan, Stan}, {type, Type}, {remark, Remark}, {status, Status}, {amount, Amount}, {notes, Notes}, {cash_presented, CashPresented}, {cash_taken, CashTaken}, {card_taken, CardTaken}} = T,
  case Stan of
    "" ->
      to_json(transactions, SessionId, Transactions, Acc);
    _ ->
      FieldsList = "session_id, record_type, stan, amount, notes, bills_dispensed, bills_taken, card_taken, remark, type, status",
      ValuesList = to_sql_list([SessionId, "Transactional", Stan, Amount, Notes, CashPresented, CashTaken, CardTaken, Remark, Type, Status], []),
      dao:insert(transaction, FieldsList, ValuesList),
      StanJson = "\"stan\" : \"" ++ Stan ++ "\"",
      TypeJson = "\"type\" : \"" ++ Type ++ "\"",
      RemarkJson = "\"remark\" : \"" ++ Remark ++ "\"",
      AmountJson = "\"amount\" : \"" ++ Amount ++ "\"",
      NotesJson = "\"notes\" : \"" ++ Notes ++ "\"",
      CashPJson = "\"cash_presented\" : \"" ++ CashPresented ++ "\"",
      CashTJson = "\"cash_taken\" : \"" ++ CashTaken ++ "\"",
      StatusJson = "\"status\" : \"" ++ Status ++ "\"",
      CardTJson = "\"card_taken\" : \"" ++ CardTaken ++ "\"",
      TJson = "{\"transaction\": {" ++ StanJson ++ "," ++ TypeJson ++ "," ++ RemarkJson ++ "," ++ StatusJson ++ "," ++ AmountJson ++ "," ++ NotesJson ++ "," ++ CashPJson ++ "," ++ CashTJson ++ "," ++ CardTJson ++ "}},",
      to_json(transactions, SessionId, Transactions, [TJson | Acc])
  end;

to_json(transactions, _SessionId, [], Acc) ->
  lists:reverse(Acc).

to_sql_list([H | T], Acc) ->
  to_sql_list(T, ["'" ++ H ++ "'," | Acc]);

to_sql_list([], Acc) ->
  Str = lists:flatten(lists:reverse(Acc)),
  Index = length(Str) - 1,
  string:sub_string(Str, 1, Index).

read_lines(Device, Acc) ->
  case io:get_line(Device, "") of
    eof ->
      file:close(Device),
      L = lists:reverse(Acc),
      L2 = lists:map(fun(S) -> re:replace(S, "\\\\", "/", [global, {return, list}]) end, L),
      lists:map(fun(S) -> re:replace(S, "-", "/", [global, {return, list}]) end, L2);
    Line ->
      read_lines(Device, [Line | Acc])
  end.