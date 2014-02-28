:- module http_date.

% Copyright (C) 2013 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module time.

:- func timestamp_to_http_date(time_t) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

    % RFC 2616, 3.3.1 Full Date
    %
timestamp_to_http_date(Time) = Expires :-
    TM = time.gmtime(Time),
    Wkday = TM ^ tm_wday,
    Day = TM ^ tm_mday,
    Month = TM ^ tm_mon,
    Year = TM ^ tm_year + 1900,
    Hour = TM ^ tm_hour,
    Min = TM ^ tm_min,
    Sec = TM ^ tm_sec,
    (
        wkday(Wkday, WkdayStr),
        month(Month, MonthStr)
    ->
        string.format("%s, %02d %s %04d %02d:%02d:%02d GMT",
            [s(WkdayStr), i(Day), s(MonthStr), i(Year),
                i(Hour), i(Min), i(Sec)], Expires)
    ;
        unexpected($module, $pred, "invalid weekkday or month")
    ).

:- pred wkday(int::in, string::out) is semidet.

wkday(0, "Sun").
wkday(1, "Mon").
wkday(2, "Tue").
wkday(3, "Wed").
wkday(4, "Thu").
wkday(5, "Fri").
wkday(6, "Sat").

:- pred month(int::in, string::out) is semidet.

month(0, "Jan").
month(1, "Feb").
month(2, "Mar").
month(3, "Apr").
month(4, "May").
month(5, "Jun").
month(6, "Jul").
month(7, "Aug").
month(8, "Sep").
month(9, "Oct").
month(10, "Nov").
month(11, "Dec").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
