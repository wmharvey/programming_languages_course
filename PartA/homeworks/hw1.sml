(* Assignment 1 *)

(* You will write 11 SML functions (and tests for them) related to calendar dates. In all problems, a “date” is an SML value of type int*int*int, where the first part is the year, the second part is the month, and the third part is the day. A “reasonable” date has a positive year, a month between 1 and 12, and a day no greater than 31 (or less depending on the month). Your solutions need to work correctly only for reasonable dates, but do not check for reasonable dates (that is a challenge problem) and many of your functions will naturally work correctly for some/all non-reasonable dates. A “day of year” is a number from 1 to 365 where, for example, 33 represents February 2. (We ignore leap years except in one challenge problem.) *)

(* Write a function is_older that takes two dates and evaluates to true or false. 
It evaluates to true if the first argument is a date that comes before the second argument. 
(If the two dates are the same, the result is false.) *)

fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
	let fun is_older_list (list1 : int list, list2 : int list) =
		if null list1
		then false
		else if hd list1 = hd list2
		then is_older_list(tl list1, tl list2)
		else hd list1 < hd list2
	in
	    is_older_list([#1 date1, #2 date1, #3 date1],  [#1 date2, #2 date2, #3 date2])
	end

(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) 
and returns how many dates in the list are in the given month. *)
	    
fun number_in_month (dateList : (int * int * int) list, month : int) =
    let fun monthCounter (count : int, dates : (int * int * int) list) =
	    if null dates
	    then count
	    else if #2 (hd dates) = month
	    then monthCounter(count + 1, tl dates)
	    else monthCounter(count, tl dates)			      
    in monthCounter (0, dateList)
    end

(* Write a function number_in_months that takes a list of dates and a list of months 
(i.e., an int list) and returns the number of dates in the list of dates that are in any of 
the months in the list of months. Assume the list of months has no number repeated. *)
											    
fun number_in_months (dates: (int * int * int) list, months: int list) =
    let fun monthCounter (count: int, remMonths: int list) =
	    if null remMonths
	    then count
	    else monthCounter(count + number_in_month(dates, hd remMonths), tl remMonths)
    in monthCounter (0, months)
    end

(* Write a function dates_in_month that takes a list of dates and a month (i.e., an int) 
and returns a list holding the dates from the argument list of dates that are in the month. 
The returned list should contain dates in the order they were originally given. *)

fun append (xs : (int*int*int) list, ys: (int*int*int) list) =
    if null xs
    then ys
    else (hd xs) :: append(tl xs, ys)
			  
fun dates_in_month (dates: (int * int * int) list, month: int) =
    let fun dateAccumulator (datesInMonth: (int*int*int) list, remDates: (int*int*int) list) =
	    if null remDates
	    then datesInMonth
	    else if #2 (hd remDates) = month
	    then dateAccumulator(append(datesInMonth, [hd remDates]) , tl remDates)
	    else dateAccumulator(datesInMonth, tl remDates)
    in dateAccumulator([], dates)
    end

(* Write a function dates_in_months that takes a list of dates and a list of months and 
returns a list holding the dates from the argument list of dates that are in any of the months 
in the list of months. Assume the list of months has no number repeated. *)
			  
fun dates_in_months (dates: (int*int*int) list, months: int list) =
    let fun dateAccumulator (remMonths: int list, dateAcc: (int*int*int) list) =
	    if null remMonths
	    then dateAcc
	    else let val newDates = dates_in_month(dates, hd remMonths)
		 in dateAccumulator (tl remMonths, append(dateAcc, newDates))
		 end
    in dateAccumulator(months, [])
    end

(* Write a function get_nth that takes a list of strings and an int n and returns the nth 
element of the list where the head of the list is 1st. Do not worry about the case where the 
list has too few elements: your function may apply hd or tl to the empty list in this case, 
which is okay. *)

fun get_nth (strings: string list, num: int) =
    let fun check_nth (position: int, remStrings: string list) =
	    if position = num
	    then hd remStrings
	    else check_nth (position + 1, tl remStrings)
    in check_nth (1, strings)
    end

(* Write a function date_to_string that takes a date and returns a string of the form 
January 20, 2013 (for example). Use the operator ^ for concatenating strings and the library 
function Int.toString for converting an int to a string. For producing the month part, 
do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer 
to the previous problem. For consistency, put a comma following the day and use capitalized 
English month names: January, February, March, April, May, June, July, August, September, 
October, November, December. *)

fun date_to_string (date: (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val month = get_nth (months, #2 date)
    in month ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

(* Write a function number_before_reaching_sum that takes an int called sum, which you can 
assume is positive, and an int list, which you can assume contains all positive numbers, 
and returns an int. You should return an int n such that the first n elements of the list 
add to less than sum, but the first n + 1 elements of the list add to sum or more. Assume 
the entire list sums to more than the passed in value; it is okay for an exception to occur 
if this is not the case. *)

fun number_before_reaching_sum (sum : int, intList: int list) =
    let fun checkSum (curSum: int, remList: int list, position: int) =
	    if curSum + hd remList >= sum
	    then position
	    else checkSum (curSum + hd remList, tl remList, position + 1)
    in checkSum (0, intList, 0)
    end
    
(* Write a function what_month that takes a day of year (i.e., an int between 1 and 365) 
and returns what month that day is in (1 for January, 2 for February, etc.). Use a list 
holding 12 integers and your answer to the previous problem. *)

fun what_month (dayOfYear: int) =
    let val daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]	      
    in number_before_reaching_sum (dayOfYear, daysInMonth) + 1
    end

(* Write a function month_range that takes two days of the year day1 and day2 and returns an 
int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and 
mn is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 
if day1>day2. *)

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else let fun monthAccumulator (countDown: int, monthAcc: int list) =
		 if (day2 - countDown) < day1
		 then monthAcc
		 else monthAccumulator(countDown + 1, what_month(day2 - countDown) :: monthAcc)
	 in monthAccumulator(0, [])
	 end

(* Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. 
It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the 
list. *)

fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else let fun oldest_nonempty (dates: (int*int*int) list, oldest: (int * int * int)) =
		 if null dates
		 then oldest
		 else let val isOlder = is_older(oldest, hd dates)
		      in
			  if isOlder
			  then oldest_nonempty(tl dates, oldest)
			  else oldest_nonempty(tl dates, hd dates)
		      end
	 in SOME (oldest_nonempty (tl dates, hd dates))
	 end
