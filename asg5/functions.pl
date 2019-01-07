/*
Terry Guan
CMPS 112 
teguan
teguan@ucsc.edu
1528854
*/



not( x ) :- x, !, fail.
not( _ ).


degree_min_to_degree( degmin( Long, Longmin ),
   degmin( Lat, Latmin ), Longdeg, Latdeg ) :- 
      Longdeg is Long + Longmin / 60,
      Latdeg is Lat + Latmin / 60.
   
degree_to_distance( Long1, Long2, Lat1, Lat2, Distance ) :-
   Dlong is Long2 - Long1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2) ** 2 + cos( Lat1 ) 
      * cos( Lat2 ) * sin( Dlong / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

degree_to_rad( In_degree, In_rad ) :-
   In_rad is In_degree * pi / 180.


distance_between_airports( Airport1, Airport2, Distance) :-
   airport( Airport1, _, Lat1, Long1),
   airport( Airport2, _, Lat2, Long2),
   degree_min_to_degree( Long1, Lat1, Long_hour1, Lat_hour1 ),
   degree_min_to_degree( Long2, Lat2, Long_hour2, Lat_hour2 ),
   degree_to_rad( Lat_hour1, Latrad1 ),
   degree_to_rad( Lat_hour2, Latrad2 ),
   degree_to_rad( Long_hour1, Longrad1 ),
   degree_to_rad( Long_hour2, Longrad2 ),
   degree_to_distance( Longrad1, Longrad2, Latrad1, Latrad2, Dist ),
   Distance is Dist.


distance_to_time( Dist, Time_in_hours ) :-
   Time_in_hours is Dist/500.

flight_time_to_hours( time( Hours, Min ), True_hour ) :-
   True_hour is Hours + Min / 60.

/* flights are always 500 mph */
try_sequence( Arrival_dest, Arrival_dest, Curr_time, Seq, [Visits] ).
try_sequence( Depart_dest, Arrival_dest, Curr_time, Seq, 
   [Takeoff_hour, Depart_dest, Arrival_time, Next_dest | Visits] ) :-
   flight(Depart_dest, Next_dest, Takeoff_time),
   not( member( Next_dest, Seq )),
   %write( Seq ),
   flight_time_to_hours( Takeoff_time, Takeoff_hour ),
   %write( '\n Curr Time is: '), write(Curr_time),
   %write( '\nTakeoff_hour is: '), write( Takeoff_hour),

   Takeoff_hour > Curr_time,
   distance_between_airports( Depart_dest, Arrival_dest, Dist),
   %write( '\n dist is the following' ), write( Dist ),
   distance_to_time( Dist, Flight_length ),
   %write( '\nFlight_length, Takeoff_hour are respectively' ),
   %write( Flight_length ), write( Takeoff_hour),
   Current_time is Takeoff_hour + Flight_length + 0.5,
   %write( '\n after adding 0.5' ),
   Arrival_time is Takeoff_hour + Flight_length,
  % write( '\n------------------------' ),
   %write( '\nCurrTime after adding is: ' ), write( Current_time ), 
   try_sequence( Next_dest, Arrival_dest, Current_time, [Next_dest | Seq],
      Visits).

write_specifics( Digits ) :-
   Digits >= 10,
   write( Digits ).

write_specifics( Digits ) :-
   Digits < 10,
   write( '0' ), write( Digits ).

write_time( Val ) :-
   Time is floor( Val * 60 ),
   Hour is  Time //  60,
   Minute is Time mod  60,
   write_specifics( Hour ), write( ' : ' ),
       write_specifics( Minute ).
 

write_visits( [ Takeoff, Depart_dest, Arrival_time, Arrival_dest, Nada | []] ) :-
   write( 'depart  ' ), write( Depart_dest ),
   airport( Depart_dest, Airport, _, _ ),
   write( ' ' ), write( Airport ),
   write( ' ' ),
   write_time( Takeoff ),
   write( '\nArrive  ' ), write( Arrival_dest ),
   airport( Arrival_dest, Airport1, _, _ ),
   write( ' ' ), write( Airport1 ),
   write( ' ' ), write_time( Arrival_time ).

 
write_visits( [Takeoff_time, Depart_dest, Arrival_time, Arrival_dest | Visit] ) :-
   write( 'depart  ' ), write( Depart_dest ),
   airport( Depart_dest, Airport, _, _ ),
   write( ' ' ), write( Airport ), 
   write( ' ' ), write_time( Takeoff_time ),
   write( '\nArrive  ' ), write( Arrival_dest ),
   airport( Arrival_dest, Airport1, _, _ ),
   write( ' ' ), write( Airport1 ),
   write( ' ' ), write_time( Arrival_time ),
   write( '\n' ),
   write_visits( Visit ).

fly( Airport1, Airport1 ) :-
   write( 'you are already at '),
   write( Airport1 ).

fly( Airport1, Airport2 ) :-
   airport( Airport1, _, _, _ ),
   airport( Airport2, _, _, _ ),
   %airport( Airport1, _, _, _),
   %airport( Airport2, _, _, _),
   /*write( '\nAp1 and Ap2 is : ' ),
   write( Airport1 ), write( Airport2 ),*/
  % write( '\n before entering the try sequqence' ),
   try_sequence( Airport1, Airport2, 0, [Airport1], Visits),
  % write( '\n--- finished ---\n' ),
  % write( '\n\n\n\n\n\n\n\n' ),
  % write( Visits ),
   write( '\n' ),
   write_visits( Visits ).
  % write( '\n \n \n' )
  
fly( Airport1, Airport2 ) :-
   airport( Airport1, _, _, _ ),
   airport( Airport2, _, _, _ ),
   write( 'The flight from ' ), write( Airport1 ),
   write( ' to ' ), write( Airport2 ),
   write( ' does not exist.' ).

fly( Airport1, Airport2 ) :-
   write( 'One of these airports: ' ), 
   write( Airport1 ), write( ', '), write( Airport2 ),
   write( ' does not exist.' ).


