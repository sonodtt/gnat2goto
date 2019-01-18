procedure Arrays is
   subtype Range1 is Integer range 10 .. 20;
   type Arr3 is array (Range1) of Integer;
   Actual3 : Arr3 := (7, 8, 9, others => 10);
begin
   Actual3 (13 .. 16) := Actual3 (11 .. 12) & Actual3 (15 .. 16);
   pragma Assert (Actual3(13) = 11);
end Arrays;
