procedure Arrays is
   type Arr is array (1..3) of Integer;
   Actual : Arr := (1,2,3);
begin
--   Actual(1..2) := Actual(2..3);
   pragma Assert (Actual(2) = 1);
end Arrays;
