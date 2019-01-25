procedure Arrays is
   type Arr is array (1..3) of Integer;
   Actual : Arr := (1,2,3);
begin
   --   Actual(1..2) := Actual(2..3);
   Actual(1) := 1;
   Actual(2) := 2;
   Actual(3) := 3;
--   pragma Assert (Actual(2) = 1);
   pragma Assert (Actual(2) = 2);
end Arrays;
