with Uint_To_Binary; use Uint_To_Binary;
with Ureal_To_Binary; use Ureal_To_Binary;

package body Binary_To_Hex is

   function Strip_Leading_Zeroes (Str : String) return String;

   function Convert_Binary_To_Hex (Binary : String) return String is
      type Hex_Digit_Pos is mod 16;
      --  this needs to be uppercase for CBMC
      type Hex_Digit is ('0', '1', '2', '3', '4',
                         '5', '6', '7', '8', '9',
                         'A', 'B', 'C', 'D', 'E',
                         'F');
      subtype Binary_Quartet_T is String (1 .. 4);

      function Convert_Binary_To_Hex_Digit (Binary_Quartet : Binary_Quartet_T)
                                           return Hex_Digit;

      function Convert_Binary_To_Hex_Digit (Binary_Quartet : Binary_Quartet_T)
                                           return Hex_Digit is
         Pos : Hex_Digit_Pos := 0;
      begin
         for Digit of Binary_Quartet loop
            Pos := Pos * 2;
            if Digit = '1' then
               Pos := Pos + 1;
            else
               pragma Assert (Digit = '0');
            end if;
         end loop;
         return Hex_Digit'Val (Pos);
      end Convert_Binary_To_Hex_Digit;

      Result : String (1 .. ((Binary'Length / 4)));
   begin
      for I in Result'Range loop
         declare
            Binary_Quartet_Start : constant Integer := Binary'First +
              --  -1 because I starts at 1
              (I - 1) * 4;
            Binary_Quartet_End : constant Integer := Binary_Quartet_Start + 3;
            Binary_Quartet : constant Binary_Quartet_T := Binary
              (Binary_Quartet_Start .. Binary_Quartet_End);
         begin
            Result (I) := Character'Value (Hex_Digit'Image
              (Convert_Binary_To_Hex_Digit (Binary_Quartet)));
         end;
      end loop;
      return Strip_Leading_Zeroes (Result);
   end Convert_Binary_To_Hex;

   function Convert_Uint_To_Hex (Value : Uint; Bit_Width : Pos) return String
   is begin
      return Convert_Binary_To_Hex (
        Convert_Uint_To_Binary (Value, Bit_Width));
   end Convert_Uint_To_Hex;

   function Convert_Ureal_To_Hex_IEEE
     (Value : Ureal;
      Fraction_Bits : Positive := 23;
      Exponent_Bits : Positive := 8;
      Exponent_Bias : Positive := 127) return String
   is begin
      return Convert_Binary_To_Hex (
       Convert_Ureal_To_Binary_IEEE
         (Value,
          Fraction_Bits,
          Exponent_Bits,
          Exponent_Bias));
   end Convert_Ureal_To_Hex_IEEE;

   function Strip_Leading_Zeroes (Str : String) return String is
   begin
      for Ix in Str'Range loop
         if Str (Ix) /= '0' then
            return Str (Ix .. Str'Last);
         end if;
      end loop;
      return "0";
   end Strip_Leading_Zeroes;
end Binary_To_Hex;
