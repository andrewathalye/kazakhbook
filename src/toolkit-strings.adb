pragma Ada_2012;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Toolkit.Strings is
   function Split
     (Input : String; Delimiter : String := " ") return Argument_List
   is
      use Ada.Strings.Fixed;

      Start_Index : Positive := Input'First;
      Delim_Index : Natural;
      Result      : Argument_List;
   begin
      Delim_Index := Index (Input, Delimiter, Start_Index);

      if Delim_Index = 0 then
         Result.Append (Input);
      end if;

      while Delim_Index /= 0 loop
         Result.Append (Input (Start_Index .. Delim_Index - 1));

         Start_Index := Delim_Index + 1;
         Delim_Index := Index (Input, Delimiter, Start_Index);

         --  If there are no more delimiters but there is still text
         if Delim_Index = 0 and Start_Index <= Input'Last then
            Result.Append (Input (Start_Index .. Input'Last));
         end if;
      end loop;

      return Result;
   end Split;

   function Join
     (List : Argument_List; First, Last : Positive; Delimiter : String := " ")
      return String
   is
      use Ada.Strings.Unbounded;
      Buffer : Unbounded_String;
   begin
      for I in First .. Last loop
         Append (Buffer, List (I));
         Append (Buffer, Delimiter);
      end loop;

      return To_String (Buffer);
   end Join;

end Toolkit.Strings;
