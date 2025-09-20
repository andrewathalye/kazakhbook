pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Toolkit.Log is

   type Enabler is array (Log_Category) of Boolean;
   Enabled : Enabler := (others => False);

   -------------
   -- Put_Log --
   -------------
   procedure Put_Log (Category : Log_Category; Message : String) is
   begin
      if Enabled (Category) then
         Put_Line (Category'Image & " — " & Message);
      end if;
   end Put_Log;

   ------------
   -- Enable --
   ------------
   procedure Enable (Category : Log_Category) is
   begin
      Enabled (Category) := True;
   end Enable;
end Toolkit.Log;
