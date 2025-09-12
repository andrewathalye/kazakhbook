pragma Ada_2012;

pragma Warnings (Off);
package body Toolkit.Contexts is

   ------------
   -- To_Ada --
   ------------

   function To_Ada (DB : Context_Database; XML : DOM.Core.Node) return Context
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Ada unimplemented");
      return raise Program_Error with "Unimplemented function To_Ada";
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (DB : Context_Database; XML : DOM.Core.Node_List) return Context_List
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Ada unimplemented");
      return raise Program_Error with "Unimplemented function To_Ada";
   end To_Ada;

end Toolkit.Contexts;
