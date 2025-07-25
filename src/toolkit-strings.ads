pragma Ada_2012;

with Ada.Containers.Indefinite_Vectors;

package Toolkit.Strings is
   package Argument_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   subtype Argument_List is Argument_Lists.Vector;

   function Split
     (Input : String; Delimiter : String := " ") return Argument_List;
end Toolkit.Strings;
