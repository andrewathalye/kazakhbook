pragma Ada_2012;
with Ada.Strings.Hash;
package body Toolkit.Phonemes is

   pragma Warnings (Off);
   function Describe
     (Sounds : Toolkit.Contexts.Feature_Set_List) return String is
     ("unimplemented");
   function Describe (Context : Toolkit.Contexts.Context) return String is
     ("unimplemented");
   --  TODO implement
   pragma Warnings (On);

   -------------
   -- Resolve --
   -------------
   function Resolve
     (DB      : Phoneme_Database; Sounds : Toolkit.Contexts.Feature_Set_List;
      Context : Toolkit.Contexts.Context; Within : String := "")
      return Phoneme_Instance
   is
      function Search
        (Phoneme_C : Phoneme_Maps.Cursor) return Phoneme_Instance;
      function Search (Phoneme_C : Phoneme_Maps.Cursor) return Phoneme_Instance
      is
      begin
         Check_Phones :
         for Phone_C in Phoneme_Maps.Element (Phoneme_C).Iterate loop
            declare
               use type Ada.Containers.Count_Type;
               use Toolkit.Contexts;
               L_Phone : Phone renames Phone_Lists.Element (Phone_C);
            begin
               Check_Contexts :
               for L_Context of L_Phone.Contexts loop
                  if Superset (Context, L_Context) then
                     goto Check_Sounds;
                  end if;
               end loop Check_Contexts;
               goto Next_Phone;

               <<Check_Sounds>>
               if Sounds.Length = L_Phone.Sounds.Length
                  and then Superset (Sounds, L_Phone.Sounds)
               then
                  return
                    (Phoneme => Phoneme_C,
                     Instance => Phone_C,
                     Extra   => Subtract (Sounds, L_Phone.Sounds));
               end if;

               <<Next_Phone>>
            end;
         end loop Check_Phones;
         raise Unknown_Phoneme with Describe (Sounds) & Describe (Context);
      end Search;
   begin
      --  Check within a specific phoneme
      if Within'Length /= 0 then
         if DB.Contains (Phoneme_Name (Within)) then
            return Search (DB.Find (Phoneme_Name (Within)));
         end if;

         raise Unknown_Phoneme with Within;
      end if;

      --  Otherwise try all of them
      for Phoneme_C in DB.Iterate loop
         begin
            return Search (Phoneme_C);
         exception
            when Unknown_Phoneme =>
               null;
         end;
      end loop;

      raise Unknown_Phoneme with Describe (Sounds) & Describe (Context);
   end Resolve;

   ------------
   -- To_XML --
   ------------
   function To_XML (Instance : Phoneme_Instance) return String is
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;

      Buffer : Unbounded_String;
   begin
      --  If a phoneme has only one phone, we use @ notation
      if Phoneme_Maps.Element (Instance.Phoneme).Length = 1 then
         Append (Buffer, "@" & String (Phoneme_Maps.Key (Instance.Phoneme)));
      else
         for Feature_Set of Phone_Lists.Element (Instance.Instance).Sounds loop
            for Feature of Feature_Set loop
               Append (Buffer, Features.To_XML (Feature) & " ");
            end loop;
            Append (Buffer, "|| ");
            --  TODO
         end loop;
      end if;

      return To_String (Buffer);
   end To_XML;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (DB : Phoneme_Database; XML : String; Context : Toolkit.Contexts.Context)
      return Phoneme_Instance
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Ada unimplemented");
      return raise Program_Error with "Unimplemented function To_Ada";
   end To_Ada;

   ----------
   -- Read --
   ----------
   procedure Read
     (Doc : DOM.Core.Document; Features : Toolkit.Features.Feature_Database;
      Phonemes : out Phoneme_Database)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   ----------
   -- Hash --
   ----------
   function Hash (L : Phoneme_Name) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (L));
   end Hash;
end Toolkit.Phonemes;
