pragma Ada_2012;

with Ada.Strings.Hash;

with Toolkit.Strings;

package body Toolkit.Phonemes_Impl is
   ------------------
   -- Get_Features --
   ------------------
   function Get_Features (PI : Phoneme_Instance) return Features.Feature_Set is
   begin
      return Features.Flatten (Phone_Lists.Element (PI.Instance).Sounds);
   end Get_Features;

   ------------------
   -- Get_Features --
   ------------------
   function Get_Features (AP : Abstract_Phoneme) return Features.Feature_Set is
   begin
      return AP.Features;
   end Get_Features;

   -------------
   -- Resolve --
   -------------
   function Resolve
     (PDB : Phoneme_Database; AP : Abstract_Phoneme;
      Cur : Contexts.Cursor'Class) return Phoneme_Instance
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Resolve unimplemented");
      return raise Program_Error with "Unimplemented function Resolve";
   end Resolve;

   -----------------
   -- Abstractise --
   -----------------
   function Abstractise (Instance : Phoneme_Instance) return Abstract_Phoneme
   is
   begin
      return
        (Phoneme => Instance.Phoneme, Features => Get_Features (Instance));
   end Abstractise;

   ---------
   -- Add --
   ---------
   procedure Add (AP : in out Abstract_Phoneme; FS : Features.Feature_Set) is
   begin
      AP.Features := Features.Add (AP.Features, FS);
   end Add;

   --------------
   -- Subtract --
   --------------
   procedure Subtract (AP : in out Abstract_Phoneme; FS : Features.Feature_Set)
   is
   begin
      AP.Features := Features.Subtract (AP.Features, FS);
   end Subtract;

   ------------
   -- To_XML --
   ------------
   function To_XML (Instance : Phoneme_Instance) return String is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
   begin
      Append (Buffer, "<phone>");

      Append
        (Buffer,
         Features.To_XML (Phone_Lists.Element (Instance.Instance).Sounds));

      Append (Buffer, "<ipa>");
      Append (Buffer, Phone_Lists.Element (Instance.Instance).IPA);
      Append (Buffer, "</ipa>");

      Append (Buffer, "</phone>");

      return To_String (Buffer);
   end To_XML;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database; Text : String)
      return Abstract_Phoneme
   is
      use type Phoneme_Maps.Cursor;

      Required_Set     : Features.Feature_Set;
      Required_Phoneme : Phoneme_Maps.Cursor;

      Strings : constant Toolkit.Strings.Argument_List :=
        Toolkit.Strings.Split (Text);
   begin
      --  Process each requested feature or phoneme
      for S of Strings loop
         --  Handle phoneme references
         if S (S'First) = '@' then
            if S'Length = 1 or Required_Phoneme /= Phoneme_Maps.No_Element then
               raise Constraint_Error;
            end if;

            declare
               PN : constant Phoneme_Name :=
                 Phoneme_Name (S (S'First + 1 .. S'Last));
            begin
               if not PDB.Contains (PN) then
                  raise Unknown_Phoneme with String (PN);
               end if;

               Required_Phoneme := PDB.Find (PN);
            end;
         else
            Required_Set.Append (Features.To_Ada (FDB, S));
         end if;
      end loop;

      return (Required_Phoneme, Required_Set);
   end To_Ada;

   ----------
   -- Read --
   ----------

   procedure Read
     (Doc : DOM.Core.Document; FDB : Features.Feature_Database;
      CDB : Contexts.Context_Database; PDB : out Phoneme_Database)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe (P : Phoneme_Instance) return String is
   begin
      return
        Ada.Strings.Unbounded.To_String (Phone_Lists.Element (P.Instance).IPA);
   end Transcribe;

   ----------
   -- Hash --
   ----------
   function Hash (L : Phoneme_Name) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (L));
   end Hash;

end Toolkit.Phonemes_Impl;
