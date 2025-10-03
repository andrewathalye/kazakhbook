pragma Ada_2012;

with Ada.Strings.Unbounded;

with DOM.Core.Documents;
with DOM.Core.Nodes;

with Toolkit.Contexts_Impl;
with Toolkit.Log; use Toolkit.Log;

package body Toolkit.Syllables is
   function Get_Features (S : Syllable) return Features.Feature_Set is
     (S.Features);

   ---------------
   -- To_Cursor --
   ---------------
   function Get_Child (LE : Syllable) return Contexts.Cursor'Class is
     (Phonemes.To_Cursor (LE.Sounds.First));

   package Syllable_Cursors is new Toolkit.Contexts.Generic_Cursors
     (Cursor_Scope => Toolkit.Contexts_Impl.Syllable, List => Syllable_Lists,
      Get_Features => Get_Features, Get_Child => Get_Child);

   function To_Cursor
     (SLC : Syllable_Lists.Cursor) return Contexts.Cursor'Class is
     (Syllable_Cursors.Create (SLC));

   ---------------
   -- Syllabify --
   ---------------
   function Syllabify
     (SDB : Syllable_Database; Phonetic_Word : Phonemes.Phoneme_List)
      return Syllable_List
   is
      use type Phonemes.Phoneme_Lists.Cursor;
      use type Ada.Containers.Count_Type;

      function Largest_Syllable
        (Initial_Cur : in out Phonemes.Phoneme_Lists.Cursor) return Syllable;
      function Largest_Syllable
        (Initial_Cur : in out Phonemes.Phoneme_Lists.Cursor) return Syllable
      is
         use Phonemes.Phoneme_Lists;
         use Syllable_Definition_Maps;
         Prev_Working_Cur : Phonemes.Phoneme_Lists.Cursor;
         Working_Cur      : Phonemes.Phoneme_Lists.Cursor := Initial_Cur;
         Temp, Result     : Syllable;
         Max_Length       : Ada.Containers.Count_Type     := 0;
      begin
         Check_Syllable_Definitions :
         for SD_Cursor in SDB.Iterate loop
            Put_Log (Log.Syllables, "TYPE: " & Key (SD_Cursor));

            --  Only consider candidates longer than the existing one
            if Element (SD_Cursor).Elements.Length <= Max_Length then
               Put_Log (Log.Syllables, "SKIP BY LENGTH");
               goto Failed_Match;
            end if;

            Temp.Sounds.Clear;
            Temp.Features := Element (SD_Cursor).Provides;
            Check_Elements :
            for SE of Element (SD_Cursor).Elements loop
               if not Has_Element (Working_Cur) then
                  Put_Log (Log.Syllables, "FAIL: INSUFFICIENT");
                  goto Failed_Match;
               end if;

               Put_Log
                 (Log.Syllables,
                  "PHONE: " & Phonemes.Transcribe (Element (Working_Cur)));

               Put_Log
                 (Log.Syllables,
                  "PROVIDE: " &
                  Features.To_String
                    (Phonemes.Get_Features (Element (Working_Cur))));
               case SE.Kind is
                  when Forbid =>
                     Put_Log
                       (Log.Syllables,
                        "FORBID: " & Features.To_String (SE.FS));
                     declare
                        WC_Features : constant Features.Feature_Set :=
                          Phonemes.Get_Features (Element (Working_Cur));
                     begin
                        for FI of SE.FS loop
                           if WC_Features.Contains (FI) then
                              goto Failed_Match;
                           end if;
                        end loop;
                     end;
                  when Require =>
                     Put_Log
                       (Log.Syllables,
                        "REQUIRE: " & Features.To_String (SE.FS));
                     if not Features.Superset
                         (Phonemes.Get_Features (Element (Working_Cur)), SE.FS)
                     then
                        goto Failed_Match;
                     end if;
               end case;
               Temp.Sounds.Append (Element (Working_Cur));
               Prev_Working_Cur := Working_Cur;
               Working_Cur      := Next (Working_Cur);
               Put_Log (Log.Syllables, "PASS");
            end loop Check_Elements;

            --  Check contexts
            if not Contexts.Applicable
                (Phonemes.To_Cursor (Initial_Cur),
                 Element (SD_Cursor).Initial_Context) or
              not Contexts.Applicable
                (Phonemes.To_Cursor (Prev_Working_Cur),
                 Element (SD_Cursor).Final_Context)
            then
               Put_Log (Log.Syllables, "CONTEXT: FAILED");
               goto Failed_Match;
            end if;

            --  Set Result and Length
            Put_Log (Log.Syllables, "CONTEXT PASSED");
            Result     := Temp;
            Max_Length := Result.Sounds.Length;

            <<Failed_Match>>
            Working_Cur := Initial_Cur;
            Put_Log (Log.Syllables, "NEXT");
         end loop Check_Syllable_Definitions;

         --  Error Out if Candidates Empty
         if Result.Sounds.Is_Empty then
            declare
               Error_Portion : Phonemes.Phoneme_List;
            begin
               for I in To_Index (Initial_Cur) .. Phonetic_Word.Last_Index loop
                  Error_Portion.Append (Phonetic_Word (I));
               end loop;
               raise Syllable_Error with Phonemes.Transcribe (Error_Portion);
            end;
         end if;

         Put_Log
           (Log.Syllables, "RESULT: " & Phonemes.Transcribe (Result.Sounds));

         --  Advance initial cursor so that the next lookup
         --  starts at the end of this syllable.
         for I in 1 .. Result.Sounds.Length loop
            Initial_Cur := Next (Initial_Cur);
         end loop;

         return Result;
      end Largest_Syllable;

      Result : Syllable_List;
      Cur    : Phonemes.Phoneme_Lists.Cursor := Phonetic_Word.First;
   begin
      Put_Log
        (Log.Syllables, "SYLLABIFY: " & Phonemes.Transcribe (Phonetic_Word));
      while Cur /= Phonemes.Phoneme_Lists.No_Element loop
         Result.Append (Largest_Syllable (Cur));
      end loop;

      return Result;
   end Syllabify;

   -------------
   -- Flatten --
   -------------
   function Flatten (SL : Syllable_List) return Phonemes.Phoneme_List is
      Result : Phonemes.Phoneme_List;
   begin
      for S of SL loop
         Result.Append_Vector (S.Sounds);
      end loop;

      return Result;
   end Flatten;

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe (S : Syllable) return String is
   begin
      return Phonemes.Transcribe (S.Sounds);
   end Transcribe;

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe (SL : Syllable_List) return String is
      use Ada.Strings.Unbounded;
      use type Syllable_Lists.Cursor;
      Buffer : Unbounded_String;
   begin
      for SC in SL.Iterate loop
         Append
           (Buffer, Phonemes.Transcribe (Syllable_Lists.Element (SC).Sounds));

         if SC /= Syllable_Lists.Last (SL) then
            Append (Buffer, ".");
         end if;
      end loop;

      return To_String (Buffer);
   end Transcribe;

   ----------
   -- Read --
   ----------
   procedure Read
     (Doc :     DOM.Core.Document; FDB : Features.Feature_Database;
      CDB :     Contexts.Context_Database; PDB : Phonemes.Phoneme_Database;
      SDB : out Syllable_Database)
   is
      use type DOM.Core.Node_Types;

      X_Syllables, X_Syllable_Children            : DOM.Core.Node_List;
      X_Syllable, X_Syllable_ID, X_Syllable_Child : DOM.Core.Node;
      SD                                          : Syllable_Definition;
      SE                                          : Syllable_Element;
   begin
      SDB.Clear;

      X_Syllables :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "syllable");

      Read_Syllables :
      for X_Syllables_Index in 1 .. DOM.Core.Nodes.Length (X_Syllables) loop
         X_Syllable          :=
           DOM.Core.Nodes.Item (X_Syllables, X_Syllables_Index - 1);
         X_Syllable_ID       :=
           DOM.Core.Nodes.Get_Named_Item
             (DOM.Core.Nodes.Attributes (X_Syllable), "id");
         X_Syllable_Children := DOM.Core.Nodes.Child_Nodes (X_Syllable);

         ------------------------------------
         --  Read Ada Syllable Definition  --
         ------------------------------------
         SD := (others => <>);

         Read_Syllable_Components :
         for X_Elements_Index in
           1 .. DOM.Core.Nodes.Length (X_Syllable_Children)
         loop
            X_Syllable_Child :=
              DOM.Core.Nodes.Item (X_Syllable_Children, X_Elements_Index - 1);
            if DOM.Core.Nodes.Node_Type (X_Syllable_Child) =
              DOM.Core.Element_Node
            then
               --  Supports phonemes
               if DOM.Core.Nodes.Node_Name (X_Syllable_Child) = "require" then
                  SE :=
                    (Require,
                     Phonemes.Get_Features
                       (Phonemes.To_Ada (FDB, PDB, X_Syllable_Child)));

                  SD.Elements.Append (SE);
               elsif DOM.Core.Nodes.Node_Name (X_Syllable_Child) = "forbid"
                  --  Supports phonemes

               then
                  SE :=
                    (Forbid,
                     Phonemes.Get_Features
                       (Phonemes.To_Ada (FDB, PDB, X_Syllable_Child)));
                  SD.Elements.Append (SE);
               elsif DOM.Core.Nodes.Node_Name (X_Syllable_Child) = "provide"
                  --  Only supports features

               then
                  SD.Provides := Features.To_Ada (FDB, X_Syllable_Child);
               elsif DOM.Core.Nodes.Node_Name (X_Syllable_Child) =
                 "initial_context"
               then
                  SD.Initial_Context :=
                    Contexts.To_Ada (CDB, X_Syllable_Child);
               elsif DOM.Core.Nodes.Node_Name (X_Syllable_Child) =
                 "final_context"
               then
                  SD.Final_Context := Contexts.To_Ada (CDB, X_Syllable_Child);
               end if;
            end if;
         end loop Read_Syllable_Components;
         SDB.Insert (DOM.Core.Nodes.Node_Value (X_Syllable_ID), SD);

         DOM.Core.Free (X_Syllable_Children);
      end loop Read_Syllables;

      DOM.Core.Free (X_Syllables);
   end Read;

end Toolkit.Syllables;
