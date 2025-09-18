pragma Ada_2012;

with Ada.Strings.Unbounded;

with DOM.Core.Documents;
with DOM.Core.Nodes;
with Toolkit.Contexts_Impl;

package body Toolkit.Syllables is
   function Get_Features (S : Syllable) return Features.Feature_Set is
     (S.Features);

   function Get_Sub
     (LE : Syllable; Placement : Contexts.Cursor_Placement)
      return Contexts.Cursor'Class;
   function Get_Sub
     (LE : Syllable; Placement : Contexts.Cursor_Placement)
      return Contexts.Cursor'Class
   is
      use all type Contexts.Cursor_Placement;
   begin
      case Placement is
         when First =>
            return Phonemes.To_Cursor (LE.Sounds.First);
         when Last =>
            return Phonemes.To_Cursor (LE.Sounds.Last);
      end case;
   end Get_Sub;

   ---------------
   -- To_Cursor --
   ---------------
   package Syllable_Cursors is new Toolkit.Contexts.Generic_Cursors
     (Cursor_Scope => Toolkit.Contexts_Impl.Syllable, List => Syllable_Lists,
      Get_Features => Get_Features, Get_Sub => Get_Sub);

   function To_Cursor
     (SLC : Syllable_Lists.Cursor) return Contexts.Cursor'Class
   is
   begin
      return Syllable_Cursors.Create (SLC);
   end To_Cursor;

   ---------------
   -- Syllabify --
   ---------------
   function Syllabify
     (SDB : Syllable_Database; Phonetic_Word : Phonemes.Phoneme_List)
      return Syllable_List
   is
      use type Phonemes.Phoneme_Lists.Cursor;

      function Largest_Syllable
        (Cur : in out Phonemes.Phoneme_Lists.Cursor) return Syllable;
      function Largest_Syllable
        (Cur : in out Phonemes.Phoneme_Lists.Cursor) return Syllable
      is
         use Phonemes.Phoneme_Lists;
         Working_Cur : Cursor := Cur;
         Candidates  : Syllable_List;
         S           : Syllable;
      begin
         Check_Syllable_Definitions :
         for SD of SDB loop
            S.Sounds.Clear;
            Check_Elements :
            for SE of SD loop
               if not Has_Element (Working_Cur) then
                  goto Failed_Match;
               end if;

               case SE.Kind is
                  when Exclude =>
                     declare
                        WC_Features : constant Features.Feature_Set :=
                          Phonemes.To_Cursor (Working_Cur).Features;
                     begin
                        for FI of SE.FS loop
                           if WC_Features.Contains (FI) then
                              goto Failed_Match;
                           end if;
                        end loop;
                     end;
                  when Require =>
                     if not Features.Superset
                         (Phonemes.To_Cursor (Working_Cur).Features, SE.FS)
                     then
                        goto Failed_Match;
                     end if;
               end case;
               S.Sounds.Append (Element (Working_Cur));
               Working_Cur := Next (Working_Cur);
            end loop Check_Elements;
            Candidates.Append (S);
            <<Failed_Match>>
            Working_Cur := Cur;
         end loop Check_Syllable_Definitions;

         --  Error Out if Candidates Empty
         if Candidates.Is_Empty then
            declare
               Error_Portion : Phonemes.Phoneme_List;
            begin
               for I in To_Index (Cur) .. Phonetic_Word.Last_Index loop
                  Error_Portion.Append (Phonetic_Word (I));
               end loop;
               raise Syllable_Error with Phonemes.Transcribe (Error_Portion);
            end;
         end if;

         ------------------------------
         -- Sieve Through Candidates --
         ------------------------------
         declare
            use type Ada.Containers.Count_Type;
            Longest_Candidate : Syllable;
         begin
            for Candidate of Candidates loop
               if Candidate.Sounds.Length > Longest_Candidate.Sounds.Length
               then
                  Longest_Candidate := Candidate;
               end if;
            end loop;

            for I in 1 .. Longest_Candidate.Sounds.Length loop
               Cur := Next (Cur);
            end loop;

            return Longest_Candidate;
         end;
      end Largest_Syllable;

      Result : Syllable_List;
      Cur    : Phonemes.Phoneme_Lists.Cursor := Phonetic_Word.First;
   begin
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
      SDB : out Syllable_Database)
   is
      use type DOM.Core.Node_Types;

      X_Syllables, X_Elements : DOM.Core.Node_List;
      X_Syllable, X_Element   : DOM.Core.Node;
      SE                      : Syllable_Element;
      SD                      : Syllable_Definition;
   begin
      SDB.Clear;

      X_Syllables :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name (Doc, "syllable");

      for X_Syllables_Index in 1 .. DOM.Core.Nodes.Length (X_Syllables) loop
         X_Syllable :=
           DOM.Core.Nodes.Item (X_Syllables, X_Syllables_Index - 1);
         X_Elements := DOM.Core.Nodes.Child_Nodes (X_Syllable);

         SD.Clear;
         for X_Elements_Index in 1 .. DOM.Core.Nodes.Length (X_Elements) loop
            X_Element :=
              DOM.Core.Nodes.Item (X_Elements, X_Elements_Index - 1);
            if DOM.Core.Nodes.Node_Type (X_Element) = DOM.Core.Element_Node
            then
               if DOM.Core.Nodes.Node_Name (X_Element) = "require" then
                  SE := (Require, Features.To_Ada (FDB, X_Element));
               elsif DOM.Core.Nodes.Node_Name (X_Element) = "exclude" then
                  SE := (Exclude, Features.To_Ada (FDB, X_Element));
               end if;
               SD.Append (SE);
            end if;
         end loop;
         SDB.Append (SD);

         DOM.Core.Free (X_Elements);
      end loop;

      DOM.Core.Free (X_Syllables);
   end Read;

end Toolkit.Syllables;
