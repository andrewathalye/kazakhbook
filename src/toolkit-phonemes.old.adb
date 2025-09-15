pragma Ada_2012;

with Ada.Strings.Unbounded;
with DOM.Core.Nodes;

with Toolkit.XML;

package body Toolkit.Phonemes is

   -------------
   -- Resolve --
   -------------
   function Resolve
     (PDB              : Phoneme_Database; List : Abstract_Phoneme_List;
      External_Context : Contexts.Context) return Phoneme_List
   is
      function Derive_Context is new Contexts.Derive_Context
        (Index => Positive,
         Element => Abstract_Phoneme,
         Lists => Abstract_Phoneme_Lists,
         Features => Phonemes_Impl.Dump_Features);

      function Derive_Context is new Contexts.Derive_Context
        (Index => Positive,
         Element => Phoneme_Instance,
         Lists => Phoneme_Lists,
         Features => Phonemes_Impl.Dump_Features);

      Result : Phoneme_List;
   begin
      --  Evaluate all phonemes that can be evaluated
      --  Left-to-Right evaluation
      for AP in List.Iterate loop
         begin
            Result.Append
              (Resolve
                 (PDB, Abstract_Phoneme_Lists.Element (AP),
                  Derive_Context (AP, External_Context)));
         exception
            when Indeterminate_Phoneme =>
               Result.Append (Phonemes_Impl.Null_Phoneme);
         end;
      end loop;

      --  Re-evaluate all null phonemes
      for P in Result.Iterate loop
         if Result (P) = Phonemes_Impl.Null_Phoneme then
            Result (P) :=
              Resolve
                (PDB, List (Phoneme_Lists.To_Index (P)),
                 Derive_Context (P, External_Context));
         end if;
      end loop;

      --  Re-evaluate all phonemes left to right
      for P in Result.Iterate loop
         Result (P) :=
           Resolve
             (PDB, List (Phoneme_Lists.To_Index (P)),
              Derive_Context (P, External_Context));
      end loop;

      return Result;
   end Resolve;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (FDB : Features.Feature_Database; PDB : Phoneme_Database;
      XML : DOM.Core.Node) return Abstract_Phoneme
   is
   begin
      if DOM.Core.Nodes.Node_Name (XML) /= "require" then
         raise Constraint_Error;
      end if;

      return To_Ada (FDB, PDB, Toolkit.XML.Get_Text (XML));
   end To_Ada;

   ----------------
   -- Transcribe --
   ----------------
   function Transcribe (PL : Phoneme_List) return String
   is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
   begin
      for PI of PL loop
         Append (Buffer, Transcribe (PI));
      end loop;

      return To_String (Buffer);
   end Transcribe;
end Toolkit.Phonemes;
