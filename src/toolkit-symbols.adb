pragma Ada_2012;

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Toolkit.Symbols is
   -------------
   -- Resolve --
   -------------
   function Resolve
     (PDB : Phonemes.Phoneme_Database; ASL : Abstract_Symbol_List;
      External_Symbol_Context, External_Phoneme_Context : Contexts.Context)
      return Symbol_List
   is
      function Derive_Symbol_Context is new Contexts.Derive_Context
        (Index    => Positive, Element => Abstract_Symbol,
         Lists    => Abstract_Symbol_Lists,
         Features => Symbols_Impl.Dump_Features);

      function Derive_Phoneme_Context is new Contexts.Derive_Context
        (Index => Positive, Element => Symbol_Instance, Lists => Symbol_Lists,
         Features => Dump_Phoneme_Features);

      Result : Symbol_List;
   begin
      --  Evaluate all symbols with only basic phoneme context
      for AS in ASL.Iterate loop
         begin
            Result.Append
              (Resolve
                 (PDB, Abstract_Symbol_Lists.Element (AS),
                  Derive_Symbol_Context (AS, External_Symbol_Context),
                  Derive_Phoneme_Context (AS, External_Phoneme_Context)));
         exception
            when Phonemes.Indeterminate_Phoneme =>
               Result.Append (Symbols_Impl.Null_Symbol);
         end;
      end loop;

      --  Re-evaluate all null symbols
      for S in Result.Iterate loop
         if Result (S) = Symbols_Impl.Null_Symbol then
            Result (S) :=
              Resolve
                (PDB, ASL (Symbol_Lists.To_Index (S)),
                 Derive_Symbol_Context (S, External_Symbol_Context),
                 Derive_Phoneme_Context (S, External_Phoneme_Context));
         end if;
      end loop;

      --  Re-evaluate all symbols left to right
      for S in Result.Iterate loop
         Result (S) :=
           Resolve
             (PDB, ASL (Symbol_Lists.To_Index (S)),
              Derive_Symbol_Context (S, External_Symbol_Context),
              Derive_Phoneme_Context (S, External_Phoneme_Context));
      end loop;

      return Result;
   end Resolve;

   ------------
   -- To_Ada --
   ------------
   function To_Ada
     (SDB : Symbol_Database; Text : String) return Abstract_Symbol_List
   is
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

      TU32              : constant Wide_Wide_String := Decode (Text);
      Index             : Natural                   := TU32'First;
      Max_Symbol_Length : constant                  := 4;
      --  A symbol can be a maximum of four Unicode characters
      --  TODO
      Symbol_Length : Natural;

      Result : Abstract_Symbol_List;
   begin
      while Index <= TU32'Last loop
         --  Determine readahead
         Symbol_Length :=
           (if TU32'Last - Index < Max_Symbol_Length then TU32'Last - Index
            else Max_Symbol_Length);

         --  Search for a match
         for SL in reverse 0 .. Symbol_Length loop
            begin
               Result.Append
                 (Abstract_Symbol'
                    (To_Ada (SDB, Encode (TU32 (Index .. Index + SL)))));
               Index := Index + SL + 1;
               goto Next;
            exception
               when Unknown_Symbol =>
                  null;
            end;
         end loop;

         --  Abort if no match
         raise Unknown_Symbol
           with Encode (TU32 (Index .. Index + Symbol_Length));

         <<Next>>
      end loop;

      return Result;
   end To_Ada;

   ----------------
   -- To_Unicode --
   ----------------
   function To_Unicode (S : Symbol_List) return String is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
   begin
      for Sym of S loop
         Append (Buffer, To_Unicode (Sym));
      end loop;

      return To_String (Buffer);
   end To_Unicode;

   -----------------
   -- To_Phonemes --
   -----------------
   function To_Phonemes (SL : Symbol_List) return Phonemes.Phoneme_List is
      PL : Phonemes.Phoneme_List;
   begin
      for Sym of SL loop
         PL.Append_Vector (To_Phonemes (Sym));
      end loop;

      return PL;
   end To_Phonemes;
end Toolkit.Symbols;
