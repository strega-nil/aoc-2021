with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aoc_Utils; use Aoc_Utils;

procedure Day_3 (Input : Input_Type.Vector) is
   package IO renames Ada.Text_IO;

   package Data_Type renames Input_Type;

   procedure Day_3a (Data : Data_Type.Vector) is
      Count : constant Natural := Length (Data (1));
      Gamma : Natural := 0;
      Epsilon : Natural := 0;
   begin
      for I in 1 .. Count loop
         declare
            Count_1s : Natural := 0;
            Count_0s : Natural := 0;
         begin
            for Str of Data loop
               if Element (Str, I) = '1' then
                  Count_1s := Count_1s + 1;
               elsif Element (Str, I) = '0' then
                  Count_0s := Count_0s + 1;
               else
                  Assert (False, "Invalid character: " & Element (Str, I));
               end if;
            end loop;

            if Count_1s > Count_0s then
               Gamma := Gamma + (2 ** (Count - I));
            else
               Epsilon := Epsilon + (2 ** (Count - I));
            end if;
         end;
      end loop;

      IO.Put_Line (
         "Day 3a: gamma = "
         & Gamma'Image
         & ", eps = "
         & Epsilon'Image
         & ", prod = "
         & Natural'Image (Gamma * Epsilon));
   end Day_3a;

   procedure Day_3b (Data : Data_Type.Vector) is
      function Updated_Data (
         V : Data_Type.Vector;
         Index : Positive;
         More_1s_Bit : Character;
         More_0s_Bit : Character
      ) return Data_Type.Vector is
         Count_1s : Natural := 0;
         Count_0s : Natural := 0;
         Bit : Character;
         Result : Data_Type.Vector := Data_Type.Empty_Vector;
      begin
         if V.Length = 1 then
            return V;
         end if;

         for Str of V loop
            if Element (Str, Index) = '1' then
               Count_1s := Count_1s + 1;
            elsif Element (Str, Index) = '0' then
               Count_0s := Count_0s + 1;
            else
               Assert (False, "Invalid character: " & Element (Str, Index));
            end if;
         end loop;

         Bit := (if Count_0s > Count_1s then More_0s_Bit else More_1s_Bit);

         for Value of V loop
            if Element (Value, Index) = Bit then
               Result.Append (Value);
            end if;
         end loop;

         return Result;
      end Updated_Data;

      Count : constant Natural := Length (Data (1));
      O2_Gen_Data : Data_Type.Vector := Data;
      CO2_Scrub_Data : Data_Type.Vector := Data;
      O2_Gen_Rating : Natural := 0;
      CO2_Scrub_Rating : Natural := 0;
   begin
      for I in 1 .. Count loop
         O2_Gen_Data := Updated_Data (O2_Gen_Data, I, '1', '0');
         CO2_Scrub_Data := Updated_Data (CO2_Scrub_Data, I, '0', '1');
      end loop;

      Assert (O2_Gen_Data.Length = 1,
         "O2_Gen_Data.Length[" & O2_Gen_Data.Length'Image & "] = 1");
      Assert (CO2_Scrub_Data.Length = 1,
         "CO2_Scrub_Data.Length[" & CO2_Scrub_Data.Length'Image & "] = 1");

      for I in 1 .. Count loop
         declare
            O2_Data_Str : constant Unbounded_String := O2_Gen_Data (1);
            CO2_Data_Str : constant Unbounded_String := CO2_Scrub_Data (1);
         begin
            if Element (O2_Data_Str, I) = '1' then
               O2_Gen_Rating := O2_Gen_Rating + (2 ** (Count - I));
            end if;
            if Element (CO2_Data_Str, I) = '1' then
               CO2_Scrub_Rating := CO2_Scrub_Rating + (2 ** (Count - I));
            end if;
         end;
      end loop;

      IO.Put_Line (
         "Day 3b: o2 = "
         & O2_Gen_Rating'Image
         & ", co2 = "
         & CO2_Scrub_Rating'Image
         & ", prod = "
         & Natural'Image (O2_Gen_Rating * CO2_Scrub_Rating));
   end Day_3b;

   Data : constant Data_Type.Vector := Input;
begin
   Day_3a (Data);
   Day_3b (Data);
end Day_3;
