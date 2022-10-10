with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aoc_Utils; use Aoc_Utils;

procedure Day_1 (Input : Input_Type.Vector) is
   package IO renames Ada.Text_IO;

   package Data_Type is new Vectors (
      Index_Type => Positive,
      Element_Type => Natural);

   procedure Day_1a (Data : Data_Type.Vector) is
      Last : Natural;
      Count : Natural := 0;
   begin
      Assert (Data.Length >= 2, "Data.Length >= 2");
      Last := Data.First_Element;
      for I in Data.First_Index + 1 .. Data.Last_Index loop
         declare
            Element : constant Integer := Data (I);
         begin
            if Element > Last then
               Count := Count + 1;
            end if;
            Last := Element;
         end;
      end loop;

      IO.Put_Line ("Day 1a: " & Natural'Image (Count));
   end Day_1a;

   procedure Day_1b (Data : Data_Type.Vector) is

      type Window is array (1 .. 3) of Integer;

      function Sum (Arr : Window) return Integer is
      begin
         return Arr (1) + Arr (2) + Arr (3);
      end Sum;
      procedure Shift (
         Arr : in out Window;
         Element : Integer) is
      begin
         Arr (1) := Arr (2);
         Arr (2) := Arr (3);
         Arr (3) := Element;
      end Shift;

      Last_Three : Window;
      Current_Three : Window;
      Count : Natural := 0;
   begin
      Assert (Data.Length >= 3, "Data.Length >= 3");
      Last_Three := (Data (1), Data (2), Data (3));
      Current_Three := Last_Three;

      for I in 4 .. Data.Last_Index loop
         Shift (Current_Three, Data (I));
         if Sum (Current_Three) > Sum (Last_Three) then
            Count := Count + 1;
         end if;
         Last_Three := Current_Three;
      end loop;

      IO.Put_Line ("Day 1b: " & Natural'Image (Count));
   end Day_1b;

   Data : Data_Type.Vector := Data_Type.Empty_Vector;
begin
   for S of Input loop
      Data.Append (Integer'Value (To_String (S)));
   end loop;

   Day_1a (Data);
   Day_1b (Data);
end Day_1;
