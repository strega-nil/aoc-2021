with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aoc_Utils; use Aoc_Utils;

procedure Day_N (Input : Input_Type.Vector) is
   package IO renames Ada.Text_IO;

   package Data_Type is new Vectors (
      Index_Type => Positive,
      Element_Type => ELEMENT);

   procedure Day_Na (Data : Data_Type.Vector) is
   begin
      IO.Put_Line ("Day Na: ");
   end Day_Na;

   procedure Day_Nb (Data : Data_Type.Vector) is
   begin
      IO.Put_Line ("Day Nb: ");
   end Day_Nb;

   function Parse_Line (Line : String) return ELEMENT is
   begin
   end Parse_Line;

   Data : Data_Type.Vector := Data_Type.Empty_Vector;
begin
   for S of Input loop
      Data.Append (Parse_Line (To_String (S)));
   end loop;

   Day_Na (Data);
   Day_Nb (Data);
end Day_N;
